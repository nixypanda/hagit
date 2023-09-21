{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PackfileParsing (
    parsePackfile,
    -- exports for testing
    Instruction (..),
    DeltaContent (..),
    RawUndeltifiedObject (..),
    RawDeltifiedObject (..),
    RawObjectHeader (..),
    ObjectType (..),
    instructionParser,
    rawObjSHA1,
    deltaContentParser,
    deltaHeaderObjSizeParser,
    reconstructDeltaFromBase,
) where

import Control.Arrow ((&&&))
import Control.Monad (unless, when)
import Crypto.Hash (Digest, SHA1, hashlazy)
import Data.Attoparsec.ByteString.Lazy (
    Parser,
    anyWord8,
    count,
    endOfInput,
    many',
    parseOnly,
    take,
 )
import Data.Bits (Bits (popCount), shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Word8 (Word8)
import ParsingUtils (sha1Parser)
import ZlibDecompression (decompressParser)
import Prelude hiding (take)

-- == pack-*.pack files have the following format:
--
--    - A header appears at the beginning and consists of the following:
--
--      4-byte signature:
--          The signature is: {'P', 'A', 'C', 'K'}
--
--      4-byte version number (network byte order):
-- 	 Git currently accepts version number 2 or 3 but
--          generates version 2 only.
--
--      4-byte number of objects contained in the pack (network byte order)
--
--      Observation: we cannot have more than 4G versions ;-) and
--      more than 4G objects in a pack.
--
--    - The header is followed by number of object entries, each of
--      which looks like this:
--
--      (undeltified representation)
--      n-byte type and length (3-bit type, (n-1)*7+4-bit length)
--      compressed data
--
--      (deltified representation)
--      n-byte type and length (3-bit type, (n-1)*7+4-bit length)
--      base object name if OBJ_REF_DELTA or a negative relative
-- 	 offset from the delta object's position in the pack if this
-- 	 is an OBJ_OFS_DELTA object
--      compressed delta data
--
--      Observation: length of each object is encoded in a variable
--      length format and is not constrained to 32-bit or anything.
--
--   - The trailer records a pack checksum of all of the above.

data PackfileWithDeltas = PackfileWithDeltas
    { packfileHeader :: PackfileHeader
    , packfileObjects :: [RawObject]
    , receivedChecksum :: Digest SHA1
    }
    deriving (Show)

data PackfileHeader = PackfileHeader
    { magicString :: BL.ByteString
    , packfileVersion :: Int
    , objectsInPackfile :: Int
    }
    deriving (Show)

data RawObject
    = RawUndeltified RawUndeltifiedObject
    | RawDeltified RawDeltifiedObject
    deriving (Show)

getDeltified :: RawObject -> Maybe RawDeltifiedObject
getDeltified (RawDeltified x) = Just x
getDeltified _ = Nothing

getUndeltified :: RawObject -> Maybe RawUndeltifiedObject
getUndeltified (RawUndeltified x) = Just x
getUndeltified _ = Nothing

data RawUndeltifiedObject = RawUndeltifiedObject
    { objHeader :: RawObjectHeader
    , objData :: BL.ByteString
    }
    deriving (Show, Eq)

rawObjSHA1 :: RawUndeltifiedObject -> Digest SHA1
rawObjSHA1 = hashlazy . objContent

objContent :: RawUndeltifiedObject -> BL.ByteString
objContent RawUndeltifiedObject{..} = objHeaderRepr objHeader <> "\0" <> objData

data RawDeltifiedObject = RawDeltifiedObject
    { deltaObjHeader :: RawObjectHeader
    , deltaObjData :: BL.ByteString
    , parentSha1 :: Digest SHA1
    }
    deriving (Show)

mkDeltifiedObj :: RawObjectHeader -> Digest SHA1 -> BL.ByteString -> RawObject
mkDeltifiedObj objHeader parentSha1 deltaObjData =
    RawDeltified $ RawDeltifiedObject objHeader deltaObjData parentSha1

mkUndeltifiedObj :: RawObjectHeader -> BL.ByteString -> RawObject
mkUndeltifiedObj objHeader objData =
    RawUndeltified $ RawUndeltifiedObject objHeader objData

data RawObjectHeader = RawObjectHeader
    { objType :: ObjectType
    , objSize :: Int
    }
    deriving (Show, Eq)

objHeaderRepr :: RawObjectHeader -> BL.ByteString
objHeaderRepr RawObjectHeader{..} = BLC.pack $ objTypeRepr objType <> " " <> show objSize

data ObjectType
    = OBJ_COMMIT
    | OBJ_TREE
    | OBJ_BLOB
    | OBJ_TAG
    | OBJ_OFS_DELTA
    | OBJ_REF_DELTA
    deriving (Show, Eq)

objTypeRepr :: ObjectType -> String
objTypeRepr OBJ_COMMIT = "commit"
objTypeRepr OBJ_TREE = "tree"
objTypeRepr OBJ_BLOB = "blob"
objTypeRepr OBJ_TAG = "tag"
objTypeRepr _ = error "Has no representation"

-- Packfile Parsing

packfileParser :: Parser PackfileWithDeltas
packfileParser = do
    packfileHeader@PackfileHeader{..} <- parsePackHeader
    unless (magicString == "PACK") $
        fail "Not a pack file: Magic string mismatch"
    unless (packfileVersion == 2) $
        fail ("Unsupported packfile version: (Expected 2, Got: " <> show packfileVersion <> ")")
    packfileObjects <- count objectsInPackfile objectParser
    receivedChecksum <- sha1Parser <* endOfInput
    pure PackfileWithDeltas{..}

parsePackfile :: BL.ByteString -> Either String [RawUndeltifiedObject]
parsePackfile input = do
    packfile <- parseOnly packfileParser input
    let inputWithoutChecksum = BL.dropEnd 20 input
        computedChecksum = hashlazy inputWithoutChecksum
        rcvdChecksum = receivedChecksum packfile
    unless (rcvdChecksum == computedChecksum) $
        Left ("Checksum Mismatch: Expected: " <> show rcvdChecksum <> ", Got: " <> show computedChecksum)
    reconstructDeltaObjects $ packfileObjects packfile

-- Packfile Header Parsing

parsePackHeader :: Parser PackfileHeader
parsePackHeader = do
    magicString <- BL.fromStrict <$> take 4
    packfileVersion <- getIntbe
    objectsInPackfile <- getIntbe
    pure PackfileHeader{..}

getIntbe :: Parser Int
getIntbe = do
    b1 <- fromIntegral <$> anyWord8
    b2 <- fromIntegral <$> anyWord8
    b3 <- fromIntegral <$> anyWord8
    b4 <- fromIntegral <$> anyWord8
    return $ (b1 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 8) .|. b4

-- Packfile Object Parsing

-- Undeltified and deltified object

objectParser :: Parser RawObject
objectParser = do
    rawObjHeader <- objHeaderParser
    let expectedSize = fromIntegral $ objSize rawObjHeader
    case objType rawObjHeader of
        OBJ_OFS_DELTA ->
            fail $ "Unsupported object type: " <> show OBJ_OFS_DELTA
        OBJ_REF_DELTA -> do
            parentSha1 <- sha1Parser
            decompressedData <- decompressParser
            let actualSize = BL.length decompressedData
            when (expectedSize /= actualSize) $
                fail (objSizeMismatch expectedSize actualSize)
            pure $ mkDeltifiedObj rawObjHeader parentSha1 decompressedData
        _ -> do
            decompressedData <- decompressParser
            let actualSize = BL.length decompressedData
            when (expectedSize /= actualSize) $
                fail (objSizeMismatch expectedSize actualSize)
            pure $ mkUndeltifiedObj rawObjHeader decompressedData
  where
    objSizeMismatch expected actual =
        "Object size mismatch: (Expected: " <> show expected <> ", Actual: " <> show actual <> ")"

-- Type and size encoding
--
-- === Type encdoding
-- Valid object types are:
--
-- - OBJ_COMMIT (1)
-- - OBJ_TREE (2)
-- - OBJ_BLOB (3)
-- - OBJ_TAG (4)
-- - OBJ_OFS_DELTA (6)
-- - OBJ_REF_DELTA (7)
--
-- Type 5 is reserved for future expansion. Type 0 is invalid.
--
-- === Size encoding
--
-- This document uses the following "size encoding" of non-negative
-- integers: From each byte, the seven least significant bits are
-- used to form the resulting integer. As long as the most significant
-- bit is 1, this process continues; the byte with MSB 0 provides the
-- last seven bits.  The seven-bit chunks are concatenated. Later
-- values are more significant.

objHeaderParser :: Parser RawObjectHeader
objHeaderParser = do
    byte <- fromIntegral <$> anyWord8
    let objectTypeInt = (byte `shiftR` 4) .&. 0x7
    let maybeObjectType = intToObjectType objectTypeInt
    let szSoFar = byte .&. 0xf
    case maybeObjectType of
        Nothing -> fail $ "Invalid object type: " <> show objectTypeInt
        Just objType -> do
            objSize <- if isMsbSet byte then objSizeParser szSoFar 0 else pure szSoFar
            pure RawObjectHeader{..}

intToObjectType :: Int -> Maybe ObjectType
intToObjectType 1 = Just OBJ_COMMIT
intToObjectType 2 = Just OBJ_TREE
intToObjectType 3 = Just OBJ_BLOB
intToObjectType 4 = Just OBJ_TAG
intToObjectType 6 = Just OBJ_OFS_DELTA
intToObjectType 7 = Just OBJ_REF_DELTA
intToObjectType _ = Nothing

isMsbSet :: Int {- 8 bit int -} -> Bool
isMsbSet w = w .&. 0x80 /= 0

objSizeParser :: Int -> Int -> Parser Int
objSizeParser sizeSoFar iteration = do
    nextByte <- fromIntegral <$> anyWord8
    let sizeToAdd = (nextByte .&. 0x7f) `shiftL` (4 + iteration * 7)
        newSize = sizeSoFar + sizeToAdd
    if isMsbSet nextByte
        then objSizeParser newSize (iteration + 1)
        else pure newSize

-- === Deltified representation
--
-- Both ofs-delta and ref-delta store the "delta" to be applied to
-- another object (called 'base object') to reconstruct the object. The
-- difference between them is, ref-delta directly encodes base object
-- name. If the base object is in the same pack, ofs-delta encodes
-- the offset of the base object in the pack instead.
--
-- The delta data starts with the size of the base object and the
-- size of the object to be reconstructed. These sizes are
-- encoded using the size encoding from above.  The remainder of
-- the delta data is a sequence of instructions to reconstruct the object
-- from the base object. If the base object is deltified, it must be
-- converted to canonical form first. Each instruction appends more and
-- more data to the target object until it's complete. There are two
-- supported instructions so far: one for copy a byte range from the
-- source object and one for inserting new data embedded in the
-- instruction itself.
--
-- ==== Instruction to copy from base object
--
--   +----------+---------+---------+---------+---------+-------+-------+-------+
--   | 1xxxxxxx | offset1 | offset2 | offset3 | offset4 | size1 | size2 | size3 |
--   +----------+---------+---------+---------+---------+-------+-------+-------+
--
-- This is the instruction format to copy a byte range from the source
-- object. It encodes the offset to copy from and the number of bytes to
-- copy. Offset and size are in little-endian order.
--
-- All offset and size bytes are optional. This is to reduce the
-- instruction size when encoding small offsets or sizes. The first seven
-- bits in the first octet determines which of the next seven octets is
-- present. If bit zero is set, offset1 is present. If bit one is set
-- offset2 is present and so on.
--
-- Note that a more compact instruction does not change offset and size
-- encoding. For example, if only offset2 is omitted like below, offset3
-- still contains bits 16-23. It does not become offset2 and contains
-- bits 8-15 even if it's right next to offset1.
--
--   +----------+---------+---------+
--   | 10000101 | offset1 | offset3 |
--   +----------+---------+---------+
--
-- In its most compact form, this instruction only takes up one byte
-- (0x80) with both offset and size omitted, which will have default
-- values zero. There is another exception: size zero is automatically
-- converted to 0x10000.
--
-- ==== Instruction to add new data
--
--   +----------+============+
--   | 0xxxxxxx |    data    |
--   +----------+============+
--
-- This is the instruction to construct target object without the base
-- object. The following data is appended to the target object. The first
-- seven bits of the first octet determines the size of data in
-- bytes. The size must be non-zero.

-- Packfile Reconstruction: Parsing Deltafied Objcet to Instructions

data InstructionType = CopyType | AddNewType deriving (Show, Eq)

instructionType :: Word8 -> InstructionType
instructionType firstByte
    | firstByte .&. 0x80 == 0 = AddNewType
    | otherwise = CopyType

data Instruction
    = Copy Int Int
    | AddNew BL.ByteString
    deriving (Show, Eq)

data DeltaContent = DeltaContent
    { baseObjSize :: Int
    , reconstructedObjSize :: Int
    , instructions :: [Instruction]
    }
    deriving (Show, Eq)

deltaContentParser :: Parser DeltaContent
deltaContentParser = do
    baseObjSize <- deltaHeaderObjSizeParser
    reconstructedObjSize <- deltaHeaderObjSizeParser
    instructions <- many' instructionParser
    pure $ DeltaContent baseObjSize reconstructedObjSize instructions

deltaHeaderObjSizeParser :: Parser Int
deltaHeaderObjSizeParser = deltaHeaderObjSizeParser' 0 0
  where
    deltaHeaderObjSizeParser' :: Int -> Int -> Parser Int
    deltaHeaderObjSizeParser' sizeSoFar iteration = do
        nextByte <- fromIntegral <$> anyWord8
        let sizeToAdd = (nextByte .&. 0x7f) `shiftL` (iteration * 7)
            newSize = sizeSoFar + sizeToAdd
        if isMsbSet nextByte
            then deltaHeaderObjSizeParser' newSize (iteration + 1)
            else pure newSize

instructionParser :: Parser Instruction
instructionParser = do
    firstByte <- anyWord8
    case instructionType firstByte of
        CopyType -> copyInstructionParser firstByte
        AddNewType -> addInstructionParser firstByte

addInstructionParser :: Word8 -> Parser Instruction
addInstructionParser firstByte = do
    let dataSize = fromIntegral $ firstByte .&. 0x7f
    dataToAppend <- take dataSize
    pure $ AddNew $ BL.fromStrict dataToAppend

copyInstructionParser :: Word8 -> Parser Instruction
copyInstructionParser firstByte = do
    offsets <- count numOffsets anyWord8
    sizes <- count numSizes anyWord8
    let offset = convertToInt hasOffset 0 0 0 4 offsets
    let size = convertToInt hasSize 0 0 0 3 sizes
    pure $ Copy offset size
  where
    hasOffset i = firstByte .&. (1 `shiftL` i) /= 0
    hasSize i = firstByte .&. (1 `shiftL` (i + 4)) /= 0
    numOffsets = popCount $ firstByte .&. 0xf
    numSizes = popCount $ firstByte .&. 0x70

convertToInt :: (Int -> Bool) -> Int -> Int -> Int -> Int -> [Word8] -> Int
convertToInt _ offset _ _ _ [] = offset
convertToInt isPresent val shift i maxI (b : bytes)
    | i == maxI = val
    | isPresent i = convertToInt isPresent newVal (shift + 8) (i + 1) maxI bytes
    | otherwise = convertToInt isPresent val (shift + 8) (i + 1) maxI (b : bytes)
  where
    newVal = val + fromIntegral b `shiftL` shift

-- Packfile Reconstruction: Applying Instructions to Deltafied Objects

reconstructDeltaFromBase ::
    RawUndeltifiedObject ->
    RawDeltifiedObject ->
    Either String RawUndeltifiedObject
reconstructDeltaFromBase baseObject deltaObject = do
    DeltaContent{..} <- parseOnly deltaContentParser (deltaObjData deltaObject)
    let reconstructedObjContent =
            foldl (applyInstruction (objData baseObject)) "" instructions
    pure $
        RawUndeltifiedObject
            (RawObjectHeader (objType . objHeader $ baseObject) (fromIntegral $ BL.length reconstructedObjContent))
            reconstructedObjContent

applyInstruction :: BL.ByteString -> BL.ByteString -> Instruction -> BL.ByteString
applyInstruction base obj (Copy offset size) =
    obj <> BL.take (fromIntegral size) (BL.drop (fromIntegral offset) base)
applyInstruction _ obj (AddNew dataToAppend) =
    obj <> dataToAppend

reconstructDeltaObjects :: [RawObject] -> Either String [RawUndeltifiedObject]
reconstructDeltaObjects rawObjects =
    Map.elems <$> reconstructGitObjects baseObjectsMap objectsToReconstruct
  where
    baseObjectsMap =
        Map.fromList $ map (rawObjSHA1 &&& id) $ mapMaybe getUndeltified rawObjects
    objectsToReconstruct = Seq.fromList $ mapMaybe getDeltified rawObjects

reconstructGitObjects ::
    Map.Map (Digest SHA1) RawUndeltifiedObject ->
    Seq.Seq RawDeltifiedObject ->
    Either String (Map.Map (Digest SHA1) RawUndeltifiedObject)
reconstructGitObjects baseObjects Seq.Empty = pure baseObjects
reconstructGitObjects baseObjects (deltaObj :<| rest) =
    case Map.lookup (parentSha1 deltaObj) baseObjects of
        Nothing -> reconstructGitObjects baseObjects (rest :|> deltaObj)
        Just baseOject -> do
            reconstructedObj <- reconstructDeltaFromBase baseOject deltaObj
            let baseObjects' = Map.insert (rawObjSHA1 reconstructedObj) reconstructedObj baseObjects
            reconstructGitObjects baseObjects' rest
