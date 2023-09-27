{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PackfileParsing (
    parsePackfile,
    -- exports for testing
    getIntBe,
    objHeaderParser,
    objectParser,
    instructionParser,
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
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Word8 (Word8)
import Object (GitObject (..), objBody, objSha1, objType)
import ObjectParse (blobParser', commitParser', gitObjectParser, treeParser')
import Packfile (
    DeltaContent (..),
    DeltafiedObj (..),
    Instruction (..),
    InstructionType (..),
    PackObjHeader (..),
    PackObjType (..),
    PackObject (..),
    PackfileHeader (..),
    PackfileWithDeltas (..),
    getDeltified,
    getUndeltified,
    instructionType,
    mkDeltifiedObj,
    mkPackObjHeader,
    objHeaderRepr,
    packObjLen,
 )
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

parsePackfile :: BL.ByteString -> Either String [GitObject]
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
    packfileVersion <- getIntBe
    objectsInPackfile <- getIntBe
    pure PackfileHeader{..}

getIntBe :: Parser Int
getIntBe = do
    b1 <- fromIntegral <$> anyWord8
    b2 <- fromIntegral <$> anyWord8
    b3 <- fromIntegral <$> anyWord8
    b4 <- fromIntegral <$> anyWord8
    return $ (b1 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 8) .|. b4

-- Packfile Object Parsing

-- Undeltified and deltified object

objectParser :: Parser PackObject
objectParser = do
    rawObjHeader <- objHeaderParser
    let expectedSize = packObjSize rawObjHeader
    rawObj <- case packObjType rawObjHeader of
        OBJ_OFS_DELTA -> fail $ "Unsupported object type: " <> show OBJ_OFS_DELTA
        OBJ_REF_DELTA -> mkDeltifiedObj rawObjHeader <$> sha1Parser <*> decompressParser
        OBJ_BLOB -> gitObjParser (Undeltafied . Blob <$> blobParser' expectedSize)
        OBJ_TREE -> gitObjParser (Undeltafied . Tree <$> treeParser')
        OBJ_COMMIT -> gitObjParser (Undeltafied . Commit <$> commitParser')
        OBJ_TAG -> fail $ "Unsupported object type: " <> show OBJ_TAG

    let actualSize = packObjLen rawObj
    when (expectedSize /= actualSize) $
        fail (objSizeMismatch expectedSize actualSize)

    pure rawObj
  where
    objSizeMismatch expected actual =
        "Object size mismatch: (Expected: " <> show expected <> ", Actual: " <> show actual <> ")"
    gitObjParser objParser = do
        decompressed <- decompressParser
        case parseOnly objParser decompressed of
            Left err -> fail err
            Right obj -> pure obj

-- gitObjParser' :: RawObjectHeader -> BL.ByteString -> Parser GitObject
-- gitObjParser' objHeader objData =
--     gitObjectParser (objHeaderRepr objHeader <> "\0" <> objData)

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

objHeaderParser :: Parser PackObjHeader
objHeaderParser = do
    byte <- fromIntegral <$> anyWord8
    let objectTypeInt = (byte `shiftR` 4) .&. 0x7
    let maybeObjectType = intToObjectType objectTypeInt
    let szSoFar = byte .&. 0xf
    case maybeObjectType of
        Nothing -> fail $ "Invalid object type: " <> show objectTypeInt
        Just packObjType -> do
            packObjSize <- if isMsbSet byte then objSizeParser szSoFar 0 else pure szSoFar
            pure PackObjHeader{..}

intToObjectType :: Int -> Maybe PackObjType
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

reconstructDeltaFromBase :: GitObject -> DeltafiedObj -> Either String GitObject
reconstructDeltaFromBase baseObject deltaObject = do
    DeltaContent{..} <- parseOnly deltaContentParser (deltaObjData deltaObject)
    let reconstructedObjContent = foldl (applyInstruction (objBody baseObject)) "" instructions
        reconstructedHeader = mkPackObjHeader (objType baseObject) (fromIntegral $ BL.length reconstructedObjContent)
        reconstructedObjData = objHeaderRepr reconstructedHeader <> "\0" <> reconstructedObjContent
    parseOnly gitObjectParser reconstructedObjData

applyInstruction :: BL.ByteString -> BL.ByteString -> Instruction -> BL.ByteString
applyInstruction base obj (Copy offset size) =
    obj <> BL.take (fromIntegral size) (BL.drop (fromIntegral offset) base)
applyInstruction _ obj (AddNew dataToAppend) =
    obj <> dataToAppend

reconstructDeltaObjects :: [PackObject] -> Either String [GitObject]
reconstructDeltaObjects rawObjects =
    Map.elems <$> reconstructGitObjects baseObjectsMap objectsToReconstruct
  where
    baseObjectsMap = Map.fromList $ map (objSha1 &&& id) $ mapMaybe getUndeltified rawObjects
    objectsToReconstruct = Seq.fromList $ mapMaybe getDeltified rawObjects

reconstructGitObjects ::
    Map.Map (Digest SHA1) GitObject ->
    Seq.Seq DeltafiedObj ->
    Either String (Map.Map (Digest SHA1) GitObject)
reconstructGitObjects baseObjects Seq.Empty = pure baseObjects
reconstructGitObjects baseObjects (deltaObj :<| rest) =
    case Map.lookup (deltaObjParentSha1 deltaObj) baseObjects of
        Nothing -> reconstructGitObjects baseObjects (rest :|> deltaObj)
        Just baseOject -> do
            reconstructedObj <- reconstructDeltaFromBase baseOject deltaObj
            let baseObjects' = Map.insert (objSha1 reconstructedObj) reconstructedObj baseObjects
            reconstructGitObjects baseObjects' rest
