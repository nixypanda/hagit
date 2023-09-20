{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PackfileParsing (parsePackfile) where

import Control.Monad (unless, when)
import Crypto.Hash (Digest, SHA1, hashlazy)
import Data.Attoparsec.ByteString.Lazy (
    Parser,
    anyWord8,
    count,
    endOfInput,
    parseOnly,
    take,
 )
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Lazy qualified as BL
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

data RawUndeltifiedObject = RawUndeltifiedObject
    { objHeader :: RawObjectHeader
    , objData :: BL.ByteString
    }
    deriving (Show)

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
    deriving (Show)

data ObjectType
    = OBJ_COMMIT
    | OBJ_TREE
    | OBJ_BLOB
    | OBJ_TAG
    | OBJ_OFS_DELTA
    | OBJ_REF_DELTA
    deriving (Show)

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

parsePackfile :: BL.ByteString -> Either String PackfileWithDeltas
parsePackfile input = do
    packfile <- parseOnly packfileParser input
    let inputWithoutChecksum = BL.dropEnd 20 input
        computedChecksum = hashlazy inputWithoutChecksum
        rcvdChecksum = receivedChecksum packfile
    unless (rcvdChecksum == computedChecksum) $
        Left ("Checksum Mismatch: Expected: " <> show rcvdChecksum <> ", Got: " <> show computedChecksum)
    pure packfile

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
