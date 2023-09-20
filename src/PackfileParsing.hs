{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PackfileParsing (parsePackfile) where

import Control.Monad (unless, when)
import Crypto.Hash (Digest, SHA1, hashlazy)
import Data.Attoparsec.ByteString.Lazy (
    Parser,
    anyWord8,
    endOfInput,
    parseOnly,
    take,
    takeLazyByteString,
 )
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import ParsingUtils (ParseError, sha1Parser)
import ZlibDecompression (DecompressError, DecompressionResult (..), decompressPartial)
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

data PackfileError
    = PackParseError ParseError
    | PackDecompressionError DecompressError
    | PackDecompressionErrorObjectSizeMismatch Int64 Int64
    | PackDecompressionErrorObjectNumberMismatch Int Int
    | PackIntegrityErrorChecksumMismatch (Digest SHA1) (Digest SHA1)
    | PackUnsupportedVersion Int Int
    | PackUnsupportedObjectError ObjectType
    deriving (Show)

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

parsePackfile :: BL.ByteString -> Either PackfileError PackfileWithDeltas
parsePackfile input = do
    let inputWithOutChecksum = BL.dropEnd 20 input
        checksumStr = BL.takeEnd 20 input
        computedChecksum = hashlazy inputWithOutChecksum
    receivedChecksum <- packParse (sha1Parser <* endOfInput) checksumStr
    unless (computedChecksum == receivedChecksum) $
        Left (PackIntegrityErrorChecksumMismatch receivedChecksum computedChecksum)

    (packfileHeader@PackfileHeader{..}, remainingInput) <-
        packParse wrarppedPackHeaderParser inputWithOutChecksum

    unless (magicString == "PACK") $
        Left (PackParseError "Not a pack file: Magic string mismatch")
    unless (packfileVersion == 2) $
        Left (PackUnsupportedVersion packfileVersion 2)

    packfileObjects <- parseObjects remainingInput
    unless (length packfileObjects == objectsInPackfile) $
        Left (PackDecompressionErrorObjectNumberMismatch objectsInPackfile (length packfileObjects))

    pure PackfileWithDeltas{..}
  where
    wrarppedPackHeaderParser = do
        header <- parsePackHeader
        rest <- takeLazyByteString
        pure (header, rest)

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

parseObject :: BL.ByteString -> Either PackfileError (RawObject, BL.ByteString)
parseObject remaining = do
    (rawObjHeader, remaining') <- packParse wrappedHeaderParser remaining
    let expectedSize = fromIntegral $ objSize rawObjHeader
    case objType rawObjHeader of
        OBJ_OFS_DELTA ->
            Left $ PackUnsupportedObjectError OBJ_OFS_DELTA
        OBJ_REF_DELTA -> do
            (parentSha1, remaining'') <- packParse wrappedSha1Parser remaining'
            DecompressionResult{..} <- decompressPartial' remaining''
            let actualSize = BL.length decompressedData
            when (expectedSize /= actualSize) $
                Left (PackDecompressionErrorObjectSizeMismatch expectedSize actualSize)
            pure (mkDeltifiedObj rawObjHeader parentSha1 decompressedData, unconsumedInput)
        _ -> do
            DecompressionResult{..} <- decompressPartial' remaining'
            let actualSize = BL.length decompressedData
            when (expectedSize /= actualSize) $
                Left (PackDecompressionErrorObjectSizeMismatch expectedSize actualSize)
            pure (mkUndeltifiedObj rawObjHeader decompressedData, unconsumedInput)
  where
    wrappedHeaderParser = do
        rawObjHeader <- objHeaderParser
        rest <- takeLazyByteString
        pure (rawObjHeader, rest)
    wrappedSha1Parser = do
        sha1 <- sha1Parser
        rest <- takeLazyByteString
        pure (sha1, rest)

parseObjects :: BL.ByteString -> Either PackfileError [RawObject]
parseObjects input = do
    (obj, remainingInput) <- parseObject input
    case parseObjects remainingInput of
        Left (PackParseError "not enough input") -> pure [obj]
        Left err -> Left err
        Right rest -> pure (obj : rest)

packParse :: Parser a -> BL.ByteString -> Either PackfileError a
packParse parser input = first PackParseError $ parseOnly parser input

decompressPartial' :: BL.ByteString -> Either PackfileError DecompressionResult
decompressPartial' = first PackDecompressionError . decompressPartial
