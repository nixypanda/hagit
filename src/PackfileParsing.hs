{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PackfileParsing (parsePackfile) where

import Codec.Compression.Zlib (defaultDecompressParams)
import Codec.Compression.Zlib.Internal (DecompressStream (..), decompressST, zlibFormat)
import Control.Monad (unless, when)
import Control.Monad.ST.Lazy (ST, runST)
import Crypto.Hash (Digest, SHA1)
import Data.Attoparsec.ByteString.Lazy (
    Parser,
    anyWord8,
    parseOnly,
    take,
    takeLazyByteString,
 )
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Lazy as BL (ByteString, empty, fromStrict, length, toStrict)
import ParsingUtils (ParseError, sha1Parser)
import Prelude hiding (take)

data ObjectType
    = OBJ_COMMIT
    | OBJ_TREE
    | OBJ_BLOB
    | OBJ_TAG
    | OBJ_OFS_DELTA
    | OBJ_REF_DELTA
    deriving (Show)

intToObjectType :: Int -> Maybe ObjectType
intToObjectType 1 = Just OBJ_COMMIT
intToObjectType 2 = Just OBJ_TREE
intToObjectType 3 = Just OBJ_BLOB
intToObjectType 4 = Just OBJ_TAG
intToObjectType 6 = Just OBJ_OFS_DELTA
intToObjectType 7 = Just OBJ_REF_DELTA
intToObjectType _ = Nothing

getIntbe :: Parser Int
getIntbe = do
    b1 <- fromIntegral <$> anyWord8
    b2 <- fromIntegral <$> anyWord8
    b3 <- fromIntegral <$> anyWord8
    b4 <- fromIntegral <$> anyWord8
    return $ (b1 `shiftL` 24) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 8) .|. b4

isMsbSet :: Int {- 8 bit int -} -> Bool
isMsbSet w = w .&. 0x80 /= 0

objectSizeParser :: Int -> Int -> Parser Int
objectSizeParser sizeSoFar iteration = do
    nextByte <- fromIntegral <$> anyWord8
    let sizeToAdd = (nextByte .&. 0x7f) `shiftL` (4 + iteration * 7)
        newSize = sizeSoFar + sizeToAdd
    if isMsbSet nextByte
        then objectSizeParser newSize (iteration + 1)
        else pure newSize

typeAndSizeParser :: Parser (ObjectType, Int)
typeAndSizeParser = do
    byte <- fromIntegral <$> anyWord8
    let objectTypeInt = (byte `shiftR` 4) .&. 0x7
    let maybeObjectType = intToObjectType objectTypeInt
    let sizeSoFar = byte .&. 0xf
    case maybeObjectType of
        Nothing -> fail "Invalid object type"
        Just objectType -> do
            objectSize <-
                if isMsbSet byte
                    then objectSizeParser sizeSoFar 0
                    else pure sizeSoFar
            pure (objectType, objectSize)

data PackfileHeader = PackfileHeader
    { magicString :: BL.ByteString
    , packfileVersion :: Int
    , objectsInPackfile :: Int
    }

parsePackHeader :: Parser PackfileHeader
parsePackHeader = do
    magicString <- BL.fromStrict <$> take 4
    unless (magicString == "PACK") $ fail "Not a pack file"
    packfileVersion <- getIntbe
    unless (packfileVersion == 2) $ fail "Incompatible version"
    objectsInPackfile <- getIntbe
    pure PackfileHeader{..}

data RawObjectHeader = RawObjectHeader
    { objType :: ObjectType
    , objSize :: Int
    }
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

data RawObject = RawUndeltified RawUndeltifiedObject | RawDeltified RawDeltifiedObject deriving (Show)

parseObject :: BL.ByteString -> Either String (RawObject, BL.ByteString)
parseObject remainingPackfile = do
    (objType', objSize, remainingPackfile') <- parseOnly wrappedTypeAndSizeParser remainingPackfile
    case objType' of
        OBJ_OFS_DELTA ->
            Left "Unsupported object type: Offest based delta objects are not yet supported"
        OBJ_REF_DELTA -> do
            (parentSha1, remainingPackfile'') <- parseOnly wrappedSha1Parser remainingPackfile'
            (deltaObjData, remainingPackfile''') <- decompressPartial remainingPackfile''
            let objType = OBJ_REF_DELTA
            let deltaObjHeader = RawObjectHeader{..}
            when (objSize /= fromIntegral (BL.length deltaObjData)) $ Left "Invalid object size"
            pure (RawDeltified RawDeltifiedObject{..}, remainingPackfile''')
        objType -> do
            (objData, remainingPackfile'') <- decompressPartial remainingPackfile'
            let objHeader = RawObjectHeader{..}
            when (objSize /= fromIntegral (BL.length objData)) $ Left "Invalid object size"
            pure (RawUndeltified RawUndeltifiedObject{..}, remainingPackfile'')
  where
    wrappedTypeAndSizeParser = do
        (t, s) <- typeAndSizeParser
        rest <- takeLazyByteString
        pure (t, s, rest)
    wrappedSha1Parser = do
        sha1 <- sha1Parser
        rest <- takeLazyByteString
        pure (sha1, rest)

parseObjects :: BL.ByteString -> Either String ([RawObject], BL.ByteString)
parseObjects input = do
    (obj, remainingInput) <- parseObject input
    -- rest <- parseObjects remainingInput
    case parseObjects remainingInput of
        Left _err -> pure ([obj], remainingInput)
        Right (rest, bs) -> pure (obj : rest, bs)

parsePackfile :: BL.ByteString -> Either ParseError ([RawObject], Int, BL.ByteString)
parsePackfile input = do
    body <- parseOnly wrarppedPackHeaderParser input
    (objects, remainingInput) <- parseObjects body
    pure (objects, Prelude.length objects, remainingInput)
  where
    wrarppedPackHeaderParser = parsePackHeader *> takeLazyByteString

decompressPartial :: BL.ByteString -> Either String (BL.ByteString, BL.ByteString)
decompressPartial input = do
    let decompresser = decompressST zlibFormat defaultDecompressParams
    runST $ decompressLoop input BL.empty decompresser

decompressLoop ::
    BL.ByteString ->
    BL.ByteString ->
    DecompressStream (ST s) ->
    ST s (Either String (BL.ByteString, BL.ByteString))
decompressLoop input output (DecompressInputRequired next) = do
    decompressLoop BL.empty output =<< next (BL.toStrict input)
decompressLoop input output (DecompressOutputAvailable outChunk next) = do
    decompressLoop input (output `mappend` BL.fromStrict outChunk) =<< next
decompressLoop _ output (DecompressStreamEnd unconsumedInput) = do
    pure $ Right (output, BL.fromStrict unconsumedInput)
decompressLoop _ _ (DecompressStreamError err) =
    pure $ Left $ show err
