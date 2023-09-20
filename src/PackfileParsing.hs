{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PackfileParsing (parsePackfile) where

import Codec.Compression.Zlib (defaultDecompressParams)
import Codec.Compression.Zlib.Internal (DecompressStream (..), decompressST, zlibFormat)
import Control.Monad (unless)
import Control.Monad.ST.Lazy (ST, runST)
import Data.Attoparsec.ByteString.Lazy (
    Parser,
    anyWord8,
    parseOnly,
    take,
    takeLazyByteString,
 )
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString.Lazy as BL (ByteString, empty, fromStrict, splitAt, toStrict)
import ParsingUtils (ParseError)
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
intToObjectType 5 = Just OBJ_OFS_DELTA
intToObjectType 6 = Just OBJ_REF_DELTA
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

data RawObject = RawObject
    { objType :: ObjectType
    , objSize :: Int
    , objData :: BL.ByteString
    }
    deriving (Show)

parseObject :: BL.ByteString -> Either String (RawObject, BL.ByteString)
parseObject remainingPackfile = do
    (objType, objSize, remainingPackfile') <- parseOnly wrappedTypeAndSizeParser remainingPackfile
    (objData, remainingPackfile'') <- decompressPartial remainingPackfile'
    pure (RawObject{..}, remainingPackfile'')
  where
    wrappedTypeAndSizeParser = do
        (t, s) <- typeAndSizeParser
        rest <- takeLazyByteString
        pure (t, s, rest)

parseObjects :: BL.ByteString -> Either String [RawObject]
parseObjects input = do
    (obj, remainingInput) <- parseObject input
    rest <- parseObjects remainingInput
    -- let rest = case parseObjects remainingInput of
    --         Left _err -> []
    --         Right rest' -> rest'
    pure $ obj : rest

parsePackfile :: BL.ByteString -> Either ParseError [RawObject]
parsePackfile input = do
    body <- parseOnly wrarppedPackHeaderParser input
    parseObjects body
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
    let (inputChunk, remainingInput) = BL.splitAt 4096 input
    decompressLoop remainingInput output =<< next (BL.toStrict inputChunk)
decompressLoop input output (DecompressOutputAvailable outChunk next) = do
    decompressLoop input (output `mappend` BL.fromStrict outChunk) =<< next
decompressLoop _ output (DecompressStreamEnd unconsumedInput) = do
    pure $ Right (output, BL.fromStrict unconsumedInput)
decompressLoop _ _ (DecompressStreamError err) =
    pure $ Left $ show err
