{-# LANGUAGE ScopedTypeVariables #-}

module ObjectParse (gitContentToObject, parseSHA1Str) where

import Crypto.Hash (Digest, digestFromByteString)
import Crypto.Hash.Algorithms (SHA1)
import Data.Binary (Word8)
import Data.ByteString as BS (pack)
import Data.ByteString.Lazy as BL (ByteString)
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Data.Maybe (fromJust)
import Numeric (readHex)
import Object (GitObject (Blob))
import Text.Parsec (ParseError, anyToken, char, count, digit, hexDigit, many1, parse, string)
import Text.Parsec.ByteString.Lazy (Parser)

gitBlobParser :: Parser GitObject
gitBlobParser = do
    _ <- string "blob "
    len <- many1 digit
    _ <- char '\0'
    -- Slow, make fast
    content :: String <- count (read len) anyToken
    return (Blob (BLU.fromString content))

gitContentToObject :: BL.ByteString -> Either ParseError GitObject
gitContentToObject = parse gitBlobParser ""

hexStringParser :: Parser [Word8]
hexStringParser = count 20 hexPairParser

hexPairParser :: Parser Word8
hexPairParser = do
    digits <- count 2 hexDigit
    case readHex digits of
        [(value, "")] -> return (fromIntegral value)
        _ -> fail "Invalid hexadecimal digits"

parseSHA1Str :: BL.ByteString -> Either ParseError (Digest SHA1)
parseSHA1Str bs = do
    hexString <- parse hexStringParser "" bs
    pure $ fromJust $ digestFromByteString $ BS.pack hexString
