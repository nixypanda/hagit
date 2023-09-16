{-# LANGUAGE ScopedTypeVariables #-}

module ObjectParse (gitContentToObject) where

import Data.ByteString.Lazy as BL (ByteString)
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Object (GitObject (Blob))
import Text.Parsec (ParseError, anyToken, char, count, digit, many1, parse, string)
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
