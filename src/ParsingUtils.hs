{-# LANGUAGE ImportQualifiedPost #-}

module ParsingUtils (
    ParseError,
    parseSHA1Str,
    sha1StrParser,
    sha1Parser,
) where

import Crypto.Hash (Digest, digestFromByteString)
import Crypto.Hash.Algorithms (SHA1)
import Data.Attoparsec.ByteString.Char8 (hexadecimal)
import Data.Attoparsec.ByteString.Lazy (Parser, anyWord8, count, parseOnly, satisfy)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.Word8 (isHexDigit)
import Prelude hiding (take, takeWhile)

type ParseError = String

hexStringParser :: Parser [Word8]
hexStringParser = count 20 hexPairParser

hexPairParser :: Parser Word8
hexPairParser = do
    hexDigits <- count 2 (satisfy isHexDigit)
    case parseOnly hexadecimal (BL.pack hexDigits) of
        Right value -> pure value
        _ -> fail "Invalid hexadecimal digits"

sha1StrParser :: Parser (Digest SHA1)
sha1StrParser = fromJust . digestFromByteString . BS.pack <$> hexStringParser

sha1Parser :: Parser (Digest SHA1)
sha1Parser = fromJust . digestFromByteString . BS.pack <$> count 20 anyWord8

parseSHA1Str :: BL.ByteString -> Either ParseError (Digest SHA1)
parseSHA1Str = parseOnly sha1StrParser
