{-# LANGUAGE ScopedTypeVariables #-}

module ObjectParse (gitContentToObject, parseSHA1Str, treeEntryParser, sha1Parser) where

import Crypto.Hash (Digest, digestFromByteString)
import Crypto.Hash.Algorithms (SHA1)
import Data.Binary (Word8)
import Data.ByteString as BS (pack)
import Data.ByteString.Lazy as BL (ByteString)
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Numeric (readHex)
import Object (GitObject (..), TreeEntry (TreeEntry))
import Text.Parsec (
    ParseError,
    anyToken,
    char,
    count,
    digit,
    hexDigit,
    many,
    many1,
    manyTill,
    parse,
    satisfy,
    space,
    string,
    (<|>),
 )
import Text.Parsec.ByteString.Lazy (Parser)
import Text.Parsec.Char (anyChar)

gitBlobParser :: Parser GitObject
gitBlobParser = do
    _ <- string "blob "
    len <- many1 digit
    _ <- char '\0'
    -- Slow, make fast
    content :: String <- count (read len) anyToken
    return (Blob (BLU.fromString content))

treeEntryParser :: Parser TreeEntry
treeEntryParser = do
    -- Slow, make fast
    mode' :: String <- many digit <* space
    name' :: String <- manyTill anyChar (char '\NUL')
    -- there has to be a better way to do this
    sha1Str :: [Char] <- count 20 (satisfy (\c -> ord c < 256))
    let sha1 :: [Word8] = map (fromIntegral . ord) sha1Str
    pure
        ( TreeEntry
            (BLU.fromString mode')
            (BLU.fromString name')
            -- At this point we know for a fact that it is valid SHA
            (fromJust $ digestFromByteString $ BS.pack sha1)
        )

treeParser :: Parser GitObject
treeParser = do
    _ <- string "tree "
    _ <- many1 digit
    _ <- char '\0'
    entries <- many treeEntryParser
    return (Tree entries)

gitObjectParser :: Parser GitObject
gitObjectParser = treeParser <|> gitBlobParser

gitContentToObject :: BL.ByteString -> Either ParseError GitObject
gitContentToObject = parse gitObjectParser ""

hexStringParser :: Parser [Word8]
hexStringParser = count 20 hexPairParser

hexPairParser :: Parser Word8
hexPairParser = do
    digits <- count 2 hexDigit
    case (readHex digits :: [(Int, String)]) of
        [(value, "")] -> return (fromIntegral value)
        _ -> fail "Invalid hexadecimal digits"

sha1Parser :: Parser (Digest SHA1)
sha1Parser = fromJust . digestFromByteString . BS.pack <$> hexStringParser

parseSHA1Str :: BL.ByteString -> Either ParseError (Digest SHA1)
parseSHA1Str = parse sha1Parser ""
