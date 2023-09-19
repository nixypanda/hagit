{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ObjectParse (gitContentToObject, treeEntryParser) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (char, decimal, digit, space)
import Data.Attoparsec.ByteString.Lazy (
    Parser,
    many',
    many1,
    parseOnly,
    string,
    take,
    takeWhile,
 )
import Data.ByteString.Lazy as BL (ByteString, fromStrict)
import Data.ByteString.Lazy.Char8 as BLC (pack)
import Data.Word8 (_nul)
import Object (GitObject (..), TreeEntry (..))
import ParsingUtils (ParseError, sha1Parser)
import Prelude hiding (take, takeWhile)

gitBlobParser :: Parser GitObject
gitBlobParser = do
    _ <- string "blob "
    len <- decimal
    _ <- char '\0'
    content <- take len
    return (Blob $ BL.fromStrict content)

treeEntryParser :: Parser TreeEntry
treeEntryParser = do
    entryMode <- BLC.pack <$> many' digit <* space
    entryName <- BL.fromStrict <$> takeWhile (/= _nul)
    _ <- char '\0'
    entrySha1 <- sha1Parser
    pure TreeEntry{..}

treeParser :: Parser GitObject
treeParser = do
    _ <- string "tree "
    _ <- many1 digit
    _ <- char '\0'
    entries <- many' treeEntryParser
    return (Tree entries)

gitObjectParser :: Parser GitObject
gitObjectParser = treeParser <|> gitBlobParser

gitContentToObject :: BL.ByteString -> Either ParseError GitObject
gitContentToObject = parseOnly gitObjectParser
