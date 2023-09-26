{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ObjectParse (
    gitContentToObject,
    commitParser,
    treeParser,
    blobParser,
    -- Packfile Parsing Requirements
    commitParser',
    treeParser',
    blobParser',
    -- Testing
    treeEntryParser,
    gitObjectParser,
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (
    anyChar,
    char,
    decimal,
    digit,
    manyTill,
    skipSpace,
    space,
 )
import Data.Attoparsec.ByteString.Lazy (
    Parser,
    many',
    many1,
    option,
    parseOnly,
    string,
    take,
    takeWhile,
    takeWhileIncluding,
 )
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC (pack)
import Data.Time (parseTimeM)
import Data.Time.Format (defaultTimeLocale)
import Data.Word8 (_greater, _nul)
import Object (CommitInner (..), Contributor (..), GitObject (..), TreeEntry (..))
import ParsingUtils (ParseError, sha1Parser, sha1StrParser)
import Prelude hiding (take, takeWhile)

blobParser :: Parser BL.ByteString
blobParser = do
    _ <- string "blob "
    len <- decimal
    _ <- char '\0'
    blobParser' len

blobParser' :: Int -> Parser BL.ByteString
blobParser' n = BL.fromStrict <$> take n

treeEntryParser :: Parser TreeEntry
treeEntryParser = do
    entryMode <- BLC.pack <$> many' digit <* space
    entryName <- BL.fromStrict <$> takeWhile (/= _nul)
    _ <- char '\0'
    entrySha1 <- sha1Parser
    pure TreeEntry{..}

treeParser :: Parser [TreeEntry]
treeParser = do
    _ <- string "tree "
    _ <- many1 digit
    _ <- char '\0'
    treeParser'

treeParser' :: Parser [TreeEntry]
treeParser' = many' treeEntryParser

commitParser :: Parser CommitInner
commitParser = do
    _ <- string "commit "
    _ <- many1 digit
    _ <- char '\0'
    commitParser'

commitParser' :: Parser CommitInner
commitParser' = do
    treeSha1 <- parseSha1Header "tree"
    _ <- char '\n'
    parentSha1 <- option Nothing (Just <$> parseSha1Header "parent" <* char '\n')
    commitAuthor <- parseContributor "author"
    commitCommitter <- parseContributor "committer"
    _ <- char '\n'
    commitMessage <- BLC.pack <$> many' anyChar
    return $ CommitInner{..}
  where
    parseSha1Header headerName = string headerName *> char ' ' *> sha1StrParser
    parseContributor name = do
        _ <- string name *> char ' '
        contribNameAndEmail <- BL.fromStrict <$> takeWhileIncluding (/= _greater)
        contribDate <- parseTimestamp
        pure Contributor{..}

    parseTimestamp = do
        let formatStr = "%s %z"
        _ <- skipSpace
        inputStr <- manyTill anyChar (char '\n')
        case parseTimeM False defaultTimeLocale formatStr inputStr of
            Just timestamp -> pure timestamp
            Nothing -> fail $ "InvalidTimestamp: " <> inputStr

gitObjectParser :: Parser GitObject
gitObjectParser =
    (Tree <$> treeParser)
        <|> (Blob <$> blobParser)
        <|> (Commit <$> commitParser)

gitContentToObject :: BL.ByteString -> Either ParseError GitObject
gitContentToObject = parseOnly gitObjectParser
