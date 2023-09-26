{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ObjectParse (
    gitContentToObject,
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
    signed,
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
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word8 (_greater, _nul)
import Object (CommitInner (..), Contributor (..), GitObject (..), TreeEntry (..))
import ParsingUtils (ParseError, sha1Parser, sha1StrParser)
import Prelude hiding (take, takeWhile)

gitBlobParser :: Parser GitObject
gitBlobParser = do
    _ <- string "blob "
    len <- decimal
    _ <- char '\0'
    blobParser' len

blobParser' :: Int -> Parser GitObject
blobParser' n = Blob . BL.fromStrict <$> take n

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
    treeParser'

treeParser' :: Parser GitObject
treeParser' = Tree <$> many' treeEntryParser

commitParser :: Parser GitObject
commitParser = do
    _ <- string "commit "
    _ <- many1 digit
    _ <- char '\0'
    commitParser'

commitParser' :: Parser GitObject
commitParser' = do
    treeSha <- parseSha1Header "tree"
    _ <- char '\n'
    parentSha <- option Nothing (Just <$> parseSha1Header "parent" <* char '\n')
    commitAuthor <- parseContributor "author"
    _ <- char '\n'
    commitCommitter <- parseContributor "committer"
    _ <- char '\n'
    _ <- char '\n'
    commitMessage <- BLC.pack <$> many' anyChar
    return $ Commit CommitInner{..}
  where
    parseSha1Header headerName = string headerName *> char ' ' *> sha1StrParser
    parseContributor name = do
        _ <- string name *> char ' '
        contribNameAndEmail <- BL.fromStrict <$> takeWhileIncluding (/= _greater)
        contribDate <- parseTimestamp
        pure Contributor{..}

    parseTimestamp = do
        _ <- skipSpace
        timestamp <- decimal
        _ <- skipSpace
        timeZome <- signed decimal
        pure $ posixSecondsToUTCTime (fromInteger $ timestamp + timeZome)

gitObjectParser :: Parser GitObject
gitObjectParser = treeParser <|> gitBlobParser <|> commitParser

gitContentToObject :: BL.ByteString -> Either ParseError GitObject
gitContentToObject = parseOnly gitObjectParser
