{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC (pack)
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
