{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Object (
    GitObject (..),
    TreeEntry (..),
    ObjectType (..),
    CommitInner (..),
    Contributor (..),
    objBody,
    objBodyLen,
    objCompressedContent,
    objContent,
    objHeader,
    objSha1,
    objSha1Str,
    objType,
    entryBodyStr,
    entryNameStr,
    fileMode,
    dirMode,
    createCommitObject,
) where

import Codec.Compression.Zlib (compress)
import Crypto.Hash (Digest, SHA1, hashlazy)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Time (ZonedTime, formatTime, zonedTimeToUTC)
import Data.Time.Format (defaultTimeLocale)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Utils (sha1ToByteString, sha1ToHexByteString)

data ObjectType
    = BlobType
    | TreeType
    | CommitType
    deriving (Generic)

instance Show ObjectType where
    show BlobType = "blob"
    show TreeType = "tree"
    show CommitType = "commit"

data GitObject
    = Blob BL.ByteString
    | Tree [TreeEntry]
    | Commit CommitInner
    deriving (Show, Eq)

data CommitInner = CommitInner
    { treeSha1 :: Digest SHA1
    , parentSha1 :: Maybe (Digest SHA1)
    , commitMessage :: BL.ByteString
    , commitAuthor :: Contributor
    , commitCommitter :: Contributor
    }
    deriving (Show, Eq)

data Contributor = Contributor
    { contribNameAndEmail :: BL.ByteString
    , contribDate :: ZonedTime
    }
    deriving (Show)

instance Eq Contributor where
    a == b = contribNameAndEmail a == contribNameAndEmail b && utcTime a == utcTime b
      where
        utcTime = zonedTimeToUTC . contribDate

objBody :: GitObject -> BL.ByteString
objBody (Blob b) = b
objBody (Tree entries) = mconcat $ fmap entryBody entries
objBody (Commit commitInner) = commitBody commitInner

objType :: GitObject -> ObjectType
objType (Blob _) = BlobType
objType (Tree _) = TreeType
objType (Commit _) = CommitType

objContent :: GitObject -> BL.ByteString
objContent obj = objHeader obj <> "\0" <> objBody obj

objBodyLen :: GitObject -> Int
objBodyLen = fromIntegral . BL.length . objBody

objHeader :: GitObject -> BL.ByteString
objHeader obj = BLC.pack $ show (objType obj) <> " " <> show (objBodyLen obj)

objCompressedContent :: GitObject -> BL.ByteString
objCompressedContent = compress . objContent

objSha1 :: GitObject -> Digest SHA1
objSha1 = hashlazy . objContent

objSha1Str :: GitObject -> String
objSha1Str = show . objSha1

data TreeEntry = TreeEntry
    { entryMode :: BL.ByteString
    , entryName :: BL.ByteString
    , entrySha1 :: Digest SHA1
    }
    deriving (Show, Eq)

fileMode, dirMode :: BL.ByteString
fileMode = "100644"
dirMode = "40000"

entryBody :: TreeEntry -> BL.ByteString
entryBody TreeEntry{..} =
    entryMode
        <> " "
        <> entryName
        <> "\0"
        <> sha1ToByteString entrySha1

entryBodyStr :: TreeEntry -> String
entryBodyStr TreeEntry{..} =
    printf "%6s " (BLC.unpack entryMode)
        <> " "
        <> show entrySha1
        <> " "
        <> BLC.unpack entryName

entryNameStr :: TreeEntry -> String
entryNameStr = BLC.unpack . entryName

createCommitObject ::
    Digest SHA1 ->
    Maybe (Digest SHA1) ->
    BL.ByteString ->
    BL.ByteString ->
    ZonedTime ->
    GitObject
createCommitObject treeSha1 parentSha1 authorNameAndEmail commitMessage commitTime = do
    let commitAuthor = Contributor authorNameAndEmail commitTime
        commitCommitter = commitAuthor
    Commit $ CommitInner{..}

commitBody :: CommitInner -> BL.ByteString
commitBody commit =
    let contribStr :: String -> Contributor -> String
        contribStr c Contributor{..} =
            printf
                "%s %s %s"
                c
                (BLC.unpack contribNameAndEmail)
                (formatTime defaultTimeLocale "%s %z" contribDate)
        parent = case parentSha1 commit of
            Just sha -> "parent " <> sha1ToHexByteString sha <> "\n"
            Nothing -> ""
     in "tree "
            <> sha1ToHexByteString (treeSha1 commit)
            <> "\n"
            <> parent
            <> BLC.pack (contribStr "author" (commitAuthor commit))
            <> "\n"
            <> BLC.pack (contribStr "committer" (commitCommitter commit))
            <> "\n"
            <> "\n"
            <> commitMessage commit
