{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Object (
    GitObject (..),
    TreeEntry (..),
    ObjectType (..),
    objBody,
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
import Data.Time (UTCTime, formatTime)
import Data.Time.Format (defaultTimeLocale)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Utils (sha1ToByteString)

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
    { treeSha :: Digest SHA1
    , parentSha :: Maybe (Digest SHA1)
    , commitMessage :: BL.ByteString
    , authorInfo :: BL.ByteString
    , commitTime :: UTCTime
    }
    deriving (Show, Eq)

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

objHeader :: GitObject -> BL.ByteString
objHeader obj = BLC.pack $ show (objType obj) <> " " <> show (BL.length (objBody obj))

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
    UTCTime ->
    GitObject
createCommitObject treeSha parentSha authorInfo commitMessage commitTime = do
    Commit $ CommitInner{..}

commitBody :: CommitInner -> BL.ByteString
commitBody commit =
    let author' = authorInfo commit
        parent = case parentSha commit of
            Just sha -> "parent " <> sha1ToByteString sha <> "\n"
            Nothing -> ""
     in "tree "
            <> sha1ToByteString (treeSha commit)
            <> "\n"
            <> parent
            <> "author "
            <> author'
            <> " "
            <> BLC.pack (formatTime defaultTimeLocale "%s" (commitTime commit))
            <> " +0000\n\n"
            <> commitMessage commit
