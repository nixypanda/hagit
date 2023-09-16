{-# LANGUAGE DeriveGeneric #-}
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
) where

import Codec.Compression.Zlib (compress)
import Crypto.Hash (Digest, SHA1, hashlazy)
import Data.ByteString.Lazy as BL (ByteString, length)
import Data.ByteString.Lazy.Char8 as BLC (pack)
import Data.ByteString.Lazy.UTF8 as BLU (toString)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Utils (sha1ToByteString)

data ObjectType
    = BlobType
    | TreeType
    deriving (Generic)

instance Show ObjectType where
    show BlobType = "blob"
    show TreeType = "tree"

data GitObject
    = Blob BL.ByteString
    | Tree [TreeEntry]
    deriving (Show)

objBody :: GitObject -> BL.ByteString
objBody (Blob b) = b
objBody (Tree entries) = mconcat $ fmap entryBody entries

objType :: GitObject -> ObjectType
objType (Blob _) = BlobType
objType (Tree _) = TreeType

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
    printf "%6s " (BLU.toString entryMode)
        <> " "
        <> show entrySha1
        <> " "
        <> BLU.toString entryName

entryNameStr :: TreeEntry -> String
entryNameStr = BLU.toString . entryName
