{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Object (GitObject (..), objBody, objCompressedContent, objContent, objHeader, objSha1, objSha1Str) where

import Codec.Compression.Zlib (compress)
import Crypto.Hash (Digest, SHA1, hashlazy)
import Data.ByteString.Lazy as BL (ByteString, length)
import Data.ByteString.Lazy.Char8 as BLC (pack)
import GHC.Generics (Generic)

data ObjectType = BlobType deriving (Generic)

instance Show ObjectType where
    show BlobType = "blob"

newtype GitObject = Blob BL.ByteString deriving (Show)

objBody :: GitObject -> BL.ByteString
objBody (Blob b) = b

objType :: GitObject -> ObjectType
objType (Blob _) = BlobType

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
