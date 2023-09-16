{-# LANGUAGE DeriveGeneric #-}

module Object (GitObject (..), body) where

import Data.Binary as Bin (Binary)
import Data.ByteString.Lazy as BL (ByteString)
import GHC.Generics (Generic)

data ObjectType = BlobType deriving (Generic)

instance Binary ObjectType

newtype GitObject = Blob BL.ByteString

body :: GitObject -> BL.ByteString
body (Blob b) = b
