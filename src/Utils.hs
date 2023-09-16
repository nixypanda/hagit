module Utils (maybeToEither, sha1ToByteString) where

import Crypto.Hash (Digest, SHA1)
import Data.ByteArray (convert)
import Data.ByteString.Lazy as BL (ByteString, fromStrict)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

sha1ToByteString :: Digest SHA1 -> BL.ByteString
sha1ToByteString = BL.fromStrict . convert
