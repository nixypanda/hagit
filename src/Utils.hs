module Utils (maybeToEither, sha1ToByteString, liftIOEither) where

import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA1)
import Data.ByteArray (convert)
import Data.ByteString.Lazy as BL (ByteString, fromStrict)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

sha1ToByteString :: Digest SHA1 -> BL.ByteString
sha1ToByteString = BL.fromStrict . convert

liftIOEither :: (MonadIO m, MonadError e m) => IO (Either e a) -> m a
liftIOEither iea = do
    ea <- liftIO iea
    liftEither ea
