{-# LANGUAGE ImportQualifiedPost #-}

module Utils (
    maybeToEither,
    sha1ToByteString,
    liftIOEither,
    sha1ToHexByteString,
) where

import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA1)
import Data.ByteArray (convert)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

sha1ToByteString :: Digest SHA1 -> BL.ByteString
sha1ToByteString = BL.fromStrict . convert

sha1ToHexByteString :: Digest SHA1 -> BL.ByteString
sha1ToHexByteString = BLC.pack . show

liftIOEither :: (MonadIO m, MonadError e m) => IO (Either e a) -> m a
liftIOEither iea = do
    ea <- liftIO iea
    liftEither ea
