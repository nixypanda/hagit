module Utils (maybeToEither) where

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right
