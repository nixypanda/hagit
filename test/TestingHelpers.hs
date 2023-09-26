{-# LANGUAGE ImportQualifiedPost #-}

module TestingHelpers (sha1String, sha1) where

import Crypto.Hash (Digest, digestFromByteString)
import Crypto.Hash.Algorithms (SHA1)
import Data.ByteString.Char8 qualified as BSC
import Data.Maybe (fromJust)

sha1String :: String
sha1String =
    [ '\131'
    , '\146'
    , '\209'
    , 'Y'
    , '\242'
    , '\231'
    , '\CAN'
    , '%'
    , '\STX'
    , '\DEL'
    , '\214'
    , '\175'
    , 'X'
    , 'c'
    , '\210'
    , '\CAN'
    , '\184'
    , '\181'
    , '\249'
    , '\203'
    ]

sha1 :: Digest SHA1
sha1 = fromJust $ digestFromByteString $ BSC.pack sha1String
