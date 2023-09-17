{-# LANGUAGE OverloadedStrings #-}

module HTTPSmart (discoverGitServerCapabilities) where

import Data.ByteString.Lazy as BL (ByteString)
import Network.HTTP.Client (Request (..), httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (renderSimpleQuery)

discoverGitServerCapabilities :: String -> IO BL.ByteString
discoverGitServerCapabilities url = do
    httpManager <- newManager tlsManagerSettings
    initReq <- parseRequest $ url <> "/info/refs"
    let params = [("service", "git-upload-pack")]
        headers =
            [ ("Accept", "application/x-git-upload-pack-advertisement")
            , ("git-protocol", "version=2")
            ]
        request =
            initReq
                { queryString = renderSimpleQuery True params
                , requestHeaders = headers
                }
    print request
    res <- httpLbs request httpManager
    pure $ responseBody res
