{-# LANGUAGE OverloadedStrings #-}

module HTTPSmart (discoverGitServerCapabilities, HttpSmartError (..)) where

import Data.ByteString.Lazy as BL (ByteString)
import HTTPSmartParse (gitServerCapabilitiesParser)
import Network.HTTP.Client (Request (..), httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (renderSimpleQuery)
import Text.Parsec (ParseError, parse)

data HttpSmartError
    = ParsingError ParseError
    | InvalidStatus Int Int
    deriving (Show)

discoverGitServerCapabilities :: String -> IO (Either HttpSmartError [BL.ByteString])
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
    res <- httpLbs request httpManager
    let received = responseBody res
    case parse gitServerCapabilitiesParser "" received of
        Left err -> pure $ Left $ ParsingError err
        Right caps -> pure $ Right caps
