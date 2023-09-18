{-# LANGUAGE OverloadedStrings #-}

module HTTPSmart (discoverGitServerCapabilities, HttpSmartError (..), lsRefs) where

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy as BL (ByteString)
import HTTPSmartCommand (Command (..), Ref, encodeCommand)
import HTTPSmartParse (gitServerCapabilitiesParser, lsResultParser)
import Network.HTTP.Client (Request (..), RequestBody (RequestBodyLBS), httpLbs, newManager, parseRequest, responseBody)
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

lsRefs :: String -> IO (Either HttpSmartError [Ref])
lsRefs url = do
    httpManager <- newManager tlsManagerSettings
    initReq <- parseRequest $ url <> "/git-upload-pack"
    let cmd = LsRefs [] ["peel", "unborn", "ref-prefix HEAD", "ref-prefix refs/heads/", "ref-prefix refs/tags/"]
        headers =
            [ ("git-protocol", "version=2")
            , ("content-type", "application/x-git-upload-pack-request")
            , ("accept", "application/x-git-upload-pack-result")
            ]
        request =
            initReq
                { method = "POST"
                , requestHeaders = headers
                , requestBody = RequestBodyLBS $ encodeCommand cmd
                }
    res <- httpLbs request httpManager
    let received = responseBody res
    pure $ first ParsingError $ lsResultParser received
