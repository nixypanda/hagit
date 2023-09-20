{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTPSmart (
    HttpSmartError (..),
    discoverGitServerCapabilities,
    lsRefs,
    fetch,
) where

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy qualified as BL
import HTTPSmartCommand (Command (..), Ref, encodeCommand, refsToFetch)
import HTTPSmartParse (fetchOutput, gitServerCapabilitiesParser, lsResultParser)
import Network.HTTP.Client (
    Request (..),
    RequestBody (RequestBodyLBS),
    httpLbs,
    newManager,
    parseUrlThrow,
    responseBody,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (renderSimpleQuery)
import ParsingUtils (ParseError)

data HttpSmartError
    = ParsingError ParseError
    | InvalidStatus Int Int
    deriving (Show)

discoverGitServerCapabilities :: String -> IO (Either HttpSmartError [BL.ByteString])
discoverGitServerCapabilities url = do
    httpManager <- newManager tlsManagerSettings
    initReq <- parseUrlThrow $ url <> "/info/refs"
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
    pure $ first ParsingError $ gitServerCapabilitiesParser received

lsRefs :: String -> IO (Either HttpSmartError [Ref])
lsRefs url = do
    httpManager <- newManager tlsManagerSettings
    initReq <- parseUrlThrow $ "POST " <> url <> "/git-upload-pack"
    let
        cmdArgs =
            [ "peel"
            , "unborn"
            , "ref-prefix HEAD"
            , "ref-prefix refs/heads/"
            , "ref-prefix refs/tags/"
            ]
        cmd = LsRefs ["object-format=sha1"] cmdArgs
        headers =
            [ ("git-protocol", "version=2")
            , ("content-type", "application/x-git-upload-pack-request")
            , ("accept", "application/x-git-upload-pack-result")
            ]
        request =
            initReq
                { requestHeaders = headers
                , requestBody = RequestBodyLBS $ encodeCommand cmd
                }
    res <- httpLbs request httpManager
    let received = responseBody res
    pure $ first ParsingError $ lsResultParser received

fetch :: String -> [Ref] -> IO (Either HttpSmartError BL.ByteString)
fetch url refs = do
    httpManager <- newManager tlsManagerSettings
    initReq <- parseUrlThrow $ "POST " <> url <> "/git-upload-pack"
    let cmd = refsToFetch ["object-format=sha1"] refs
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
    pure $ first ParsingError $ fetchOutput received
