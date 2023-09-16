{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (
    initialize,
    catFile,
    GitError,
    GitM,
    runGitM,
) where

import Codec.Compression.Zlib (decompress)
import Control.Monad.Except (ExceptT, MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.ByteString.Lazy as BL (
    ByteString,
    putStr,
    readFile,
 )
import Object (GitObject, body)
import ObjectParse (gitContentToObject)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import System.IO.Error (tryIOError)
import Text.Parsec (ParseError)

data GitError = GitContentParseError ParseError | IOErr IOError deriving (Show)

newtype GitM a = GitM {runGitM :: ExceptT GitError IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError GitError)

gitLocation :: FilePath
gitLocation = ".git"

-- Commands
initialize :: GitM ()
initialize = do
    let createParents = True
    liftIO $ createDirectoryIfMissing createParents (gitLocation </> "objects")
    liftIO $ createDirectoryIfMissing createParents (gitLocation </> "refs")
    liftIO $ withFile (gitLocation </> "HEAD") WriteMode $ \f -> do
        hPutStrLn f "ref: refs/heads/master"
    liftIO $ putStrLn "Initialized git directory"

catFile :: String -> GitM ()
catFile sha1 = do
    gitObjectFileContent <- readContentFromSHA1Code sha1
    gitObject <- gitContentToObject' gitObjectFileContent
    liftIO $ BL.putStr (body gitObject)

-- Helpers

readContentFromSHA1Code :: String -> GitM BL.ByteString
readContentFromSHA1Code sha1 = do
    let (objectDir, objectFilename) = sha1ToDirAndFilename sha1
        filePath = gitLocation </> "objects" </> objectDir </> objectFilename
    content <- liftIO $ tryIOError $ BL.readFile filePath
    liftEither (decompress <$> first IOErr content)

gitContentToObject' :: BL.ByteString -> GitM GitObject
gitContentToObject' = liftEither . first GitContentParseError . gitContentToObject

-- SHA-1 to directory and filename
sha1ToDirAndFilename :: String -> (String, String)
sha1ToDirAndFilename = splitAt 2
