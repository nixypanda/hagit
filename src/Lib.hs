{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (
    GitError,
    GitM,
    Command (..),
    CatFileOpts (..),
    HashObjOpts (..),
    LsTreeOpts (..),
    runGitM,
    runCommand,
) where

import Codec.Compression.Zlib (decompress)
import Control.Monad.Except (ExceptT, MonadError, liftEither, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA1)
import Data.Bifunctor (first)
import Data.ByteString.Lazy as BL (
    ByteString,
    putStr,
    readFile,
    writeFile,
 )
import Object (
    GitObject (..),
    ObjectType (..),
    entryBody,
    entryNameStr,
    objBody,
    objCompressedContent,
    objSha1,
    objSha1Str,
    objType,
 )
import ObjectParse (gitContentToObject, parseSHA1Str)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import System.IO.Error (tryIOError)
import Text.Parsec (ParseError)

data Command
    = Init
    | CatFile CatFileOpts
    | HashObject HashObjOpts
    | LsTree LsTreeOpts

data GitError
    = InvalidSHA1 ParseError
    | GitContentParseError ParseError
    | UnexpectedObjectError ObjectType ObjectType
    | IOErr IOError
    deriving (Show)

newtype GitM a = GitM {runGitM :: ExceptT GitError IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError GitError)

gitLocation :: FilePath
gitLocation = ".git"

-- Main runner

runCommand :: Command -> GitM ()
runCommand Init = initialize
runCommand (CatFile opts) = catFile opts
runCommand (HashObject opts) = hashObject opts
runCommand (LsTree opts) = lsTree opts

-- Commands
initialize :: GitM ()
initialize = do
    let createParents = True
    liftIO $ createDirectoryIfMissing createParents (gitLocation </> "objects")
    liftIO $ createDirectoryIfMissing createParents (gitLocation </> "refs")
    liftIO $ withFile (gitLocation </> "HEAD") WriteMode $ \f -> do
        hPutStrLn f "ref: refs/heads/master"
    liftIO $ putStrLn "Initialized git directory"

data CatFileOpts = CatFileOpts
    { preview :: Bool
    , sha1Str :: BL.ByteString
    }

catFile :: CatFileOpts -> GitM ()
catFile CatFileOpts{..} = do
    sha1 <- liftEither $ first InvalidSHA1 $ parseSHA1Str sha1Str
    gitObjectFileContent <- readContentFromSHA1Code sha1
    gitObject <- gitContentToObject' gitObjectFileContent
    liftIO $ BL.putStr (objBody gitObject)

data HashObjOpts = HashObjOpts
    { write :: Bool
    , filePath :: FilePath
    }

hashObject :: HashObjOpts -> GitM ()
hashObject HashObjOpts{..} = do
    obj <- hashObject' write filePath
    liftIO $ Prelude.putStr (objSha1Str obj)

data LsTreeOpts = LsTreeOpts
    { nameOnly :: Bool
    , treeSha :: BL.ByteString
    }

lsTree :: LsTreeOpts -> GitM ()
lsTree LsTreeOpts{..} = do
    sha1 <- liftEither $ first InvalidSHA1 $ parseSHA1Str treeSha
    gitObjectFileContent <- readContentFromSHA1Code sha1
    gitObject <- gitContentToObject' gitObjectFileContent
    treeEntries <- liftEither $ case gitObject of
        Tree entries -> pure entries
        gitObj -> Left $ UnexpectedObjectError TreeType (objType gitObj)
    let showFunc =
            if nameOnly
                then entryNameStr
                else entryBody
    liftIO $ Prelude.putStrLn $ Prelude.unlines $ fmap showFunc treeEntries

-- Helpers

readContentFromSHA1Code :: Digest SHA1 -> GitM BL.ByteString
readContentFromSHA1Code sha1 = do
    let (objectDir, objectFilename) = sha1ToDirAndFilename sha1
        filePath = gitLocation </> "objects" </> objectDir </> objectFilename
    content <- liftIO $ tryIOError $ BL.readFile filePath
    liftEither (decompress <$> first IOErr content)

writeObject :: GitObject -> GitM ()
writeObject obj = do
    let (dir, filename) = sha1ToDirAndFilename (objSha1 obj)
        objectDirPath = gitLocation </> "objects" </> dir
        objectFilePath = objectDirPath </> filename
        createParents = True

    liftIO $ createDirectoryIfMissing createParents objectDirPath
    liftIO $ BL.writeFile objectFilePath (objCompressedContent obj)

hashObject' :: Bool -> FilePath -> GitM GitObject
hashObject' doWrite filePath = do
    content <- liftIO $ BL.readFile filePath
    let blob = Blob content
    when doWrite $
        writeObject blob
    pure blob

gitContentToObject' :: BL.ByteString -> GitM GitObject
gitContentToObject' = liftEither . first GitContentParseError . gitContentToObject

-- SHA-1 to directory and filename
sha1ToDirAndFilename :: Digest SHA1 -> (String, String)
sha1ToDirAndFilename = splitAt 2 . show
