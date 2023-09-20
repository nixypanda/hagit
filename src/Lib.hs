{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (
    GitError,
    GitM,
    Command (..),
    CatFileOpts (..),
    HashObjOpts (..),
    LsTreeOpts (..),
    CommitTreeOpts (..),
    CloneRepoOpts (..),
    runGitM,
    runCommand,
) where

import Codec.Compression.Zlib (decompress)
import Control.Monad (forM, unless)
import Control.Monad.Except (ExceptT, MonadError, liftEither, throwError, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA1)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Either (fromRight)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import HTTPSmart (HttpSmartError, discoverGitServerCapabilities, fetch, lsRefs)
import Object (
    GitObject (..),
    ObjectType (..),
    TreeEntry (..),
    createCommitObject,
    dirMode,
    entryBodyStr,
    entryNameStr,
    fileMode,
    objBody,
    objCompressedContent,
    objSha1,
    objSha1Str,
    objType,
 )
import ObjectParse (gitContentToObject)
import PackfileParsing (parsePackfile)
import ParsingUtils (ParseError, parseSHA1Str)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, (</>))
import System.IO (IOMode (..), hPutStrLn, withBinaryFile, withFile)
import System.IO.Error (tryIOError)
import Utils (liftIOEither)

data Command
    = Init
    | CatFile CatFileOpts
    | HashObject HashObjOpts
    | LsTree LsTreeOpts
    | WriteTree
    | CommitTree CommitTreeOpts
    | CloneRepo CloneRepoOpts

data GitError
    = InvalidSHA1 ParseError
    | GitContentParseError ParseError
    | UnexpectedObjectError ObjectType ObjectType
    | HttpSmartErr HttpSmartError
    | ServerCapabilitiesMismatch [BL.ByteString] [BL.ByteString]
    | IOErr IOError
    | DevTest
    deriving (Show)

newtype GitM a = GitM {runGitM :: ExceptT GitError IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError GitError)

gitLocation :: FilePath
gitLocation = "git_test"

-- Main runner

runCommand :: Command -> GitM ()
runCommand Init = initialize
runCommand (CatFile opts) = catFile opts
runCommand (HashObject opts) = hashObject opts
runCommand (LsTree opts) = lsTree opts
runCommand WriteTree = writeTree
runCommand (CommitTree opts) = commitTree opts
runCommand (CloneRepo opts) = cloneRepo opts

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
                else entryBodyStr
    liftIO $ Prelude.putStrLn $ Prelude.unlines $ fmap showFunc treeEntries

writeTree :: GitM ()
writeTree = do
    treeEntry <- writeTree' "."
    liftIO $ print (entrySha1 treeEntry)

writeTreeExclusionList :: [FilePath]
writeTreeExclusionList = [".git", "git_test", ".direnv", "tags", ".stack-work"]

writeTree' :: FilePath -> GitM TreeEntry
writeTree' basePath = do
    eitherFilepaths <- liftIO $ tryIOError $ listDirectory basePath
    filepaths <- liftEither $ first IOErr eitherFilepaths
    let filteredFilePaths = filter (`notElem` writeTreeExclusionList) filepaths
    let sortedFilePaths = sort filteredFilePaths

    treeEntries <- forM sortedFilePaths $ \filepath -> do
        let path = basePath </> filepath
        isDir <- liftIO $ doesDirectoryExist path
        if isDir
            then writeTree' path
            else do
                let doCreate = True
                gitObject <- hashObject' doCreate path
                let entryMode = fileMode
                    entryName = BLC.pack filepath
                    entrySha1 = objSha1 gitObject
                pure $ TreeEntry{..}

    let tree = Tree treeEntries
    writeObject tree

    let entryMode = dirMode
        entryName = BLC.pack $ takeBaseName basePath
        entrySha1 = objSha1 tree
    pure $ TreeEntry{..}

data CommitTreeOpts = CommitTreeOpts
    { treeShaOpt :: BL.ByteString
    , parentShaOpt :: Maybe BL.ByteString
    , commitMessageOpt :: BL.ByteString
    , commitAuthorOpt :: Maybe BL.ByteString
    }

defaultAuthor :: BL.ByteString
defaultAuthor = "Sparki Coco <sparki.coco@email.random>"

commitTree :: CommitTreeOpts -> GitM ()
commitTree CommitTreeOpts{..} = do
    treeSha1 <- liftEither $ first InvalidSHA1 $ parseSHA1Str treeShaOpt
    parentSha1 <- case parentShaOpt of
        Nothing -> pure Nothing
        Just pSha1 -> do
            pSha1' <- liftEither $ first InvalidSHA1 $ parseSHA1Str pSha1
            pure $ Just pSha1'
    let msg = commitMessageOpt
    let author = fromMaybe defaultAuthor commitAuthorOpt
    now <- liftIO getCurrentTime
    let commit = createCommitObject treeSha1 parentSha1 author msg now
    writeObject commit
    liftIO $ print $ objSha1Str commit

data CloneRepoOpts = CloneRepoOpts
    { repoUrlHTTPS :: String
    , repoLocalPath :: FilePath
    }

cloneRepo :: CloneRepoOpts -> GitM ()
cloneRepo CloneRepoOpts{..} = do
    -- Remove these two lines
    liftIO cloneTest
    _ <- throwError DevTest

    let expectedCapabilities = ["version 2", "object-format=sha1"]
    capabilities <-
        liftIOEither
            (first HttpSmartErr <$> discoverGitServerCapabilities repoUrlHTTPS)
    unless (all (`elem` capabilities) expectedCapabilities) $
        throwError $
            ServerCapabilitiesMismatch expectedCapabilities capabilities
    refs <- liftIOEither $ first HttpSmartErr <$> lsRefs repoUrlHTTPS
    packfile <- liftIO $ fetch repoUrlHTTPS refs
    liftIO $ BL.putStr (fromRight "" packfile)

cloneTest :: IO ()
cloneTest = do
    withBinaryFile "git-sample-1.pack" ReadMode $ \handle -> do
        content <- BL.hGetContents handle
        print $ parsePackfile content

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
