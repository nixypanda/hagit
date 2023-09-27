{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Monad (forM, forM_, unless)
import Control.Monad.Except (ExceptT, MonadError, liftEither, throwError, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA1)
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Time (getZonedTime)
import HTTPSmart (HttpSmartError, discoverGitServerCapabilities, fetch, lsRefs)
import HTTPSmartCommand (Ref (..))
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
    objContent,
    objSha1,
    objSha1Str,
    treeSha1,
 )
import ObjectParse (blobParser, commitParser, gitContentToObject, treeParser)
import PackfileParsing (parsePackfile)
import ParsingUtils (ParseError, parseSHA1Str)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    setCurrentDirectory,
 )
import System.FilePath (takeBaseName, (</>))
import System.IO (IOMode (..), hPutStrLn, withFile)
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
    | InvalidGitContent ParseError
    | PackfileParseError ParseError
    | UnexpectedObjectError ObjectType ObjectType
    | HttpSmartErr HttpSmartError
    | ServerCapabilitiesMismatch [BL.ByteString] [BL.ByteString]
    | IOErr IOError
    | DevTest
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
runCommand WriteTree = writeTree
runCommand (CommitTree opts) = commitTree opts
runCommand (CloneRepo opts) = cloneRepo opts

-- Commands
initialize :: GitM ()
initialize = liftIO $ do
    let createParents = True
    createDirectoryIfMissing createParents (gitLocation </> "objects")
    createDirectoryIfMissing createParents (gitLocation </> "refs")
    withFile (gitLocation </> "HEAD") WriteMode $ \f -> do
        hPutStrLn f "ref: refs/heads/master"
    putStrLn "Initialized git directory"

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
    treeEntries <- liftEither $ first InvalidGitContent $ parseOnly treeParser gitObjectFileContent
    let showFunc = if nameOnly then entryNameStr else entryBodyStr
    liftIO $ Prelude.putStrLn $ Prelude.unlines $ fmap showFunc treeEntries

writeTree :: GitM ()
writeTree = do
    treeEntry <- writeTree' "."
    liftIO $ print (entrySha1 treeEntry)

gitignore :: [FilePath]
gitignore = [".git", "git_test", ".direnv", "tags", ".stack-work"]

writeTree' :: FilePath -> GitM TreeEntry
writeTree' basePath = do
    filepaths <- liftIO $ listDirectory basePath
    let filteredFilePaths = sort $ filter (`notElem` gitignore) filepaths

    treeEntries <- forM filteredFilePaths $ \filepath -> do
        let path = basePath </> filepath
        isDir <- liftIO $ doesDirectoryExist path
        if isDir
            then writeTree' path
            else fromFile filepath <$> hashObject' True path

    let tree = Tree treeEntries
    writeObject tree
    pure $ fromDir basePath tree
  where
    fromFile filepath gitObject =
        TreeEntry fileMode (BLC.pack filepath) (objSha1 gitObject)
    fromDir dirPath gitObject =
        TreeEntry dirMode (BLC.pack $ takeBaseName dirPath) (objSha1 gitObject)

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
    now <- liftIO getZonedTime
    let commit = createCommitObject treeSha1 parentSha1 author msg now
    writeObject commit
    liftIO $ print $ objSha1Str commit

data CloneRepoOpts = CloneRepoOpts
    { repoUrlHTTPS :: String
    , repoLocalPath :: FilePath
    }

cloneRepo :: CloneRepoOpts -> GitM ()
cloneRepo CloneRepoOpts{..} = do
    liftIO $ createDirectoryIfMissing True repoLocalPath
    liftIO $ setCurrentDirectory repoLocalPath
    initialize

    (gitObjects, headSha1) <- getObjectsFromServer repoUrlHTTPS
    forM_ gitObjects $ do
        writeObject

    liftIO $ print headSha1
    -- We are not handling refs, hence this needs an input
    headTree <- getLatesetTree headSha1

    checkoutTreeAt headTree ""

getLatesetTree :: Digest SHA1 -> GitM (Digest SHA1)
getLatesetTree headSha1 = do
    headCommitStr <- readContentFromSHA1Code headSha1
    headCommit <- liftEither $ first InvalidGitContent $ parseOnly commitParser headCommitStr
    pure $ treeSha1 headCommit

getObjectsFromServer :: String -> GitM ([GitObject], Digest SHA1)
getObjectsFromServer repoUrlHTTPS = do
    let expectedCapabilities = ["version 2", "object-format=sha1"]
    capabilities <- liftIOEither (first HttpSmartErr <$> discoverGitServerCapabilities repoUrlHTTPS)
    unless (all (`elem` capabilities) expectedCapabilities) $
        throwError (ServerCapabilitiesMismatch expectedCapabilities capabilities)
    refs <- liftIOEither $ first HttpSmartErr <$> lsRefs repoUrlHTTPS
    liftIO $ print refs
    packfile <- liftIOEither $ first HttpSmartErr <$> fetch repoUrlHTTPS refs
    let headSha1 = getHead refs
    gitObjects <- liftEither $ first PackfileParseError $ parsePackfile packfile
    pure (gitObjects, headSha1)

checkoutTreeAt :: Digest SHA1 -> FilePath -> GitM ()
checkoutTreeAt treeSha path = do
    treeContents <- readContentFromSHA1Code treeSha
    treeObjectEntries <-
        liftEither $ first InvalidGitContent $ parseOnly treeParser treeContents
    forM_ treeObjectEntries $ \treeEntry -> do
        let filePath = path </> entryNameStr treeEntry
        if
                | entryMode treeEntry == fileMode -> do
                    blobContents <- readContentFromSHA1Code (entrySha1 treeEntry)
                    blob <- liftEither $ first InvalidGitContent $ parseOnly blobParser blobContents
                    liftIO $ BL.writeFile filePath (objContent $ Blob blob)
                | entryMode treeEntry == dirMode -> do
                    liftIO $ createDirectoryIfMissing True filePath
                    checkoutTreeAt (entrySha1 treeEntry) filePath
                | otherwise ->
                    throwError $ InvalidGitContent "not a file or directory"

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
gitContentToObject' = liftEither . first InvalidGitContent . gitContentToObject

-- SHA-1 to directory and filename
sha1ToDirAndFilename :: Digest SHA1 -> (String, String)
sha1ToDirAndFilename = splitAt 2 . show

getHead :: [Ref] -> Digest SHA1
getHead = refSha1 . head . filter ((== "HEAD") . refName)
