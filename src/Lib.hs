module Lib (
    initialize,
) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)

gitLocation :: FilePath
gitLocation = "git_test"

initialize :: IO ()
initialize = do
    let createParents = True
    createDirectoryIfMissing createParents (gitLocation </> "objects")
    createDirectoryIfMissing createParents (gitLocation </> "refs")
    withFile (gitLocation </> "HEAD") WriteMode $ \f -> do
        hPutStrLn f "ref: refs/heads/master"
    putStrLn "Initialized git directory"
