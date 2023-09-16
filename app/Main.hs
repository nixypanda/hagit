{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Options.Applicative (
    command,
    execParser,
    fullDesc,
    header,
    helper,
    long,
    metavar,
    progDesc,
    short,
    strArgument,
    subparser,
    switch,
    (<**>),
 )
import Options.Applicative.Builder (info)
import Options.Applicative.Types (Parser)

import Lib (CatFileOpts (..), Command (..), HashObjOpts (..), runCommand, runGitM)

main :: IO ()
main = runCmd =<< execParser opts
  where
    opts =
        info
            (commands <**> helper)
            ( fullDesc
                <> progDesc "A distributed VCS written in haskell"
                <> header "hagit - a haskell git clone"
            )

commands :: Parser Command
commands =
    subparser
        ( command "init" (info (pure Init) (progDesc "Initialize a git repository"))
            <> command "cat-file" (info (CatFile <$> catFileParser) (progDesc "Print the contents of a file"))
            <> command "hash-object" (info (HashObject <$> hashObjectParser) (progDesc "Hash an object"))
        )

catFileParser :: Parser CatFileOpts
catFileParser =
    CatFileOpts
        <$> switch (long "preview" <> short 'p')
        <*> strArgument (metavar "SHA1")

hashObjectParser :: Parser HashObjOpts
hashObjectParser =
    HashObjOpts
        <$> switch (long "write" <> short 'w')
        <*> strArgument (metavar "PATH")

runCmd :: Command -> IO ()
runCmd cmd = do
    result <- runExceptT $ runGitM $ runCommand cmd
    case result of
        Left err -> print err
        Right () -> pure ()
