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

import Data.ByteString.Lazy as BL (ByteString)
import Lib (GitM, catFile, initialize, runGitM)

data Command = Init | CatFile CatFileOpts

data CatFileOpts = CatFileOpts
    { preview :: Bool
    , sha1 :: BL.ByteString
    }

main :: IO ()
main = runCommand =<< execParser opts
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
        )

catFileParser :: Parser CatFileOpts
catFileParser =
    CatFileOpts
        <$> switch (long "preview" <> short 'p')
        <*> strArgument (metavar "SHA1")

runCommand :: Command -> IO ()
runCommand cmd = do
    result <- runExceptT $ runGitM $ runCommand' cmd
    case result of
        Left err -> print err
        Right () -> pure ()

runCommand' :: Command -> GitM ()
runCommand' Init = initialize
runCommand' (CatFile CatFileOpts{..}) = catFile sha1
