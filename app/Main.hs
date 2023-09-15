module Main (main) where

import Options.Applicative (command, execParser, fullDesc, header, helper, progDesc, subparser, (<**>))
import Options.Applicative.Builder (info)
import Options.Applicative.Types (Parser)

import Lib

data Command = Init

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
commands = subparser (command "init" (info (pure Init) (progDesc "Initialize a git repository")))

runCommand :: Command -> IO ()
runCommand Init = initialize
