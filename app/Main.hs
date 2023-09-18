module Main (main) where

import Control.Monad.Except (runExceptT)
import Options.Applicative (
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    long,
    metavar,
    optional,
    progDesc,
    short,
    strArgument,
    strOption,
    subparser,
    switch,
    (<**>),
 )
import Options.Applicative.Builder (info)
import Options.Applicative.Types (Parser)

import Lib (
    CatFileOpts (..),
    CloneRepoOpts (..),
    Command (..),
    CommitTreeOpts (..),
    HashObjOpts (..),
    LsTreeOpts (..),
    runCommand,
    runGitM,
 )

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
            <> command
                "cat-file"
                (info (CatFile <$> catFileParser) (progDesc "Print contents of a file"))
            <> command
                "hash-object"
                (info (HashObject <$> hashObjectParser) (progDesc "Hash an object"))
            <> command
                "ls-tree"
                (info (LsTree <$> lsTreeParser) (progDesc "List contents of a tree"))
            <> command
                "write-tree"
                (info (pure WriteTree) (progDesc "Write contents of cwd to a tree"))
            <> command
                "commit-tree"
                (info (CommitTree <$> commitTreeParser) (progDesc "Commit a tree"))
            <> command
                "clone"
                (info (CloneRepo <$> cloneRepoParser) (progDesc "Clone a git repo"))
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

lsTreeParser :: Parser LsTreeOpts
lsTreeParser =
    LsTreeOpts
        <$> switch (long "name-only" <> short 'n')
        <*> strArgument (metavar "PATH")

commitTreeParser :: Parser CommitTreeOpts
commitTreeParser =
    CommitTreeOpts
        <$> strArgument (metavar "TREE_SHA1" <> help "SHA1 of the tree to commit")
        <*> optional
            ( strOption
                ( short 'p'
                    <> long "parent"
                    <> metavar "PARENT_SHA1"
                    <> help "SHA1 of parent commit (if any)"
                )
            )
        <*> strOption
            (short 'm' <> long "message" <> metavar "MESSAGE" <> help "Commit message")
        <*> optional
            ( strOption
                ( short 'a'
                    <> long "author"
                    <> metavar
                        "NAME_AND_EMAIL"
                    <> help
                        "author name followed by email `Author Name <author@email.eg>`"
                )
            )

cloneRepoParser :: Parser CloneRepoOpts
cloneRepoParser =
    CloneRepoOpts
        <$> strArgument
            (metavar "REPO_URL" <> help "Repository URL (only HTTPS is supported)")
        <*> strArgument (metavar "REPO_LOCAL_PATH" <> help "Local path to clone to")

runCmd :: Command -> IO ()
runCmd cmd = do
    result <- runExceptT $ runGitM $ runCommand cmd
    case result of
        Left err -> print err
        Right () -> pure ()
