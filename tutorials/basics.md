# Basics

In this section of the tutorial we will cover the basics of git, at the end of
this section we will have a working toy-clone of git that you can use as a VCS
locally if you don't mind a crappy user interface. Let's begin.

## `git init`

Following is the bare minimum to get a git repository working -

```text
- .git/
  - objects/
  - refs/
  - HEAD (should contain "ref: refs/heads/master\n")
```

We will not go beyond it. But let's try to understand what is going on here.

- `objects`: Git’s internal warehouse of blobs, all indexed by their SHA. This
  (dealing with git objects) is what we will spend most of our time working
  with.
- `refs`: The master copy of all refs that live in your repository, be they for
  stashes, tags, remote tracking branches, or local branches. We will not worry
  about this a lot in this article.
- `HEAD`: The current `ref` that you’re looking at. In most cases it’s probably
  `refs/heads/master`.

Now take a break and explore what is in these directories for yourself. Just
create a simple repository and get digging. Create a bare git repository, using
`git init` and then list the files in the `.git` directory. You may see more
files than what we have listed here, but as we can get by just creating these,
that is what we will do when implementing.

### Implementing `git init`

Let's start by introducing the `Git` `newtype` wrapper that we will be dealing
with mostly.

```haskell
newtype GitM a = GitM {runGitM :: ExceptT GitError IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError GitError)
```

Basically we want to do loads of IO and error handling, so we define this
`newtype` which sets up the expectations clearly.

As mentioned earlier, I will omit a lot of code from the implementation in this
article to focus on the gist of git's internals and important bits of
implementation. Some things that I will omit include

- Argument parsing
- Basic helpers
- wiring logic
- imports
- etc

Now with that out of the way, let's look at how we will achieve this using
Haskell.

```haskell
initialize :: GitM ()
initialize = liftIO $ do
    let createParents = True
    createDirectoryIfMissing createParents (gitLocation </> "objects")
    createDirectoryIfMissing createParents (gitLocation </> "refs")
    withFile (gitLocation </> "HEAD") WriteMode $ \f -> do
        hPutStrLn f "ref: refs/heads/master"
    putStrLn "Initialized git directory"

gitLocation :: FilePath
gitLocation = ".git"
```

If you are not familiar with Haskell this should pretty much look like
pseudocode which does not really require a lot of explanation. Still I will
cover some basics.

- `</>` is an operator which joins two paths together and inserts a `/` between
  them. (Operators in Haskell are just functions that you use in infix form)
- `liftIO` has type `MonadIO m => IO a -> m a` which basically says "lift IO
  into this monad m which is an instance of MonadIO typeclass". In our case
  `GitM` is.

## `git cat-file`

In this article we will deal with 3 `git objects`

- `blob`
- `tree`
- `commit`

Starting with `blob`s.

Let's start by understanding a bit about git.

Git at its core is a key-value store on your filesystem with a VCS interface on
top of it. What this means is that you can insert any kind of content into a Git
repository, for which Git will hand you back a unique key you can use later to
retrieve that content. To achieve this we look at a plumbing command
`hash-object`.

> Note: Plumbing and Porcelain (from git docs) But because Git was initially a
> toolkit for a version control system rather than a full user-friendly VCS, it
> has a lot of subcommands that do low-level work and were designed to be
> chained together UNIX-style or called from scripts. These commands are
> generally referred to as Git’s “plumbing” commands, while the more
> user-friendly commands are called “porcelain” commands.

```sh
> echo 'I am digging into git internals' | git hash-object --stdin
2ebd10597697df40eb7a514e06731b951ac5086c
```

With this command we get back a hash for our content. This hash is SHA1 of the
content. This command will not create anything, to create we need to pass the
`-w` flag to `git hash-object`. Afterward try listing the contents of the
directory `.git/objects`. What do you see?

```nushell
╭───┬────────────────────────────────────────────────────────┬──────┬──────┬────────────────╮
│ # │                          name                          │ type │ size │    modified    │
├───┼────────────────────────────────────────────────────────┼──────┼──────┼────────────────┤
│ 0 │ .git/objects/2e                                        │ dir  │ 96 B │ 12 seconds ago │
│ 1 │ .git/objects/2e/bd10597697df40eb7a514e06731b951ac5086c │ file │ 44 B │ 12 seconds ago │
│ 2 │ .git/objects/info                                      │ dir  │ 64 B │ 9 minutes ago  │
│ 3 │ .git/objects/pack                                      │ dir  │ 64 B │ 9 minutes ago  │
╰───┴────────────────────────────────────────────────────────┴──────┴──────┴────────────────╯
```

As we can see, this command used the SHA1 it showed us and used the first two
chars as directory name and the rest as the file name. Care to guess what is in
the file? That is where our command `cat-file` comes in. Let's try it out.

```sh
> git cat-file -p  2ebd10597697df40eb7a514e06731b951ac5086c
I am digging into git internals
```

My hope is that, now the initial statement about "git being a key-value store
makes more sense". Before we get to implementing it let's try to `cat` the
contents of the file.

```sh
> cat  .git/objects/2e/bd10597697df40eb7a514e06731b951ac5086c
<binary content>
```

Hmm, that does not look like anything sensible. That's because git is
compressing the data that it stores, and it uses `zlib` to compress it. To
implement `cat-file` we need to decompress it and then print it to the screen.
Let's write a quick python script to see what the decompressed data looks like.

```python
import zlib
with open('.git/objects/2e/bd10597697df40eb7a514e06731b951ac5086c', 'rb') as f:
    data = f.read()
print(zlib.decompress(data))
```

This is the output of this script.

```sh
b'blob 31\x00I am digging into git internals'
```

We have a string "blob" followed by a space, then the length of the content we
used `has-object` on, followed by `\x00` and then the string "I am digging into
git internals". Basically we have some header info followed by our content. With
this we have everything we need to implement `cat-file`.

### Implementing `git cat-file`

Now that we know what `git cat-file` does, let's implement it.

```haskell
catFile :: CatFileOpts -> GitM ()
catFile CatFileOpts{..} = do
    sha1 <- liftEither $ first InvalidSHA1 $ parseSHA1Str sha1Str
    gitObjectFileContent <- readContentFromSHA1Code sha1
    gitObject <- gitContentToObject' gitObjectFileContent
    liftIO $ BL.putStr (objBody gitObject)
```

As you can see the flow is pretty straightforward.

- We parse the SHA1 from the command line
- read the content from the code
- turn it into an internal representation
- extract out the data we want to show.

The internal representation that we have is simply the following, at the moment.

```haskell
data GitObject = Blob BL.ByteString
```

The read content part is essentially just reading the file and decompressing it.

```haskell
readContentFromSHA1Code :: Digest SHA1 -> GitM BL.ByteString
readContentFromSHA1Code sha1 = do
    let (objectDir, objectFilename) = sha1ToDirAndFilename sha1
        filePath = gitLocation </> "objects" </> objectDir </> objectFilename
    content <- liftIO $ tryIOError $ BL.readFile filePath
    liftEither (decompress <$> first IOErr content)

sha1ToDirAndFilename :: Digest SHA1 -> (String, String)
sha1ToDirAndFilename = splitAt 2 . show
```

Turning into internal representation is essentially just a wrapper over the
parsing logic. Which is as follows for the blob.

```haskell
blobParser :: Parser BL.ByteString
blobParser = do
    _ <- string "blob "
    len <- decimal
    _ <- char '\0'
    blobParser' len

blobParser' :: Int -> Parser BL.ByteString
blobParser' n = BL.fromStrict <$> take n

gitContentToObject :: BL.ByteString -> Either ParseError GitObject
gitContentToObject = parseOnly gitObjectParser
```

If you are not familiar with Haskell the `blobParser` should still be
straightforward to understand.

- We match the string `"blob "`
- get the length `n`
- read the `\0` char
- take next `n` characters

## `git hash-object`

This we have already discussed at length in the previous section. Let's jump
straight into its implementation.

### Implementing `git hash-object`

The core logic is

- read the file (from the given filepath)
- get sha1 of its content
- compress the content (prepending the header info)
- store this compressed data in the location that we got from the sha1

```haskell
data HashObjOpts = HashObjOpts
    { write :: Bool
    , filePath :: FilePath
    }

hashObject :: HashObjOpts -> GitM ()
hashObject HashObjOpts{..} = do
    obj <- hashObject' write filePath
    liftIO $ Prelude.putStr (objSha1Str obj)

hashObject' :: Bool -> FilePath -> GitM GitObject
hashObject' doWrite filePath = do
    content <- liftIO $ BL.readFile filePath
    let blob = Blob content
    when doWrite $
        writeObject blob
    pure blob

writeObject :: GitObject -> GitM ()
writeObject obj = liftIO $ do
    let (dir, filename) = sha1ToDirAndFilename (objSha1 obj)
        objectDirPath = gitLocation </> "objects" </> dir
        objectFilePath = objectDirPath </> filename
        createParents = True

    createDirectoryIfMissing createParents objectDirPath
    BL.writeFile objectFilePath (objCompressedContent obj)
```

`objCompressedContent` is what creates the actual data that we will compress,
the logic for that is as follows.

```haskell
objCompressedContent :: GitObject -> BL.ByteString
objCompressedContent = compress . objContent

objContent :: GitObject -> BL.ByteString
objContent obj = objHeader obj <> "\0" <> objBody obj

objHeader :: GitObject -> BL.ByteString
objHeader obj = BLC.pack $ show (objType obj) <> " " <> show (objBodyLen obj)
  where
    objBodyLen = BL.length . objBody

objBody :: GitObject -> BL.ByteString
objBody (Blob b) = b

objType :: GitObject -> ObjectType
objType (Blob _) = BlobType

```

## `git ls-tree`

Before implementing this command let's look at what its doing. In our test
directory (which we created earlier) just create some files and directories and
then do a `git commit`.

```nushell
> echo "# hagit" | save Readme.md
> echo "Do whatever" | save LICENSE
> mkdir src
> echo 'main = putStrLn "Hello git enthusiasts"' | save src/Main.hs
> git add .
> git commit -m "Initial Commit"
```

After this setup if we try to do the following

```sh
> git cat-file -p main^{tree} # Depending on your shell you might have to quote main^{tree}
```

or

```sh
> git ls-tree -p main^{tree}
```

we should see something like this

```sh
100644 blob 4fdab927deefcb7fc2c3c0fb41ad58fbca051445    LICENSE
100644 blob 6859d05f4fc0253a3fe97aeaeeba1eec60a550b8    Readme.md
040000 tree f552fe22de6977828edd3567472e5edc3a4aeed2    src
```

Try executing `cat-file` and `ls-tree` on these sha1 hashes. What happens?

Conceptually we can think of the way git stores your data as a `Tree`

```text

                        ┌──────┐
         ┌──────────────┤ tree ├──────────────┐
         │              └──┬───┘              │
         │                 │                  │
         │                 │                  │
         │                 │                  │
  ┌──────┴─────┐     ┌─────┴───────┐     ┌────┴────┐
  │            │     │             │     │         │
  │  LICENSE   │     │  Readme.md  │     │  src    │
  │   (blob)   │     │   (blob)    │     │ (tree)  │
  │            │     │             │     │         │
  └────────────┘     └─────────────┘     └────┬────┘
                                              │
                                              │
                                              │
                                       ┌──────┴────┐
                                       │           │
                                       │  Main.hs  │
                                       │   (blob)  │
                                       │           │
                                       └───────────┘

```

This is all fine and dandy from an understanding standpoint but if we want to
implement it we will need to dig a little deeper. Let's pull out our python
script to see how git stores this in files. We need to figure out the sha1 of
the tree though, which is something we can easily do using the `HEAD`, the
`HEAD` has the commit info which contains the `sha1` of the tree.

```nu
> git cat-file -p HEAD
tree 7090a76ce56ca6d8205defaee163b3f53727f46b
... other details ...
```

Let's run our python script on this (Can you figure out the location of the file
now for the script?). Once we execute the script this is the decompressed data
that we see.

```
b'tree 102\x00100644 LICENSE\x00O\xda\xb9\'\xde\xef\xcb\x7f\xc2\xc3\xc0\xfbA\xadX\xfb\xca\x05\x14E100644 Readme.md\x00hY\xd0_O\xc0%:?\xe9z\xea\xee\xba\x1e\xec`\xa5P\xb840000 src\x00\xf5R\xfe"\xdeiw\x82\x8e\xdd5gG.^\xdc:J\xee\xd2'
```

As we can see the format here is

- Header (`tree<space><length>`). Similar to what we saw earlier with `blob`s
- List of entries where each entry has the following structure
  - mode
  - `<space>`
  - name
  - `\x00`
  - Length 20 sha1 of the entry

With this information in our armamentarium we can implement `ls-tree`.

### Implementing `git ls-tree`

First we need to extend our `GitObject` data type to include a `Tree` in it.

```haskell
data GitObject
    = Blob BL.ByteString
    | Tree [TreeEntry]

data TreeEntry = TreeEntry
    { entryMode :: BL.ByteString
    , entryName :: BL.ByteString
    , entrySha1 :: Digest SHA1
    }
    deriving (Show, Eq)
```

Now let's see what the core logic is going to be

- Parse the SHA1 we get from command line. (We obviously are not supporting
  fancy stuff like parsing `main^{tree}`)
- Read the content and decompress it (remember `readContentFromSHA1Code` from
  earlier)
- Parse the tree entries
- decide how to show the entries based on the CLI flags
- show the entries

All this translates to the following Haskell code

```haskell
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
```

The only piece missing here is the `treeParser`. I am omitting functions like
`entryNameStr` and `entryBodyStr` as there is nothing interesting going on
there, it's just accessing fields or computed fields on the data type.

```haskell
treeParser :: Parser [TreeEntry]
treeParser = do
    _ <- string "tree "
    _ <- decimal
    _ <- char '\0'
    treeParser'

treeParser' :: Parser [TreeEntry]
treeParser' = many' treeEntryParser

treeEntryParser :: Parser TreeEntry
treeEntryParser = do
    entryMode <- BLC.pack <$> many' digit
    _ <- space
    entryName <- BL.fromStrict <$> takeWhile (/= _nul)
    _ <- char '\0'
    entrySha1 <- sha1Parser
    pure TreeEntry{..}
```

This should look totally like the parsing structure we discussed.

## `git write-tree`

Let's see if we can use the git plumbing commands to understand what
`git write-tree` does. Before we can execute this command we will need to stage
our files. Let's start by creating a fresh git repository, with the contents we
had earlier.

```sh
> mkdir git-test
> cd git-test
> echo "# hagit" | save Readme.md
> echo "Do whatever" | save LICENSE
> mkdir src
> echo 'main = putStrLn "Hello git enthusiasts"' | save src/Main.hs
> git init
```

Now we will stage LICENSE and Readme.md.

```sh
> git update-index --add Readme.md
> git update-index --add LICENSE
```

We can now run the `write-tree` command

```sh
> git write-tree
6434b37c202856f8885c459d32a78f31f425af82
```

Let's see what is inside this file.

```sh
> git cat-file -p 6434b37c202856f8885c459d32a78f31f425af82
100644 blob 4fdab927deefcb7fc2c3c0fb41ad58fbca051445    LICENSE
100644 blob 6859d05f4fc0253a3fe97aeaeeba1eec60a550b8    Readme.md
```

If you execute `git status` on this repo you will see these files are still
staged. After this, our store looks something like the following

```nushell
❯ ls -a .git/objects/**/* | where type == file
╭───┬────────────────────────────────────────────────────────┬──────┬──────┬────────────────╮
│ # │                          name                          │ type │ size │    modified    │
├───┼────────────────────────────────────────────────────────┼──────┼──────┼────────────────┤
│ 0 │ .git/objects/4f/dab927deefcb7fc2c3c0fb41ad58fbca051445 │ file │ 27 B │ 20 seconds ago │
│ 1 │ .git/objects/64/34b37c202856f8885c459d32a78f31f425af82 │ file │ 85 B │ now            │
│ 2 │ .git/objects/68/59d05f4fc0253a3fe97aeaeeba1eec60a550b8 │ file │ 22 B │ now            │
╰───┴────────────────────────────────────────────────────────┴──────┴──────┴────────────────╯
```

We can edit the file and do these steps again to save a new tree.

```nushell
> echo "Do whatever with this code, idk" | save -f LICENSE
> git update-index --add LICENSE
> git write-tree
9b5c7c528da4d5a9c8423e24fe67862cc34a6615
> git cat-file -p 9b5c7c528da4d5a9c8423e24fe67862cc34a6615
100644 blob 90d2d9b7995603a9a736631944610c956b786dd2    LICENSE
100644 blob 6859d05f4fc0253a3fe97aeaeeba1eec60a550b8    Readme.md
```

After this operation our git store looks like this

```nushell
> ls -a .git/objects/**/* | where type == file
╭───┬────────────────────────────────────────────────────────┬──────┬──────┬───────────────╮
│ # │                          name                          │ type │ size │   modified    │
├───┼────────────────────────────────────────────────────────┼──────┼──────┼───────────────┤
│ 0 │ .git/objects/4f/dab927deefcb7fc2c3c0fb41ad58fbca051445 │ file │ 27 B │ 9 minutes ago │ # LICENSE version 1
│ 1 │ .git/objects/64/34b37c202856f8885c459d32a78f31f425af82 │ file │ 85 B │ 9 minutes ago │ # tree version 1
│ 2 │ .git/objects/68/59d05f4fc0253a3fe97aeaeeba1eec60a550b8 │ file │ 22 B │ 9 minutes ago │ # Readme version 1 (only)
│ 3 │ .git/objects/90/d2d9b7995603a9a736631944610c956b786dd2 │ file │ 47 B │ 3 minutes ago │ # LICENSE version 2
│ 4 │ .git/objects/9b/5c7c528da4d5a9c8423e24fe67862cc34a6615 │ file │ 85 B │ 3 minutes ago │ # tree version 2
╰───┴────────────────────────────────────────────────────────┴──────┴──────┴───────────────╯
```

We can easily see if we `checkout` these files into our working directory it's
effectively like going back and forth in our coding history i.e. we can do
version control.

Now let's jump into its implementation details. But, there is going to be an
important distinction between `git write-tree` and our implementation. Our
implementation will not have any concept of a staging area. You can think of it
as implicitly staging everything and then executing the `git write-tree`
command.

### Implementing `git write-tree`

Here is what we need to do

- For each file path in a given directory
  - If it's a directory
    - Call `git write-tree` recursively on that directory
  - If it's a file
    - Call `git hash-object` on that file.

Which translates to the following Haskell code. If you look closely we also have
a basic setup of ignoring files.

```haskell
writeTree :: GitM ()
writeTree = do
    treeEntry <- writeTree' "."
    liftIO $ print (entrySha1 treeEntry)

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
```

The call to `writeObject` internally calls the construction of this object to be
in memory as we saw in the section `git hash-object`. We need to extend the
functions `objBody` and `objType` to understand what a tree is. Which looks
like -

```haskell
objBody :: GitObject -> BL.ByteString
objBody (Blob b) = b                                       -- same as before
objBody (Tree entries) = mconcat $ fmap entryBody entries

objType :: GitObject -> ObjectType
objType (Blob _) = BlobType                                -- same as before
objType (Tree _) = TreeType

entryBody :: TreeEntry -> BL.ByteString
entryBody TreeEntry{..} =
    entryMode <> " " <> entryName <> "\0" <> sha1ToByteString entrySha1
```

## `git commit-tree`

After the last exercise we have two trees written into the git store. As you can
imagine we can repeat those steps to create write a new tree. Each tree
represents a snapshot of our code (staged code in case of git). We can switch to
any of these trees, but it's hard to remember those SHA1 codes, moreover there
is no link between these different top level trees. You also don’t have any
information about who saved the snapshots, when they were saved, or why. This is
where `commit` object makes it entrance.

The plumbing command `commit-tree` does exactly that. Let's see it in action

```sh
> echo 'First commit' | git commit-tree 6434b37c202856f8885c459d32a78f31f425af82
1c89325168dca1733293036a6a22e9b4ae21d79a

> git cat-file -p 1c89325168dca1733293036a6a22e9b4ae21d79a
tree 6434b37c202856f8885c459d32a78f31f425af82
author Sherub Thakur <sherub.thakur@gmail.com> 1695802439 +0530
committer Sherub Thakur <sherub.thakur@gmail.com> 1695802439 +0530

First commit
```

Commit-tree makes use of the `user.email` and `user.name` from the config, it
also adds a timestamp to the commit. This gives us an idea of what we want to
implement. But before doing that there is another thing that commit stores, it's
parent's info, let's see that in action too.

```sh
> echo 'Updated license terms' | git commit-tree 9b5c7c528da4d5a9c8423e24fe67862cc34a6615 -p 1c89325168dca1733293036a6a22e9b4ae21d79a
64d7612cbddff91269b4d73e72af1e4c28f1e5e9

> git cat-file -p 64d7612cbddff91269b4d73e72af1e4c28f1e5e9
tree 9b5c7c528da4d5a9c8423e24fe67862cc34a6615
parent 1c89325168dca1733293036a6a22e9b4ae21d79a
author Sherub Thakur <sherub.thakur@gmail.com> 1695802728 +0530
committer Sherub Thakur <sherub.thakur@gmail.com> 1695802728 +0530

Updated license terms
```

With this we now have the full picture of what we need to do.

We can also note this gives us a fairly good view of what git is doing under the
hood. We essentially have a "linked-list" of commits and each of the node in
that linked-list points to a "tree" where each tree is a snapshot of our code at
any point. There's also some "structural-sharing" going on. This is what I
always used to think is the case when dealing with git but to see it in action,
I have a newfound appreciation for this tool. Git does more with compression on
larger repos, but that is a topic for another day.

To visualize this is how this structure looks like

```text

       ┌─────────────────────┐         ┌──────────────────┐                 ┌────────────┐
       │Updated license terms├────────►│       tree       ├──┬─────────────►│  LICENSE   │
       │     64d7612c        │         │      9b5c7c52    │  │              │  90d2d9b7  │
       └────────┬────────────┘         └──────────────────┘  │              └────────────┘
                │                                            │
                │                                            │
                │                                            │              ┌────────────┐
                │                                            └─────────────►│   Readme   │
                │                                            ┌─────────────►│   6859d05f │
                │                                            │              └────────────┘
                ▼                                            │
       ┌─────────────────────┐         ┌──────────────────┐  │              ┌────────────┐
       │    First Commit     ├────────►│      tree        │  │              │  LICENSE   │
       │     1c893251        │         │    6434b37c      ├──┴─────────────►│  4fdab927  │
       └─────────────────────┘         └──────────────────┘                 └────────────┘


```

You can even run `git log --stat 64d7612cbddff91269b4d73e72af1e4c28f1e5e9` to
see the history what we created. Pretty wild what we can achieve with just these
plumbing commands.

### Implementing `git commit-tree`

We need to again extend our `GitObject` to also understand what a commit is.

```haskell
data GitObject
    = Blob BL.ByteString
    | Tree [TreeEntry]
    | Commit CommitInner

data CommitInner = CommitInner
    { treeSha1 :: Digest SHA1
    , parentSha1 :: Maybe (Digest SHA1)
    , commitMessage :: BL.ByteString
    , commitAuthor :: Contributor
    , commitCommitter :: Contributor
    }

data Contributor = Contributor
    { contribNameAndEmail :: BL.ByteString
    , contribDate :: ZonedTime
    }
```

Now, let's see what we have to do.

- Parse CLI options into an internal structure
- Write that structure into the git store

And here is the code -

```haskell
data CommitTreeOpts = CommitTreeOpts
    { treeShaOpt :: BL.ByteString
    , parentShaOpt :: Maybe BL.ByteString
    , commitMessageOpt :: BL.ByteString
    , commitAuthorOpt :: Maybe BL.ByteString
    }

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
```

Just as we saw with `git write-tree` we need to extend `objBody` and `objType`
to accommodate for `commit` so that `writeObject` can write it to a file.

```haskell
objBody :: GitObject -> BL.ByteString
objBody (Blob b) = b                                       -- same as before
objBody (Tree entries) = mconcat $ fmap entryBody entries  -- same as before
objBody (Commit commitInner) = commitBody commitInner

objType :: GitObject -> ObjectType
objType (Blob _) = BlobType                                -- same as before
objType (Tree _) = TreeType                                -- same as before
objType (Commit _) = CommitType

commitBody :: CommitInner -> BL.ByteString
commitBody CommitInner{..} =
    "tree "
        <> sha1ToHexByteString treeSha1
        <> "\n"
        <> parentStr
        <> BLC.pack (contribStr "author" commitAuthor)
        <> "\n"
        <> BLC.pack (contribStr "committer" commitCommitter)
        <> "\n"
        <> "\n"
        <> commitMessage
  where
    parentStr = maybe "" (\sha -> "parent " <> sha1ToHexByteString sha <> "\n") parentSha1
    contribStr :: String -> Contributor -> String
    contribStr c Contributor{..} =
        printf "%s %s %s" c (BLC.unpack contribNameAndEmail) (formatTime defaultTimeLocale "%s %z" contribDate)
```

With this we now have a VCS system that can do version control locally. Pretty
rad if you ask me.

## Limitations

As you can imagine if git was storing data like this you would have quickly run
out of storage on a large repo with lots of edits. Obviously that is not the
case. In the next tutorial we will explore how git saves you some space.
