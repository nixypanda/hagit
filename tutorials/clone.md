# Clone

To be able to implement `git clone` we need to understand how a git client
interacts with the git server to get the contents of a repository.

## Reverse Engineering (kinda)

I believe one of the best way to learn about something is to just get your hands
dirty. In this particular case we will take that approach and then get to the
theory aspects of this from git documentation. So, let's start by cloning a
repository and let's observe what requests were send and what responses we got.
Fire up your faviourate network protocol analyzer and get to oberving what is
going on.

For this tutorial I will make use of `mitmproxy`

```sh
> mitmproxy # Start this in a terminal window and execute the following on another
> git -c http.sslVerify=false -c http.proxy=localhost:8080 clone https://github.com/codecrafters-io/git-sample-1
```

Try to explore the requests and responses can you make sense of what is going
on. As you can see there were a few requests made. At the time of writing this
article there are 3 requests made. First seemimgly asking about servers
capabilities, second executing a command `ls-refs` and getting a response of the
`refs` and lastly we send a command `fetch`. Here is the flow in
"gen-z"/millenial speak.

```text
Client ----- Ae-yo, what all can you do?                              ---->
       <---- Ma man, I can do the following (list of capabilites)     ----- Server
Client ----- Bruh, fetch me refs you got, using (list of capabilties) ---->
       <---- Sure homie, here you go (sends a list of refs)           ----- Server
Client ----- Ok, man fetch me these refs                              ---->
       <---- counts stuff to send, packs them and then sends them     ----- Server
```

At this point I think we should probably take a look at some documentation to
see what is going on as we have run into some weird encoding scheme.

## Pkt-line

Chances are when you went in to see what was going on using a network analyzer
you saw something like this -

```text
001e# service=git-upload-pack
0000000eversion 2
0022agent=git/github-b60c2b516187
0013ls-refs=unborn
0027fetch=shallow wait-for-done filter
0012server-option
0017object-format=sha1
0000
```

Git’s protocol payload makes extensive use of the so-called packet line (or
pkt-line as used in the technical documentation) format. A pkt-line is a
variable length binary string with the length encoded in the first four bytes
(in hexadecimal) of the pkt-line.

### Example `001e# service=git-upload-pack`

Let's take an example: `001e# service=git-upload-pack`.

Let us start count the number of bytes starting at `#`. The value is 24, and
what is `1e` in decimal? It's 30. Wait a minute what is going on here? Looking
through the technical documentation we can see that the first few values are
reserved, upto 0004.

- '0000' Flush Packet (flush-pkt) - indicates the end of a message
- '0001' Delimiter Packet (delim-pkt) - separates sections of a message
- '0002' Response End Packet (response-end-pkt) - indicates the end of a
  response for stateless connections
- '0003' reserved
- '0004' reserved

We need to add 5 to everything but that still gets us to 29 length. No? Turns
out there is a `\n` at the end of each line (which `mitmproxy` uses to format
the data) and when that is the case we need to add 1 to the length. It's not
always present.

### Implementing Pkt-line parsing

We can easily parse pkt-line as follows

```haskell
pktLineParser :: Parser PktLine
pktLineParser = do
    pktLineLen :: Int <- fromIntegral <$> hexNumber
    case pktLineLen of
        0 -> pure flushPkt
        1 -> pure delimiterPkt
        2 -> pure responseEndPkt
        3 -> fail "Invalid packet length: 3"
        4 -> fail "Invalid packet length: 4"
        n -> do
            let len = n - pktLineLengthSize
            pktData <- take len
            pure $ dataPktLine $ BL.fromStrict pktData

pktLinesParser :: Parser [PktLine]
pktLinesParser = many' pktLineParser
```

The `hexNumber` parses only first 4 bytes. We add a few tests for this and we
are done parsing Pkt-line.

## HTTP Smart

Now let's get back to those http calls with this newfound knowledge of
`Pkt-Line` format. Of the few requests that were made we know that the first one
is about asking for server capabilities which looks like -

```sh
GET /git-upload-pack?service=git-upload-pack HTTP/2.0`
```

It's sent alongside a bunch of headers, which we are not interested in. Here is
the response that we get is of the form the server.

```text
001e# service=git-upload-pack
0000000eversion 2
0022agent=git/github-b60c2b516187
0013ls-refs=unborn
0027fetch=shallow wait-for-done filter
0012server-option
0017object-format=sha1
0000
```

Which we now know is just `pkt-line` encoded data. So let's remove all the
`pkt-line` stuff from it and we are left with.

```
# service=git-upload-pack

version 2
agent=git/github-b60c2b516187
ls-refs=unborn
fetch=shallow wait-for-done filter
server-option
object-format=sha1

```

These are what we call server capabilities. A client can call the server
specifying which capabilities to be in effect. To know more about capabilities
you can refer to
[gitprotocol-capabilities.txt](https://github.com/git/git/blob/795ea8776befc95ea2becd8020c7a284677b4161/Documentation/gitprotocol-capabilities.txt)
and
[gitprotocol-v2.txt](https://github.com/git/git/blob/795ea8776befc95ea2becd8020c7a284677b4161/Documentation/gitprotocol-v2.txt).
We are going to specify that the `object-format` should be `sha1`.

Now let's look at the 2nd API call.

```sh
POST /git-upload-pack HTTP/2.0
git-protocol: version-2
content-type: application/x-git-upload-pack-request
accept: application/x-git-upload-pack-result
..other headers..

0014command=ls-refs
0014agent=git/2.41.00016object-format=sha100010009peel
000csymrefs
000bunborn
0014ref-prefix HEAD
001bref-prefix refs/heads/
001aref-prefix refs/tags/
0000
```

Looking
[here](https://github.com/git/git/blob/795ea8776befc95ea2becd8020c7a284677b4161/Documentation/gitprotocol-v2.txt#L126)
we get a pretty good sense of what is going on here. Essentially we are asking
for a list of refs here. Let's see what the server response is to this request.

```sh
005247b37f1a82bfe85f6d8df52b6258b75e4343b7fd HEAD symref-target:refs/heads/master
003f47b37f1a82bfe85f6d8df52b6258b75e4343b7fd refs/heads/master
0000
```

In this case the server responded with the list of refs (pkt-line encoded
obviously). Now let's look at the last request.

```sh
POST /git-upload-pack HTTP/2.0
git-protocol: version-2
content-type: application/x-git-upload-pack-request
accept: application/x-git-upload-pack-result
..other headers..

0011command=fetch0014agent=git/2.41.00016object-format=sha10001000dthin-pack000dofs-delta0032want 47b37f1a82bfe85f6d8df52b6258b75e4343b7fd
0032want 47b37f1a82bfe85f6d8df52b6258b75e4343b7fd
0009done
0000
```

and the response for this is something like this

```sh
000dpackfile
0025\x02Enumerating objects: 332, done.
0024\x02Counting Objects: 0% (1/332)
....
002d\x02Counting Objects: 0% (332/332), done
0027\x02Compressing Objects:   0% (1/152)
....
0030\x02Compressing Objects: 100% (1/152), done
2004\x01PACK.....
...
0000
```

You might find the Enumerating, Counting and Compressing objects lines familiar
as these are what you see when you do `git clone` yourself. The interesting part
is what comes next this is the Packfile format encoded with Pktline encoding.
Packfiles we have discussed at length already, so we know how to get data out of
it. For now let's focus on implementing these API calls and parsing their
responses.

### Implementing getting server capabilities

Using `mitmproxy` we have already seen what the API call was so we can simply
implement it as follows -

```haskell
discoverGitServerCapabilities :: String -> IO (Either HttpSmartError [BL.ByteString])
discoverGitServerCapabilities url = do
    httpManager <- newManager tlsManagerSettings
    initReq <- parseUrlThrow $ url <> "/info/refs"
    let params = [("service", "git-upload-pack")]
        headers =
            [ ("Accept", "application/x-git-upload-pack-advertisement")
            , ("git-protocol", "version=2")
            ]
        request = initReq
            { queryString = renderSimpleQuery True params
            , requestHeaders = headers
            }
    res <- httpLbs request httpManager
    let received = responseBody res
    pure $ first ParsingError $ gitServerCapabilitiesParser received
```

Where the parsing of response is done as follows.

```haskell
gitServerCapabilitiesParser :: BL.ByteString -> Either ParseError [BL.ByteString]
gitServerCapabilitiesParser = parseOnly (onlyRelevantData <$> pktLinesParser)

-- Removes the special lines and drops the \n at the end of each pkt line
onlyRelevantData :: [PktLine] -> [BL.ByteString]
onlyRelevantData = map (BL.dropEnd 1 . getPktLineData) . filter (not . isPktLineSpecial)
```

### Implementing ls-refs

Without providing much explanation here is how this API call looks like. The
only thing to note here is that we specify `object-format=sha1` as capability.

```haskell
lsRefs :: String -> IO (Either HttpSmartError [Ref])
lsRefs url = do
    httpManager <- newManager tlsManagerSettings
    initReq <- parseUrlThrow $ "POST " <> url <> "/git-upload-pack"
    let
        cmdArgs =
            [ "peel"
            , "unborn"
            , "ref-prefix HEAD"
            , "ref-prefix refs/heads/"
            , "ref-prefix refs/tags/"
            ]
        cmd = LsRefs ["object-format=sha1"] cmdArgs
        headers =
            [ ("git-protocol", "version=2")
            , ("content-type", "application/x-git-upload-pack-request")
            , ("accept", "application/x-git-upload-pack-result")
            ]
        request =
            initReq
                { requestHeaders = headers
                , requestBody = RequestBodyLBS $ encodeCommand cmd
                }
    res <- httpLbs request httpManager
    let received = responseBody res
    pure $ first ParsingError $ lsResultParser received
```

Let's take a look at the types that we use here.

```haskell
type Capability = BL.ByteString
type CommandArugment = BL.ByteString

data Command
    = LsRefs [Capability] [CommandArugment]
```

Let's also look at how we are encoding it into the pkt-line format

```haskell
encodeCommand :: Command -> BL.ByteString
encodeCommand (LsRefs capabilities cmdArgs) =
    encodeCmd' "command=ls-refs" capabilities cmdArgs

encodeCmd' :: BL.ByteString -> [Capability] -> [CommandArugment] -> BL.ByteString
encodeCmd' cmd capabilities cmdArgs =
    BL.concat
        [ encodePktLine $ dataPktLine cmd
        , encodeCapabilities
        , encodePktLine delimiterPkt
        , encodeCmdArgs
        , encodePktLine flushPkt
        ]
  where
    encodeLines = BL.concat . map (encodePktLine . dataPktLine)
    encodeCapabilities = encodeLines capabilities
    encodeCmdArgs = encodeLines . map (`BL.snoc` lf) $ cmdArgs
```

Lastly the parsing logic to get the `ref`s

```haskell
data Ref = Ref
    { refName :: BL.ByteString
    , refSha1 :: Digest SHA1
    }

lsResultParser :: BL.ByteString -> Either ParseError [Ref]
lsResultParser input = do
    pktLines <- parseOnly pktLinesParser input
    let releventData = onlyRelevantData pktLines
    mapM (parseOnly refLineParser) releventData

refLineParser :: Parser Ref
refLineParser = do
    refSha1 <- sha1StrParser
    _ <- string " "
    refName <- takeLazyByteString
    pure $ Ref{..}
```

### Implementing fetch

This is just another API call.

```haskell
fetch :: String -> [Ref] -> IO (Either HttpSmartError BL.ByteString)
fetch url refs = do
    httpManager <- newManager tlsManagerSettings
    initReq <- parseUrlThrow $ "POST " <> url <> "/git-upload-pack"
    let cmd = refsToFetch ["object-format=sha1"] refs
        headers =
            [ ("git-protocol", "version=2")
            , ("content-type", "application/x-git-upload-pack-request")
            , ("accept", "application/x-git-upload-pack-result")
            ]
        request =
            initReq
                { method = "POST"
                , requestHeaders = headers
                , requestBody = RequestBodyLBS $ encodeCommand cmd
                }
    res <- httpLbs request httpManager
    let received = responseBody res
    pure $ first ParsingError $ fetchOutput received
```

One thing to note here is the presence of `refsToFetch`. Let's look inside this
function.

```haskell
refsToFetch :: [Capability] -> [Ref] -> Command
refsToFetch caps =
    Fetch caps
        . ("no-progress" :)
        . map (("want " <>) . BLC.pack . show . refSha1)
```

As we can see there is this `no-progress` stuff that we are prepending to the
`want`s. The `want` part tells the server what `ref`s we want. The `no-progress`
part tells the server that to not send the following lines and just send the
packfiles. This reduces the parsing that we need to do on this response.

```text
0025\x02Enumerating objects: 332, done.
0024\x02Counting Objects: 0% (1/332)
....
002d\x02Counting Objects: 0% (332/332), done
0027\x02Compressing Objects:   0% (1/152)
....
0030\x02Compressing Objects: 100% (1/152), done
```

Which now looks simple -

```haskell
fetchOutput :: BL.ByteString -> Either ParseError BL.ByteString
fetchOutput input = do
    pktLines <- parseOnly pktLinesParser input
    let releventData = map getPktLineData . tail . init $ pktLines
    packLines <- mapM (parseOnly packLineParser) releventData
    pure $ BLC.concat packLines

packLineParser :: Parser BL.ByteString
packLineParser = char '\x01' *> takeLazyByteString
```

We need to also expand our `Command` and `encodeCommand` to accommodate for this
new command.

```haskell
data Command
    = LsRefs [Capability] [CommandArugment]
    | Fetch [Capability] [CommandArugment]
    deriving (Show)

encodeCommand :: Command -> BL.ByteString
encodeCommand (LsRefs capabilities cmdArgs) =
    encodeCmd' "command=ls-refs" capabilities cmdArgs
encodeCommand (Fetch capabilities cmdArgs) =
    encodeCmd' "command=fetch" capabilities cmdArgs
```

### Putting it all together

With that we can write a simple interface which deals with all the HTTP Smart
and Packfile stuff and just returns the `GitObject`s

```haskell
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
```

## Storing the data locally

With everything now we can write a simple clone function.

```haskell
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
```

Notice the `forM_ gitObjects writeObject`. We are simply keeping everything as a
loose object in our local storage. This will take up a lot of space when dealing
with a large repository so keep that in mind.

We have mostly seen these functions before except for the `checkoutTreeAt`.
Let's see what that looks like. This function is doing the following Starting at
a tree root

- Read the data of the tree
- for all its entries one-by-one
  - check the type of the entry
  - if it's a file
    - write the file to disk
  - if it's a directory
    - recursively call `checkoutTreeAt`

```haskell
checkoutTreeAt :: Digest SHA1 -> FilePath -> GitM ()
checkoutTreeAt treeSha1 path = do
    treeContents <- readContentFromSHA1Code treeSha1
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
```

And with that we are finally done with cloning.
