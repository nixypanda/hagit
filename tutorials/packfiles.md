# Packfile

The challenge that we are doing now asks you to clone a repository. I think this
is way too big a task to do in one go. We will deviate from it a bit. Let's
assume now the stage is to parse packfiles. Odds are if you explored the
`.git/objects` of a sufficiently large repository you would have encountered far
fewer files than you'd expect it would have. In those particular cases you would
have seen something like
`.git/objects/pack/pack-c99eebad96ec700095b7b3b4923e180a197139f1.pack` in
`.git/objects` folder of those repositories. In this section let's explore how
they store data and how we can parse them into normal git objects that we
encountered in the basics section.

Over the lifetime of a repository loose objects (if any) and smaller packs are
consolidated into larger pack(s). The pack format is also used over-the-wire
(which we will explore in the next section). Now, let's look at what all is
present in a packfile.

## Why

Before jumping into parsing packfiles let's first understand why there is need
for these files in the first place. As we dicussed in the basics section that if
git just stored everything with a SHA1 code as is, large repositories with loads
of data would take way too much space on your disk. It will also be a hassle to
transfer them. So what we need is an efficient mechanism to store these objects.

## What to expect from this tutorial

This tutorial will not discuss creation of packfiles and how git uses it. We are
focussing on implementing `git clone` command and for that what we need is to
just be able to parse packfiles. So that we can use the latest tree to get out
the files.

With that out of the way let's dive into parsing packfiles.

## Packfile structure

Let's start by understanding the packfile structure.

- A header appears at the beginning and consists of the following:

  - 4-byte signature: The signature is: {'P', 'A', 'C', 'K'}
  - 4-byte version number (network byte order)
  - 4-byte number of objects contained in the pack (network byte order)

- The header is followed by number of object entries, each of which looks like
  this:

  - _undeltified representation_
    - n-byte type and length (3-bit type (n-1)\*7+4-bit length)
    - compressed data which decompresses to size length
  - _deltified representation_
    - n-byte type and length (3-bit type, (n-1)\*7+4-bit length)
    - base object sha1 (20 bytes) if OBJ_REF_DELTA or a negative relative offset
      from the delta object's position in the pack if this is an OBJ_OFS_DELTA
      object
    - compressed delta data

- The trailer records a pack checksum of all of the above.

With that we can easily come up with a parser for packfiles

- parse header and see it's exactly what we expect
- parse `objectsInPackfile` number of objects
- parse the checksum
- compute the checksum of the received data
- reconstruct the delta objects
- fin

And here is the implementation of that.

```haskell
packfileParser :: Parser PackfileWithDeltas
packfileParser = do
    packfileHeader@PackfileHeader{..} <- parsePackHeader
    unless (magicString == "PACK") $
        fail "Not a pack file: Magic string mismatch"
    unless (packfileVersion == 2) $
        fail ("Unsupported packfile version: (Expected 2, Got: " <> show packfileVersion <> ")")
    packfileObjects <- count objectsInPackfile objectParser
    receivedChecksum <- sha1Parser <* endOfInput
    pure PackfileWithDeltas{..}

parsePackfile :: BL.ByteString -> Either String [GitObject]
parsePackfile input = do
    packfile <- parseOnly packfileParser input
    let inputWithoutChecksum = BL.dropEnd 20 input
        computedChecksum = hashlazy inputWithoutChecksum
        rcvdChecksum = receivedChecksum packfile
    unless (rcvdChecksum == computedChecksum) $
        Left ("Checksum Mismatch: Expected: " <> show rcvdChecksum <> ", Got: " <> show computedChecksum)
    reconstructDeltaObjects $ packfileObjects packfile
```

Now lets deep dive into the implementation details of various parsers that we
have

## Packfile header parsing

It's straightforward and does not need any discussion, so I will just present
the code for it and call it a day.

```haskell
parsePackHeader :: Parser PackfileHeader
parsePackHeader = do
    magicString <- BL.fromStrict <$> take 4
    packfileVersion <- getIntBe
    objectsInPackfile <- getIntBe
    pure PackfileHeader{..}
```

## Object Parsing: Header

Now for the tricky bit, parsing object's size, type and it's data. First we need
to understand how type and size info is stored.

The first byte in this object information has 3 parts

- MSB: This bit tells us if we need to include more bytes to get the size of the
  object
- 3-bit type: These 3 bit tells us object's type
  - OBJ_COMMIT = 1
  - OBJ_TREE = 2
  - OBJ_BLOB = 3
  - OBJ_TAG = 4
  - OBJ_OFS_DELTA = 6
  - OBJ_REF_DELTA = 7
- 4-bit length: least significant bits of the length of the decompressed object.

### Size encoding

Now if the MSB is set then we read the next byte. The seven bits after the MSB
are then included into the bit representation of the size. If the MSB is set in
this byte too, we need to do this again. We do this till we find a byte where
MSB is not set. Let's look at a few examples to make sure we understand this.

#### Example 1: `\x77`

Let's say the byte is `\x77` which in binary is `\b01110111`

- MSB is not set so we don't need to read anything else
- `111`, the first three bits (when reading) represent value `7` which
  translates to `OBJ_REF_DELTA` type
- `0111`, the 4 least significant bits represent the value `7` which translates
  to size `7`

#### Example 2: `\xfa\xfe\xee\x00`

Let's say we have the following sequence of bytes `\xfa\xfe\xee\x00`

- MSB is set so need to read more
- excluding the MSB the first `f` (`1111`) becomes `7` (`111`) which means this
  type is `OBJ_REF_DELTA`
- We take `a` as is this gives us the least significant `4` bits (`1010`)
- in `\xfe` MSB is set again, so the next 7 bits are `111_1110`
- in `\xee` MSB is set again, so the next 7 bits are `110_1110`
- in `\x00` MBS is not set, so we stop here with the bits being `000_0000` Now
  we arrange these bits (in human-readable form):
  `000_0000_110_1110_111_1110_1010` (`0001_0111_0111_1110_1010`) (`177ea`) which
  is `227306` in decimal

#### Implementing size encoding

Now that we understand the logic we can write a parser which implements this
"interesting" size encoding.

```haskell
isMsbSet :: Int8 -> Bool
isMsbSet w = w .&. 0x80 /= 0

objSizeParser :: Int -> Int -> Parser Int
objSizeParser sizeSoFar iteration = do
    nextByte <- fromIntegral <$> anyWord8
    let sizeToAdd = (fromIntegral (nextByte .&. 0x7f)) `shiftL` (4 + iteration * 7)
        newSize = sizeSoFar + sizeToAdd
    if isMsbSet nextByte
        then objSizeParser newSize (iteration + 1)
        else pure newSize
```

Note: the first iteration (i.e. the byte with type info too) is not part of this
function and the assumption is that the caller will handle that part.

Usually Haskell is incredibly concise, however that is not the case here.

### Implementing object header parsing

With that "interesting" size encoding out of the picture the header parsing is
fairly straightforward

```haskell
objHeaderParser :: Parser PackObjHeader
objHeaderParser = do
    byte <- fromIntegral <$> anyWord8
    let objectTypeInt = (byte `shiftR` 4) .&. 0x7
    let maybeObjectType = intToObjectType objectTypeInt
    let szSoFar = byte .&. 0xf
    case maybeObjectType of
        Nothing -> fail $ "Invalid object type: " <> show objectTypeInt
        Just packObjType -> do
            packObjSize <- if isMsbSet byte then objSizeParser szSoFar 0 else pure szSoFar
            pure PackObjHeader{..}

intToObjectType :: Int -> Maybe PackObjType
intToObjectType 1 = Just OBJ_COMMIT
intToObjectType 2 = Just OBJ_TREE
intToObjectType 3 = Just OBJ_BLOB
intToObjectType 4 = Just OBJ_TAG
intToObjectType 6 = Just OBJ_OFS_DELTA
intToObjectType 7 = Just OBJ_REF_DELTA
intToObjectType _ = Nothing
```

## Object Parsing: Dealing with compressed data

Now we are in another tricky situation where the size that we got is the size of
decompressed data but what we have with us is compressed data (zlib). How do we
parse this?

Interestingly enough if we feed this data (as is) to zlib it will decompress the
data and, additionally, it will also return all the non-consumed input as is.
That is great news for us. We just have to see if the Haskell library that wraps
it supports this or not. If you are following along you need to check this for
your specific language implementation.

Note: The implementation that follows is inefficient, but given that the purpose
here is learning this is fine.

After a bunch of digging around in the zlib library I figured out a way to do
this using the streaming interface provided by the library.

```haskell
data DecompressionResult = DecompressionResult
    { decompressedData :: BL.ByteString
    , unconsumedInput :: BL.ByteString
    }

decompressPartial :: BL.ByteString -> Either DecompressError DecompressionResult
decompressPartial input = do
    let decompresser = decompressST zlibFormat defaultDecompressParams
    runST $ decompressLoop BL.empty decompresser
  where
    decompressLoop ::
        BL.ByteString ->
        DecompressStream (ST s) ->
        ST s (Either DecompressError DecompressionResult)
    decompressLoop output (DecompressInputRequired next) = do
        decompressLoop output =<< next (BL.toStrict input)
    decompressLoop output (DecompressOutputAvailable outChunk next) = do
        decompressLoop (output `mappend` BL.fromStrict outChunk) =<< next
    decompressLoop output (DecompressStreamEnd unconsumed) = do
        pure $ Right $ DecompressionResult output (BL.fromStrict unconsumed)
    decompressLoop _ (DecompressStreamError err) =
        pure $ Left err
```

What this does is

- Read the input
- Decompress it
- Return it along with any unconsumed input.

With this out of the way we run into another problem. This thing is doing
decompression, but so far we were doing parsing. We want to alternate between
these two. After much pondering and a few discussions in various FP related
discord servers, I eventually decided to offer a parser like interface for this.
That way we can compose these naturally.

```haskell
decompressParser :: Parser BL.ByteString
decompressParser = do
    everything <- takeLazyByteString
    case decompressPartial everything of
        Left err -> fail $ show err
        Right DecompressionResult{..} -> do
            let goBack = BL.length unconsumedInput
            _ <- goBackParser (fromIntegral goBack)
            pure decompressedData

goBackParser :: Int -> Parser ()
goBackParser n = AIT.Parser $
    \t (AIT.Pos pos_) more _lose success ->
        success t (AIT.Pos $ pos_ - n) more ()
```

And viola, with that we have our parser for decompressing. Is it a good idea or
not, I will let you be the judge for that. I am still weirded out by it and
somehow it feels wrong to me. Still the way this composes with outher parsers is
quite great so I am going to stick to it. If you are interesed about it's impact
on the code you can look at
[this diff](https://github.com/sherubthakur/hagit/pull/2/files)

## Object Parsing: Putting it all together

After we have figured out the header parsing and the decompression we can move
on to the actual object parsing. There's just one detail we need to take care of
before that, the deltified representation. Luckily it's pretty much the same
just that there is a SHA1 between the header and the compressed data. With all
that in mind here is what we have.

```haskell
objectParser :: Parser PackObject
objectParser = do
    rawObjHeader <- objHeaderParser
    let expectedSize = packObjSize rawObjHeader
    rawObj <- case packObjType rawObjHeader of
        OBJ_OFS_DELTA -> fail $ "Unsupported object type: " <> show OBJ_OFS_DELTA
        OBJ_REF_DELTA -> mkDeltifiedObj rawObjHeader <$> sha1Parser <*> decompressParser
        OBJ_BLOB -> gitObjParser (Undeltafied . Blob <$> blobParser' expectedSize)
        OBJ_TREE -> gitObjParser (Undeltafied . Tree <$> treeParser')
        OBJ_COMMIT -> gitObjParser (Undeltafied . Commit <$> commitParser')
        OBJ_TAG -> fail $ "Unsupported object type: " <> show OBJ_TAG

    let actualSize = packObjLen rawObj
    when (expectedSize /= actualSize) $
        fail (objSizeMismatch expectedSize actualSize)

    pure rawObj
  where
    objSizeMismatch expected actual =
        "Object size mismatch: (Expected: " <> show expected <> ", Actual: " <> show actual <> ")"
    gitObjParser objParser = do
        decompressed <- decompressParser
        case parseOnly objParser decompressed of
            Left err -> fail err
            Right obj -> pure obj
```

Note: We are not dealing with everything here. That is something I might do in
the future (no promises though).

We are still not done with packfiles though we need to take care of the deltas.

## Deltas to Commands

Let's understand what is present in the detlafied representation's body. As we
have seen it refers to a base object either using a `SHA1`, when objecty type is
`OBJ_REF_DELTA` and "negative offset" in the case of `OBJ_OFS_DELTA`. Both
ofs-delta and ref-delta store the "delta" that we need to apply to another
object which is called the base object to reconstruct the object.

The delta data starts with the size of the base object and the size of the
object to we will reconstruct. These sizes are encoded using the size encoding
from above. Rest of the deltafied representation's data is a sequence of
instructions to reconstruct the object from the base object. If the base object
is deltified, it must be converted to canonical form first.

Based on this description we can quickly write the size parsing logic that we
will need.

```haskell
deltaHeaderObjSizeParser :: Parser Int
deltaHeaderObjSizeParser = deltaHeaderObjSizeParser' 0 0
  where
    deltaHeaderObjSizeParser' :: Int -> Int -> Parser Int
    deltaHeaderObjSizeParser' sizeSoFar iteration = do
        nextByte <- fromIntegral <$> anyWord8
        let sizeToAdd = (nextByte .&. 0x7f) `shiftL` (iteration * 7)
            newSize = sizeSoFar + sizeToAdd
        if isMsbSet nextByte
            then deltaHeaderObjSizeParser' newSize (iteration + 1)
            else pure newSize
```

Each instruction appends more and more data to the target object until it's
complete. There are two supported instructions:

- One for copy a byte range from the source object
- One for inserting new data embedded in the instruction itself.

With this info we can write the instruction parser. We will fill in the details
later

```haskell
instructionParser :: Parser Instruction
instructionParser = do
    firstByte <- anyWord8
    case instructionType firstByte of
        CopyType -> copyInstructionParser firstByte
        AddNewType -> addInstructionParser firstByte

instructionType :: Word8 -> InstructionType
instructionType firstByte
    | firstByte .&. 0x80 == 0 = AddNewType
    | otherwise = CopyType
```

Now to complete this we will need to better understand each of the instruction

### Instruction to copy from base object

```text
+----------+---------+---------+---------+---------+-------+-------+-------+
| 1xxxxxxx | offset1 | offset2 | offset3 | offset4 | size1 | size2 | size3 |
+----------+---------+---------+---------+---------+-------+-------+-------+
```

It encodes the offset to copy from and the number of bytes to copy. Offset and
size are in little-endian order.

All offset and size bytes are optional. The first seven bits in the first octet
determines which of the next seven octets is present. If bit zero is set,
offset1 is present. If bit one is set offset2 is present and so on.

Note that a more compact instruction does not change offset and size encoding.
For example, if only offset2 is omitted like below, offset3 still contains bits
16-23. It does not become offset2 and contains bits 8-15 even if it's right next
to offset1.

```text
+----------+---------+---------+
| 10000101 | offset1 | offset3 |
+----------+---------+---------+
```

In its most compact form, this instruction only takes up one byte (`0x80`) with
both offset and size omitted, which will have default values zero.

Note: There's another exception: size zero is automatically converted to
`0x10000`.

Let's try to understand this with an example

#### Example 1: `\x90\x14`

- The first byte in binary is `0b1001_0000`
- The MSB tells us this is a `COPY` instruction
- 4 least significant bits are 0, which means that there are no offset bytes
  following this byte. i.e. the offset is 0.
- Out of the three size bits the least significant one is set which means that
  the size is `\x14` or `20` in decimal

#### Example 2: `\xae\x09\x0a\x00\x77`

- The first byte in binary is `0b1010_1110`
- The MSB tells us this is a `COPY` instruction
- 4 least significant bits are `1110`, which means that there are 3 bytes that
  represent the offset following this. The size is therefore `\x000a0900` or
  `657664` in decimal
- Out of the three size bits the second bit is set which means that the size is
  `\x7700` or `30464` in decimal

With a clear understanding of how copy instruction is encoded, let's first see
how this will look like in Haskell

```haskell
data Instruction
    = Copy Int Int
    ...
```

is doing let's parse this

```haskell
copyInstructionParser :: Word8 -> Parser Instruction
copyInstructionParser firstByte = do
    offsets <- count numOffsets anyWord8
    sizes <- count numSizes anyWord8
    let offset = convertToInt hasOffset 0 0 0 4 offsets
    let size = convertToInt hasSize 0 0 0 3 sizes
    pure $ Copy offset size
  where
    hasOffset i = firstByte .&. (1 `shiftL` i) /= 0
    hasSize i = firstByte .&. (1 `shiftL` (i + 4)) /= 0
    numOffsets = popCount $ firstByte .&. 0xf
    numSizes = popCount $ firstByte .&. 0x70

convertToInt :: (Int -> Bool) -> Int -> Int -> Int -> Int -> [Word8] -> Int
convertToInt _ offset _ _ _ [] = offset
convertToInt isPresent val shift i maxI (b : bytes)
    | i == maxI = val
    | isPresent i = convertToInt isPresent newVal (shift + 8) (i + 1) maxI bytes
    | otherwise = convertToInt isPresent val (shift + 8) (i + 1) maxI (b : bytes)
  where
    newVal = val + fromIntegral b `shiftL` shift
```

This again is a bit awkward to write.

### Instruction to add new data

```
+----------+============+
| 0xxxxxxx | data |
+----------+============+
```

This is the instruction to construct target object without the base object. The
following data is appended to the target object. The first seven bits of the
first octet determines the size of data in bytes. The size must be non-zero.

With that we can define our data type for this

```haskell
data Instruction
    ...
    | AddNew BL.ByteString
```

followed by the parsing logic that we need

```haskell
addInstructionParser :: Word8 -> Parser Instruction
addInstructionParser firstByte = do
    let dataSize = fromIntegral $ firstByte .&. 0x7f
    dataToAppend <- take dataSize
    pure $ AddNew $ BL.fromStrict dataToAppend
```

### Putting it all together

With all these pieces in place let's represent a Delta object's content in
Haskell

```haskell
data DeltaContent = DeltaContent
    { baseObjSize :: Int
    , reconstructedObjSize :: Int
    , instructions :: [Instruction]
    }
    deriving (Show, Eq)
```

and the parser for this looks like

```haskell
deltaContentParser :: Parser DeltaContent
deltaContentParser = do
    baseObjSize <- deltaHeaderObjSizeParser
    reconstructedObjSize <- deltaHeaderObjSizeParser
    instructions <- many' instructionParser
    pure $ DeltaContent{..}
```

With that, we are done with converting delta objects to instructions/commands on
base object. Now let's execute them

## Reconstruction of Deltas

With everything that we have been through this part is extremely easy. We just
convert the delta into instructions (we already discussed how to do that) and we
just apply the instructions one by one to a base object, which is just a `fold`
using the applying one instruction to a raw object.

```haskell
reconstructDeltaFromBase :: GitObject -> DeltafiedObj -> Either String GitObject
reconstructDeltaFromBase baseObject deltaObject = do
    DeltaContent{..} <- parseOnly deltaContentParser (deltaObjData deltaObject)
    let reconstructedObjContent = foldl (applyInstruction (objBody baseObject)) "" instructions
        reconstructedHeader = mkPackObjHeader (objType baseObject) (fromIntegral $ BL.length reconstructedObjContent)
        reconstructedObjData = objHeaderRepr reconstructedHeader <> "\0" <> reconstructedObjContent
    parseOnly gitObjectParser reconstructedObjData

applyInstruction :: BL.ByteString -> BL.ByteString -> Instruction -> BL.ByteString
applyInstruction base obj (Copy offset size) =
    obj <> BL.take (fromIntegral size) (BL.drop (fromIntegral offset) base)
applyInstruction _ obj (AddNew dataToAppend) =
    obj <> dataToAppend
```

Now we just need to do this for all the deltas. Remember there could be cases
where the base object might also need reconstruction.

```haskell
reconstructDeltaObjects :: [PackObject] -> Either String [GitObject]
reconstructDeltaObjects rawObjects =
    Map.elems <$> reconstructGitObjects baseObjectsMap objectsToReconstruct
  where
    baseObjectsMap = Map.fromList $ map (objSha1 &&& id) $ mapMaybe getUndeltified rawObjects
    objectsToReconstruct = Seq.fromList $ mapMaybe getDeltified rawObjects

reconstructGitObjects ::
    Map.Map (Digest SHA1) GitObject ->
    Seq.Seq DeltafiedObj ->
    Either String (Map.Map (Digest SHA1) GitObject)
reconstructGitObjects baseObjects Seq.Empty = pure baseObjects
reconstructGitObjects baseObjects (deltaObj :<| rest) =
    case Map.lookup (deltaObjParentSha1 deltaObj) baseObjects of
        Nothing -> reconstructGitObjects baseObjects (rest :|> deltaObj)
        Just baseOject -> do
            reconstructedObj <- reconstructDeltaFromBase baseOject deltaObj
            let baseObjects' = Map.insert (objSha1 reconstructedObj) reconstructedObj baseObjects
            reconstructGitObjects baseObjects' rest
```

With that our packfile reconstruction is complete and what we have is a list of
`GitObject`s.
