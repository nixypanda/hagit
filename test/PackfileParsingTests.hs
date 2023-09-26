{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PackfileParsingTests (packfileTests) where

import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.ByteString.Lazy qualified as BL
import Data.Either (fromRight, isLeft)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Object (
    CommitInner (..),
    Contributor (..),
    GitObject (..),
    TreeEntry (..),
    objSha1,
 )
import Packfile (
    DeltaContent (..),
    DeltafiedObj (..),
    Instruction (..),
    PackObjHeader (..),
    PackObjType (..),
    PackObject (..),
 )
import PackfileParsing (
    deltaContentParser,
    deltaHeaderObjSizeParser,
    getIntBe,
    instructionParser,
    objHeaderParser,
    objectParser,
    parsePackfile,
    reconstructDeltaFromBase,
 )
import ParsingUtils (sha1StrParser)
import Test.HUnit

import TestingHelpers (sha1)
import Utils (sha1ToByteString)

testInteBEParser :: [Test]
testInteBEParser =
    [ TestCase $
        assertBool
            "int be parser expects 4 bytes"
            (isLeft $ parseOnly getIntBe "\x12\x34")
    , TestCase $
        assertEqual
            "int be parser parses correctly"
            (Right 374)
            (parseOnly getIntBe "\x00\x00\x01\x76")
    ]

testObjHeaderParser :: [Test]
testObjHeaderParser =
    [ TestCase $
        assertBool
            "obj header parser fails on invalid obj type: 0"
            (isLeft $ parseOnly objHeaderParser "\x00\x00\x01\x76")
    , TestCase $
        assertBool
            "obj header parser fails on invalid obj type: 4"
            (isLeft $ parseOnly objHeaderParser "\x04\x00\x01\x76")
    , TestCase $
        assertEqual
            "obj header parser"
            (Right $ PackObjHeader OBJ_REF_DELTA 7)
            (parseOnly objHeaderParser "\x77")
    , TestCase $
        assertEqual
            "obj header parser"
            (Right $ PackObjHeader OBJ_REF_DELTA 227306)
            (parseOnly objHeaderParser "\xfa\xfe\xee\x00")
    , TestCase $
        assertEqual
            "obj header stops when MSB is zero"
            (Right $ PackObjHeader OBJ_REF_DELTA 10)
            (parseOnly objHeaderParser "\x7a\xfe\xee\x00")
    ]

blob1 :: GitObject
blob1 = Blob "hello world"

compressedBlob1 :: BL.ByteString
compressedBlob1 = "\xbb\00\x78\x9c\xcb\x48\xcd\xc9\xc9\x57\x28\xcf\x2f\xca\x49\x01\x00\x1a\x0b\x04\x5d"

tree1 :: GitObject
tree1 =
    Tree
        [ TreeEntry
            { entryMode = "100644"
            , entryName = "helloworld.txt"
            , entrySha1 = fromRight sha1 $ parseOnly sha1StrParser "95d09f2b10159347eece71399a7e2e907ea3df4f"
            }
        ]
compressedTree1 :: BL.ByteString
compressedTree1 = "\xaa\x02\x78\x9c\x01\x2a\x00\xd5\xff\x31\x30\x30\x36\x34\x34\x20\x68\x65\x6c\x6c\x6f\x77\x6f\x72\x6c\x64\x2e\x74\x78\x74\x00\x95\xd0\x9f\x2b\x10\x15\x93\x47\xee\xce\x71\x39\x9a\x7e\x2e\x90\x7e\xa3\xdf\x4f\x3c\x0d\x10\xd3"

commit1 :: GitObject
commit1 =
    Commit $
        CommitInner
            { treeSha1 = fromRight sha1 $ parseOnly sha1StrParser "33afd6485aadae927bc4bc2986ea9a0d86d5d699"
            , parentSha1 = Nothing
            , commitMessage = "initial commit"
            , commitAuthor = Contributor "example <example@me.com>" (posixSecondsToUTCTime 0)
            , commitCommitter = Contributor "example <example@me.com>" (posixSecondsToUTCTime 0)
            }

compressedCommit1 :: BL.ByteString
compressedCommit1 = "\x90\x09\x78\x9c\x8d\x8b\x4b\x0a\x84\x30\x10\x44\xf7\x39\x45\xef\x07\x86\xe0\x27\x93\x86\x61\x98\xab\x94\xe9\x16\x03\x46\x45\x5a\xf0\xf8\x0a\x7a\x00\x6b\x55\x0f\xde\xb3\x55\x95\xea\x1a\xbd\x84\x26\xb6\x80\x40\xb9\xfa\x74\xa9\xe9\x52\xc5\x31\x28\x18\x5e\x62\x90\x56\x02\xb3\xc3\x66\xc3\xbc\x92\xee\x28\xcb\xa8\xf4\xbd\xcf\xbf\xe8\x3b\xcd\xe5\x47\x9e\x5e\xfe\x9c\x3b\xa1\x64\x33\x7d\xa0\xba\x3c\x65\xcb\x18\xe9\x6a\x0e\xf9\xdf\x2e\x5f"

checksum1 :: BL.ByteString
checksum1 = "\xf6\xf7\x5a\xbc\xcd\xc8\x1c\x33\xc9\xa6\xf8\xa7\x54\x44\x4f\x48\xb7\xa3\xaa\x49"

refDelta1 :: DeltafiedObj
refDelta1 =
    DeltafiedObj
        { deltaObjHeader = PackObjHeader OBJ_REF_DELTA 16
        , deltaObjData = "\x0b\x10\x90\x05\x0b dumb bitch"
        , deltaObjParentSha1 = objSha1 blob1
        }

reconstructedBlobFromBlob1 :: GitObject
reconstructedBlobFromBlob1 = Blob "hello dumb bitch"

-- 1111 1 0000

compressedRefDelta1 :: BL.ByteString
compressedRefDelta1 =
    BL.concat
        [ "\xf0\x01"
        , sha1ToByteString $ objSha1 blob1
        , compressedDeltaObjBody
        ]
  where
    compressedDeltaObjBody = "\x78\x9c\xe3\x16\x98\xc0\xca\xad\x90\x52\x9a\x9b\xa4\x90\x94\x59\x92\x9c\x01\x00\x20\xa8\x04\xae"

testObjParser :: [Test]
testObjParser =
    [ TestCase $
        assertEqual
            "obj parser: blob"
            (Right $ Undeltafied blob1)
            (parseOnly objectParser compressedBlob1)
    , TestCase $
        assertEqual
            "obj parser: tree"
            (Right $ Undeltafied tree1)
            (parseOnly objectParser compressedTree1)
    , TestCase $
        assertEqual
            "obj parser: commit"
            (Right $ Undeltafied commit1)
            (parseOnly objectParser compressedCommit1)
    , TestCase $
        assertEqual
            "obj parser: ref delta"
            (Right $ Deltafied refDelta1)
            (parseOnly objectParser compressedRefDelta1)
    , TestCase $
        assertBool
            "obj parser fails when there is a size mismatch"
            (isLeft $ parseOnly objectParser "\xbc\00\x78\x9c\xcb\x48\xcd\xc9\xc9\x57\x28\xcf\x2f\xca\x49\x01\x00\x1a\x0b\x04\x5d")
    , TestCase $
        assertBool
            "obj parser fails on unsupported types: OBJ_OFS_DELTA"
            (isLeft $ parseOnly objectParser "\x06")
    , TestCase $
        assertBool
            "obj parser fails on unsupported types: OBJ_TAG"
            (isLeft $ parseOnly objectParser "\x04")
    ]

packfile :: BL.ByteString
packfile =
    BL.concat
        [ "PACK\0\0\0\x02\0\0\0\x04"
        , compressedBlob1
        , compressedTree1
        , compressedCommit1
        , compressedRefDelta1
        , checksum1
        ]

testPackfileParser :: Test
testPackfileParser =
    TestCase $
        assertEqual
            "packfile parser parser a valid packfile"
            -- This list will be sorted
            (Right [tree1, blob1, commit1, reconstructedBlobFromBlob1])
            (parsePackfile packfile)

testCopyInstructionParsing :: [Test]
testCopyInstructionParsing =
    [ TestCase $
        assertEqual
            "copy instruction parsing"
            (Right (Copy 0 20))
            (parseOnly instructionParser "\x90\x14")
    , TestCase $
        assertEqual
            "copy instruction parsing"
            (Right (Copy 2304 2560))
            -- 1010_0010 -> \x0900 0a00
            (parseOnly instructionParser "\xa2\x09\x0a")
    , TestCase $
        assertEqual
            "copy instruction parsing"
            (Right (Copy 655369 0))
            -- 1010_0101 -> 0a0009
            (parseOnly instructionParser "\xa5\x09\x0a\x00")
    , TestCase $
        assertEqual
            "copy instruction parsing"
            (Right (Copy 657664 30464))
            -- 1010_1110 -> 000a0900
            (parseOnly instructionParser "\xae\x09\x0a\x00\x77")
    ]

testAddNewInstructionParsing :: Test
testAddNewInstructionParsing =
    TestCase $
        assertEqual
            "add new instruction parsing"
            (Right (AddNew " dumb bitch"))
            (parseOnly instructionParser "\x0b dumb bitch")

testDetafiedObjectToInstructions :: Test
testDetafiedObjectToInstructions =
    TestCase $
        assertEqual
            "detailed object to instructions"
            (Right $ DeltaContent 23 31 [Copy 0 20, AddNew " dumb bitch"])
            (parseOnly deltaContentParser "\x17\x1f\x90\x14\x0b dumb bitch")

testReconstructDeltaFromBase :: [Test]
testReconstructDeltaFromBase =
    [ let baseObject =
            Blob "hey there I am a badass"
          deltaObject =
            DeltafiedObj
                (PackObjHeader OBJ_REF_DELTA 16)
                "\x17\x1f\x90\x14\x0b dumb bitch"
                (objSha1 baseObject)
       in TestCase $
            assertEqual
                "reconstruct delta from base"
                (Right $ Blob "hey there I am a bad dumb bitch")
                (reconstructDeltaFromBase baseObject deltaObject)
    , let baseObject =
            Blob "Hello world"
          deltaObject =
            DeltafiedObj
                (PackObjHeader OBJ_REF_DELTA 16)
                "\x0b\x10\x90\x05\x0b dumb bitch"
                (objSha1 baseObject)
       in TestCase $
            assertEqual
                "reconstruct delta from base"
                (Right $ Blob "Hello dumb bitch")
                (reconstructDeltaFromBase baseObject deltaObject)
    ]

testDeltaObjHeaderSizeParser :: [Test]
testDeltaObjHeaderSizeParser =
    let
        testData =
            [ ("\x05", 5)
            , ("\x00", 0)
            , ("\x87\x05", 647)
            , ("\x7f\x81\x05", 127)
            , ("\x80\x01", 128)
            , ("\x84\x08", 1028)
            ]
        testFunc :: Int -> (BL.ByteString, Int) -> Test
        testFunc i (x, y) =
            TestCase $
                assertEqual
                    ("delta obj header size parser: " <> show i)
                    (Right y)
                    (parseOnly deltaHeaderObjSizeParser x)
     in
        zipWith testFunc [1 ..] testData

packfileTests :: [Test]
packfileTests =
    testInteBEParser
        <> testObjHeaderParser
        <> testObjParser
        <> [testPackfileParser]
        <> testCopyInstructionParsing
        <> [ testAddNewInstructionParsing
           , testDetafiedObjectToInstructions
           ]
        <> testReconstructDeltaFromBase
        <> testDeltaObjHeaderSizeParser
