{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PackfileTests (packfileTests) where

import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.ByteString.Lazy qualified as BL
import PackfileParsing (
    DeltaContent (..),
    Instruction (..),
    ObjectType (..),
    RawDeltifiedObject (..),
    RawObjectHeader (..),
    RawUndeltifiedObject (..),
    deltaContentParser,
    deltaHeaderObjSizeParser,
    instructionParser,
    rawObjSHA1,
    reconstructDeltaFromBase,
 )
import Test.HUnit

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

testReconstructDeltaFromBase :: Test
testReconstructDeltaFromBase =
    let baseObject =
            RawUndeltifiedObject (RawObjectHeader OBJ_BLOB 23) "hey there I am a badass"
        deltaObject =
            RawDeltifiedObject
                (RawObjectHeader OBJ_REF_DELTA 16)
                "\x17\x1f\x90\x14\x0b dumb bitch"
                (rawObjSHA1 baseObject)
     in TestCase $
            assertEqual
                "reconstruct delta from base"
                ( Right $
                    RawUndeltifiedObject
                        (RawObjectHeader OBJ_BLOB 31)
                        "hey there I am a bad dumb bitch"
                )
                (reconstructDeltaFromBase baseObject deltaObject)

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
    testCopyInstructionParsing
        <> [ testAddNewInstructionParsing
           , testDetafiedObjectToInstructions
           , testReconstructDeltaFromBase
           ]
        <> testDeltaObjHeaderSizeParser
