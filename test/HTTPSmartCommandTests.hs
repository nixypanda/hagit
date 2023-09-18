{-# LANGUAGE OverloadedStrings #-}

module HTTPSmartCommandTests (commandTests) where

import Data.ByteString.Lazy as BL (concat)
import HTTPSmartCommand (Command (..), encodeCommand)
import Test.HUnit

testEncodeCmd :: Test
testEncodeCmd =
    let
        cmdArgs =
            [ "peel"
            , "symrefs"
            , "unborn"
            , "ref-prefix HEAD"
            , "ref-prefix refs/heads/"
            , "ref-prefix refs/tags/"
            ]
        capabilities = ["object-format=sha1", "agent=git/2.41.0"]
        cmd = LsRefs capabilities cmdArgs
        expectedOutput =
            BL.concat
                [ "0013command=ls-refs"
                , "0016object-format=sha1"
                , "0014agent=git/2.41.0"
                , "0001"
                , "0009peel\n"
                , "000csymrefs\n"
                , "000bunborn\n"
                , "0014ref-prefix HEAD\n"
                , "001bref-prefix refs/heads/\n"
                , "001aref-prefix refs/tags/\n"
                , "0000"
                ]
     in
        TestCase $
            assertEqual
                "Encoded command should match expected output"
                expectedOutput
                (encodeCommand cmd)

commandTests :: [Test]
commandTests = [testEncodeCmd]
