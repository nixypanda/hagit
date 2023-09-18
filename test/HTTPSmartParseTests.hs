{-# LANGUAGE OverloadedStrings #-}

module HTTPSmartParseTests (smartParserTests) where

import Crypto.Hash (Digest, SHA1, digestFromByteString)
import Data.ByteString.Char8 as BSC (pack)
import Data.ByteString.Lazy as BL (concat)
import Data.Maybe (fromJust)
import HTTPSmartCommand (Ref (..))
import HTTPSmartParse (fetchOutput, lsResultParser)
import Test.HUnit

headSha1Str :: String
headSha1Str =
    [ '\x47'
    , '\xb3'
    , '\x7f'
    , '\x1a'
    , '\x82'
    , '\xbf'
    , '\xe8'
    , '\x5f'
    , '\x6d'
    , '\x8d'
    , '\xf5'
    , '\x2b'
    , '\x62'
    , '\x58'
    , '\xb7'
    , '\x5e'
    , '\x43'
    , '\x43'
    , '\xb7'
    , '\xfd'
    ]

headSha1 :: Digest SHA1
headSha1 = fromJust $ digestFromByteString $ BSC.pack headSha1Str

testLsRefs :: Test
testLsRefs =
    let
        input =
            BL.concat
                [ "003247b37f1a82bfe85f6d8df52b6258b75e4343b7fd HEAD\n"
                , "003f47b37f1a82bfe85f6d8df52b6258b75e4343b7fd refs/heads/master\n"
                , "0000"
                ]
        expected =
            [ Ref{refName = "HEAD", refSha1 = headSha1}
            , Ref{refName = "refs/heads/master", refSha1 = headSha1}
            ]
     in
        TestCase $
            assertEqual
                "ls-refs command result is parsed successfully"
                (Right expected)
                (lsResultParser input)

testFetch :: Test
testFetch =
    let input =
            BL.concat
                [ "000dpackfile\n"
                , "000a\x01PACK\n"
                , "000a\x01\x00\x00\x00\x02\n"
                , "000c\x01\x00\x00\x01L\x94\x0f\n"
                , "0000"
                ]
        expected = "PACK\n\x00\x00\x00\x02\n\x00\x00\x01L\x94\x0f\n"
     in TestCase $
            assertEqual
                "fetch command result is parsed successfully"
                (Right expected)
                (fetchOutput input)

smartParserTests :: [Test]
smartParserTests = [testLsRefs, testFetch]
