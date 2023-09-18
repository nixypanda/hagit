{-# LANGUAGE OverloadedStrings #-}

module HTTPSmartParseTests (smartParserTests) where

import Crypto.Hash (Digest, SHA1, digestFromByteString)
import Data.ByteString.Char8 as BSC (pack)
import Data.ByteString.Lazy as BL (concat)
import Data.Maybe (fromJust)
import HTTPSmartCommand (Ref (..))
import HTTPSmartParse (lsResultParser)
import Test.HUnit

headSha1Str :: String
headSha1Str = ['\x47', '\xb3', '\x7f', '\x1a', '\x82', '\xbf', '\xe8', '\x5f', '\x6d', '\x8d', '\xf5', '\x2b', '\x62', '\x58', '\xb7', '\x5e', '\x43', '\x43', '\xb7', '\xfd']

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

smartParserTests :: [Test]
smartParserTests = [testLsRefs]
