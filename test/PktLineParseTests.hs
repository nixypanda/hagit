{-# LANGUAGE OverloadedStrings #-}

module PktLineParseTests (pktLineParserTests) where

import Test.HUnit

import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.Either (isLeft)
import PktLine (dataPktLine, flushPkt)
import PktLineParse (pktLineParser)

pktLineParserTests :: [Test]
pktLineParserTests =
    [ TestCase
        ( assertEqual
            "PktLine is parsed properly when it has actual data"
            (parseOnly pktLineParser "001e# service=git-upload-pack\n")
            (Right $ dataPktLine "# service=git-upload-pack\n")
        )
    , TestCase
        ( assertBool
            "PktLine errors out when thre is no input"
            (isLeft (parseOnly pktLineParser ""))
        )
    , TestCase
        ( assertEqual
            "PktLine parses SpecialPkt FlushPkt properly"
            (parseOnly pktLineParser "0000")
            (Right flushPkt)
        )
    , TestCase
        ( assertBool
            "PktLine errors if it sees reserved pktLine"
            (isLeft $ parseOnly pktLineParser "0004")
        )
    , TestCase
        ( assertBool
            "PktLine errors when length is incorrect"
            (isLeft $ parseOnly pktLineParser "000a123")
        )
    ]
