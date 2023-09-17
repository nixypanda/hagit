{-# LANGUAGE OverloadedStrings #-}

module PktLineParseTests (pktLineParserTests) where

import Test.HUnit

import Data.Either (isLeft)
import PktLine (PktLine (..), PktLineData (..), PktLineSpecial (..))
import PktLineParse (pktLineParser)
import Text.Parsec (parse)

pktLineParserTests :: [Test]
pktLineParserTests =
    [ TestCase
        ( assertEqual
            "PktLine is parsed properly when it has actual data"
            (parse pktLineParser "" "001e# service=git-upload-pack\n")
            (Right $ PktLine $ PktLineData "# service=git-upload-pack\n")
        )
    , TestCase
        ( assertBool
            "PktLine errors out when thre is no input"
            (isLeft (parse pktLineParser "" ""))
        )
    , TestCase
        ( assertEqual
            "PktLine parses SpecialPkt FlushPkt properly"
            (parse pktLineParser "" "0000")
            (Right $ PktSpecial FlushPkt)
        )
    , TestCase
        ( assertBool
            "PktLine errors if it sees reserved pktLine"
            (isLeft $ parse pktLineParser "" "0004")
        )
    , TestCase
        ( assertBool
            "PktLine errors when length is incorrect"
            (isLeft $ parse pktLineParser "" "000a123")
        )
    ]
