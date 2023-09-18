{-# LANGUAGE OverloadedStrings #-}

module PktLineTests (pktLineTests) where

import Test.HUnit

import Data.Either (isLeft)
import PktLine (PktLine (..), PktLineData (..), PktLineSpecial (..), encodePktLine)

pktLineEncodingTests :: [Test]
pktLineEncodingTests =
    [ TestCase
        ( assertEqual
            "PktLine is encoded properly when it has actual data"
            (encodePktLine $ PktLine $ PktLineData "# service=git-upload-pack\n")
            "001e# service=git-upload-pack\n"
        )
    , TestCase
        ( assertEqual
            "PktLine encodes SpecialPkt FlushPkt properly"
            (encodePktLine $ PktSpecial FlushPkt)
            "0000"
        )
    ]

pktLineTests :: [Test]
pktLineTests = pktLineEncodingTests
