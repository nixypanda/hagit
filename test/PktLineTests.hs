{-# LANGUAGE OverloadedStrings #-}

module PktLineTests (pktLineTests) where

import Test.HUnit

import PktLine (dataPktLine, encodePktLine, flushPkt)

pktLineEncodingTests :: [Test]
pktLineEncodingTests =
    [ TestCase
        ( assertEqual
            "PktLine is encoded properly when it has actual data"
            (encodePktLine $ dataPktLine "# service=git-upload-pack\n")
            "001e# service=git-upload-pack\n"
        )
    , TestCase
        ( assertEqual
            "PktLine encodes SpecialPkt FlushPkt properly"
            (encodePktLine flushPkt)
            "0000"
        )
    ]

pktLineTests :: [Test]
pktLineTests = pktLineEncodingTests
