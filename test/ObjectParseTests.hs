{-# LANGUAGE OverloadedStrings #-}

module ObjectParseTests (parserTests) where

import Test.HUnit

import Crypto.Hash (Digest, SHA1, digestFromByteString)
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.ByteString.Char8 as BSC (pack)
import Data.ByteString.Lazy as BL (concat)
import Data.Maybe (fromJust)
import Object (GitObject (..), TreeEntry (..))
import ObjectParse (gitContentToObject, treeEntryParser)
import ParsingUtils (sha1Parser)

sha1String :: String
sha1String =
    [ '\131'
    , '\146'
    , '\209'
    , 'Y'
    , '\242'
    , '\231'
    , '\CAN'
    , '%'
    , '\STX'
    , '\DEL'
    , '\214'
    , '\175'
    , 'X'
    , 'c'
    , '\210'
    , '\CAN'
    , '\184'
    , '\181'
    , '\249'
    , '\203'
    ]

sha1 :: Digest SHA1
sha1 = fromJust $ digestFromByteString $ BSC.pack sha1String

sha1ParserTest :: Test
sha1ParserTest =
    let
        input = "\131\146\209Y\242\231\CAN%\STX\DEL\214\175Xc\210\CAN\184\181\249\203"
     in
        TestCase
            ( assertEqual
                "sha1Parser parser a blob entry properly"
                (Right sha1)
                (parseOnly sha1Parser input)
            )

treeEntryParserTestBasic :: Test
treeEntryParserTestBasic =
    let
        input =
            BL.concat
                [ "100644 somefile.txt"
                , "\NUL\131\146\209Y\242\231\CAN%\STX\DEL\214\175Xc\210\CAN\184\181\249\203\&"
                ]
     in
        TestCase
            ( assertEqual
                "treeEntryParser parser a blob entry properly"
                (Right $ TreeEntry "100644" "somefile.txt" sha1)
                (parseOnly treeEntryParser input)
            )

treeParserTest :: Test
treeParserTest =
    let
        input =
            ( "tree 19\NUL100644 somefile.txt\NUL\131\146\209Y\242\231\CAN%\STX\DEL\214\175Xc\210\CAN\184\181\249\203\&"
                <> "40000 some dir\NUL\131\146\209Y\242\231\CAN%\STX\DEL\214\175Xc\210\CAN\184\181\249\203\&"
            )
        expected =
            Tree
                [ TreeEntry "100644" "somefile.txt" sha1
                , TreeEntry "40000" "some dir" sha1
                ]
     in
        TestCase
            ( assertEqual
                "treeParser pases blob entry and tree entry with space properly"
                (Right expected)
                (gitContentToObject input)
            )

treeEntryParserTests :: [Test]
treeEntryParserTests = [treeEntryParserTestBasic]

parserTests :: [Test]
parserTests = treeEntryParserTests <> [treeParserTest] <> [sha1ParserTest]
