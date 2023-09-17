{-# LANGUAGE OverloadedStrings #-}

module ObjectParseTests (parserTests) where

import Test.HUnit

import Crypto.Hash (Digest, SHA1, digestFromByteString)
import Data.ByteString.Char8 as BSC (pack)
import Data.Maybe (fromJust)
import Object (GitObject (..), TreeEntry (..))
import ObjectParse (gitContentToObject, treeEntryParser)
import Text.Parsec (parse)

sha1String :: String
sha1String = ['\131', '\146', '\209', 'Y', '\242', '\231', '\CAN', '%', '\STX', '\DEL', '\214', '\175', 'X', 'c', '\210', '\CAN', '\184', '\181', '\249', '\203']

sha1 :: Digest SHA1
sha1 = fromJust $ digestFromByteString $ BSC.pack sha1String

treeEntryParserTestBasic :: Test
treeEntryParserTestBasic =
    TestLabel "Check basic treeEntryParser" $
        TestCase
            ( assertEqual
                "Basic treeEntryParser test"
                ( parse
                    treeEntryParser
                    ""
                    "100644 somefile.txt\NUL\131\146\209Y\242\231\CAN%\STX\DEL\214\175Xc\210\CAN\184\181\249\203\&"
                )
                (Right $ TreeEntry "100644" "somefile.txt" sha1)
            )

treeParserTest :: Test
treeParserTest =
    TestLabel "Check basic treeParser" $
        TestCase
            ( assertEqual
                "Basic treeParser test"
                ( gitContentToObject
                    ( "tree 19\NUL100644 somefile.txt\NUL\131\146\209Y\242\231\CAN%\STX\DEL\214\175Xc\210\CAN\184\181\249\203\&"
                        <> "40000 some dir\NUL\131\146\209Y\242\231\CAN%\STX\DEL\214\175Xc\210\CAN\184\181\249\203\&"
                    )
                )
                (Right $ Tree [TreeEntry "100644" "somefile.txt" sha1, TreeEntry "40000" "some dir" sha1])
            )

treeEntryParserTests :: [Test]
treeEntryParserTests = [treeEntryParserTestBasic]

parserTests :: [Test]
parserTests = treeEntryParserTests <> [treeParserTest]
