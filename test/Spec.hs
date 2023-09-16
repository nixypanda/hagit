{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import Crypto.Hash (Digest, SHA1, digestFromByteString)
import Data.ByteString.Char8 as BSC (pack)
import Data.Maybe (fromJust)
import Object (TreeEntry (..))
import ObjectParse (treeEntryParser)
import Text.Parsec (parse)

sha1 :: Digest SHA1
sha1 =
    fromJust $
        digestFromByteString $
            BSC.pack
                ['\131', '\146', '\209', 'Y', '\242', '\231', '\CAN', '%', '\STX', '\DEL', '\214', '\175', 'X', 'c', '\210', '\CAN', '\184', '\181', '\249', '\203']

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

treeEntryParserTests :: Test
treeEntryParserTests = TestList [treeEntryParserTestBasic]

main :: IO ()
main = do
    _ <- runTestTT treeEntryParserTests
    return ()
