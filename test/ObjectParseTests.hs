{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ObjectParseTests (parserTests) where

import Test.HUnit

import Crypto.Hash (Digest, SHA1, digestFromByteString)
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Object (CommitInner (..), Contributor (Contributor), GitObject (..), TreeEntry (..))
import ObjectParse (gitContentToObject, treeEntryParser)
import ParsingUtils (sha1Parser, sha1StrParser)

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

blobParserTest :: Test
blobParserTest =
    let
        input = "blob 4\0hell"
     in
        TestCase
            ( assertEqual
                "blobParser parser a blob entry properly"
                (Right $ Blob "hell")
                (gitContentToObject input)
            )

commitWithoutParentSha1Test :: Test
commitWithoutParentSha1Test =
    let
        treeSha1Str = "33afd6485aadae927bc4bc2986ea9a0d86d5d699"
        -- sha1 will not match with anything, hence it is ok to use it here
        treeSha = fromRight sha1 $ parseOnly sha1StrParser treeSha1Str
        commitTime = parseTimeOrError True defaultTimeLocale "%s %z" "0 +0000"

        input =
            BL.concat
                [ "commit 100\0"
                , "tree "
                , treeSha1Str
                , "\n"
                , "author example <example@me.com> 0 +0000\n"
                , "committer example <example@me.com> 0 +0000\n"
                , "\n"
                , "initial commit"
                ]
        expected =
            CommitInner
                { treeSha1 = treeSha
                , parentSha1 = Nothing
                , commitAuthor = Contributor "example <example@me.com>" commitTime
                , commitCommitter = Contributor "example <example@me.com>" commitTime
                , commitMessage = "initial commit"
                }
     in
        TestCase
            ( assertEqual
                "commit without parent SHA1 is parsed into a commit entry properly"
                (Right $ Commit expected)
                (gitContentToObject input)
            )

commitWithParentSha1Test :: Test
commitWithParentSha1Test =
    let
        treeSha1Str = "33afd6485aadae927bc4bc2986ea9a0d86d5d699"
        -- sha1 will not match with anything, hence it is ok to use it here
        treeSha = fromRight sha1 $ parseOnly sha1StrParser treeSha1Str
        parentSha1Str = "33afd6485aadae927bc4bc2986ea9a0d86d5d699"
        parentSha = fromRight sha1 $ parseOnly sha1StrParser parentSha1Str
        commitTime = parseTimeOrError True defaultTimeLocale "%s %z" "0 +0000"

        input =
            BL.concat
                [ "commit 100\0"
                , "tree "
                , treeSha1Str
                , "\n"
                , "parent "
                , parentSha1Str
                , "\n"
                , "author example <example@me.com> 0 +0000\n"
                , "committer example <example@me.com> 0 +0000\n"
                , "\n"
                , "initial commit"
                ]
        expected =
            CommitInner
                { treeSha1 = treeSha
                , parentSha1 = Just parentSha
                , commitAuthor = Contributor "example <example@me.com>" commitTime
                , commitCommitter = Contributor "example <example@me.com>" commitTime
                , commitMessage = "initial commit"
                }
     in
        TestCase
            ( assertEqual
                "commit without parent SHA1 is parsed into a commit entry properly"
                (Right $ Commit expected)
                (gitContentToObject input)
            )

treeEntryParserTests :: [Test]
treeEntryParserTests = [treeEntryParserTestBasic]

gitObjectParsingTests :: [Test]
gitObjectParsingTests =
    [ blobParserTest
    , treeParserTest
    , commitWithoutParentSha1Test
    , commitWithParentSha1Test
    ]

parserTests :: [Test]
parserTests = treeEntryParserTests <> gitObjectParsingTests <> [sha1ParserTest]
