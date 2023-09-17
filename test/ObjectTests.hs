{-# LANGUAGE OverloadedStrings #-}

module ObjectTests (objectTests) where

import Codec.Compression.Zlib (compress)
import Object (GitObject (..), objCompressedContent, objContent, objHeader)
import Test.HUnit

testObjContent :: Test
testObjContent =
    TestCase $
        assertEqual
            "objContent should return the content of a GitObject"
            (objContent (Blob "Hello"))
            "blob 5\NULHello"

testObjHeader :: Test
testObjHeader =
    TestCase $
        assertEqual
            "objHeader should return the header of a GitObject"
            (objHeader (Blob "Hello"))
            "blob 5"

testObjCompressedContent :: Test
testObjCompressedContent =
    TestCase $
        assertEqual
            "objCompressedContent should return the compressed content of a GitObject"
            (objCompressedContent (Blob "Hello, World!"))
            (compress "blob 13\0Hello, World!")

objectTests :: [Test]
objectTests =
    [ testObjContent
    , testObjHeader
    , testObjCompressedContent
    ]
