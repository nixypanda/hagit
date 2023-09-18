{-# LANGUAGE ScopedTypeVariables #-}

module PktLineParse (pktLineParser, pktLinesParser) where

import Data.Binary (Word16)
import Data.ByteString.Lazy.Char8 as BLC (pack)
import Numeric (readHex)
import PktLine (PktLine, dataPktLine, delimiterPkt, flushPkt, responseEndPkt)
import Text.Parsec (
    count,
    hexDigit,
    many,
 )
import Text.Parsec.ByteString.Lazy (Parser)
import Text.Parsec.Char (anyChar)

pktLineLengthSize :: Int
pktLineLengthSize = 4

hexNumber :: Parser Word16
hexNumber = do
    hexDigits <- count 4 hexDigit
    case readHex hexDigits of
        [(n, _)] -> return n
        _ -> fail "Invalid hexadecimal number"

pktLineParser :: Parser PktLine
pktLineParser = do
    pktLineLen :: Int <- fromIntegral <$> hexNumber
    case pktLineLen of
        0 -> pure flushPkt
        1 -> pure delimiterPkt
        2 -> pure responseEndPkt
        3 -> fail "Invalid packet length: 3"
        4 -> fail "Invalid packet length: 4"
        n -> do
            let len = n - pktLineLengthSize
            pktData <- count len anyChar
            pure $ dataPktLine $ BLC.pack pktData

pktLinesParser :: Parser [PktLine]
pktLinesParser = many pktLineParser
