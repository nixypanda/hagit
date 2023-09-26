{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PktLineParse (pktLineParser, pktLinesParser) where

import Data.Attoparsec.ByteString.Char8 (hexadecimal)
import Data.Attoparsec.ByteString.Lazy (Parser, count, many', parseOnly, satisfy, take)
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word16)
import Data.Word8 (isHexDigit)
import PktLine (PktLine, dataPktLine, delimiterPkt, flushPkt, responseEndPkt)
import Prelude hiding (take)

pktLineLengthSize :: Int
pktLineLengthSize = 4

hexNumber :: Parser Word16
hexNumber = do
    hexDigits <- count 4 (satisfy isHexDigit)
    case parseOnly hexadecimal (BL.pack hexDigits) of
        Right n -> pure n
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
            pktData <- take len
            pure $ dataPktLine $ BL.fromStrict pktData

pktLinesParser :: Parser [PktLine]
pktLinesParser = many' pktLineParser
