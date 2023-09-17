{-# LANGUAGE ScopedTypeVariables #-}

module HTTPSmartParse (pktLineParser) where

import Data.Binary (Word16)
import Data.ByteString.Lazy.Char8 as BLC (pack)
import HTTPSmartTypes (PktLine (..), PktLineData (PktLineData), PktLineSpecial (..))
import Numeric (readHex)
import Text.Parsec (
    count,
    hexDigit,
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
        0 -> pure $ PktSpecial FlushPkt
        1 -> pure $ PktSpecial DelimiterPkt
        2 -> pure $ PktSpecial ResponseEndPkt
        3 -> fail "Invalid packet length: 3"
        4 -> fail "Invalid packet length: 4"
        n -> do
            let len = n - pktLineLengthSize
            pktData <- count len anyChar
            pure $ PktLine $ PktLineData $ BLC.pack pktData
