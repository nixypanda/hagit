{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PktLine (
    PktLine,
    PktLineSpecial,
    PktLineData,
    isPktLineSpecial,
    getPktLineData,
    encodePktLine,
    flushPkt,
    delimiterPkt,
    responseEndPkt,
    dataPktLine,
) where

import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Text.Printf (printf)

gitProtocolV2Reserved :: Int
gitProtocolV2Reserved = 4

data PktLine
    = PktSpecial PktLineSpecial
    | PktLine PktLineData
    deriving (Show, Eq)

data PktLineSpecial
    = FlushPkt
    | DelimiterPkt
    | ResponseEndPkt
    deriving (Show, Eq)

flushPkt :: PktLine
flushPkt = PktSpecial FlushPkt

delimiterPkt :: PktLine
delimiterPkt = PktSpecial DelimiterPkt

responseEndPkt :: PktLine
responseEndPkt = PktSpecial ResponseEndPkt

newtype PktLineData = PktLineData
    {line :: BL.ByteString}
    deriving (Show, Eq)

dataPktLine :: BL.ByteString -> PktLine
dataPktLine = PktLine . PktLineData

isPktLineSpecial :: PktLine -> Bool
isPktLineSpecial (PktSpecial _) = True
isPktLineSpecial _ = False

getPktLineData :: PktLine -> BL.ByteString
getPktLineData (PktLine (PktLineData pktLineData)) = pktLineData
getPktLineData _ = ""

dataLen :: PktLineData -> Int
dataLen (PktLineData pktLineData) = fromIntegral $ BL.length pktLineData

encodePktLineData :: PktLineData -> BL.ByteString
encodePktLineData pktLine@(PktLineData input) = formattedLen <> input
  where
    length' = dataLen pktLine + gitProtocolV2Reserved
    formattedLen = BLC.pack $ printf "%04x" length'

encodePktLineSpecial :: PktLineSpecial -> BL.ByteString
encodePktLineSpecial FlushPkt = "0000"
encodePktLineSpecial DelimiterPkt = "0001"
encodePktLineSpecial ResponseEndPkt = "0002"

encodePktLine :: PktLine -> BL.ByteString
encodePktLine (PktSpecial pktLineSpecial) = encodePktLineSpecial pktLineSpecial
encodePktLine (PktLine pktLineData) = encodePktLineData pktLineData
