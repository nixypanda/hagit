{-# LANGUAGE OverloadedStrings #-}

module HTTPSmartCommand (encodeCommand) where

import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Word (Word8)
import PktLine (dataPktLine, delimiterPkt, encodePktLine, flushPkt)

type Capability = BL.ByteString
type CommandArugment = BL.ByteString

data Command = LsRefs [Capability] [CommandArugment]

encodeCommand :: Command -> BL.ByteString
encodeCommand (LsRefs capabilities cmdArgs) = encodeCmd' "command=ls-refs" capabilities cmdArgs

encodeCmd' :: BL.ByteString -> [Capability] -> [CommandArugment] -> BL.ByteString
encodeCmd' cmd capabilities cmdArgs =
    BL.concat
        [ encodePktLine $ dataPktLine cmd
        , encodeCapabilities
        , encodePktLine delimiterPkt
        , encodeCmdArgs
        , encodePktLine flushPkt
        ]
  where
    encodeLines = BL.concat . map (encodePktLine . dataPktLine)
    encodeCapabilities = encodeLines capabilities
    encodeCmdArgs = encodeLines . map (`BL.snoc` lf) $ cmdArgs

lf :: Word8
lf = fromIntegral . ord $ '\n'
