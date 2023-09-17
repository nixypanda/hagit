{-# LANGUAGE OverloadedStrings #-}

module PktLine (
    PktLine (..),
    PktLineSpecial (..),
    PktLineData (..),
    isPktLineSpecial,
    getPktLineData,
) where

import qualified Data.ByteString.Lazy as BL

data PktLine
    = PktSpecial PktLineSpecial
    | PktLine PktLineData
    deriving (Show, Eq)

data PktLineSpecial
    = FlushPkt
    | DelimiterPkt
    | ResponseEndPkt
    deriving (Show, Eq)

newtype PktLineData = PktLineData
    {line :: BL.ByteString}
    deriving (Show, Eq)

isPktLineSpecial :: PktLine -> Bool
isPktLineSpecial (PktSpecial _) = True
isPktLineSpecial _ = False

getPktLineData :: PktLine -> BL.ByteString
getPktLineData (PktLine (PktLineData pktLineData)) = pktLineData
getPktLineData _ = ""
