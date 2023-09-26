{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTPSmartCommand (
    encodeCommand,
    Command (..),
    Ref (..),
    refsToFetch,
) where

import Crypto.Hash (Digest, SHA1)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Char (ord)
import Data.Word (Word8)
import PktLine (dataPktLine, delimiterPkt, encodePktLine, flushPkt)

type Capability = BL.ByteString
type CommandArugment = BL.ByteString

data Command
    = LsRefs [Capability] [CommandArugment]
    | Fetch [Capability] [CommandArugment]
    deriving (Show)

encodeCommand :: Command -> BL.ByteString
encodeCommand (LsRefs capabilities cmdArgs) =
    encodeCmd' "command=ls-refs" capabilities cmdArgs
encodeCommand (Fetch capabilities cmdArgs) =
    encodeCmd' "command=fetch" capabilities cmdArgs

refsToFetch :: [Capability] -> [Ref] -> Command
refsToFetch caps =
    Fetch caps
        . ("no-progress" :)
        . map (("want " <>) . BLC.pack . show . refSha1)

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

data Ref = Ref
    { refName :: BL.ByteString
    , refSha1 :: Digest SHA1
    }
    deriving (Show, Eq)
