{-# LANGUAGE RecordWildCards #-}

module HTTPSmartParse (gitServerCapabilitiesParser, lsResultParser, fetchOutput) where

import Data.ByteString.Lazy as BL (ByteString, dropEnd)
import qualified Data.ByteString.Lazy.Char8 as BLC
import HTTPSmartCommand (Ref (..))
import ObjectParse (sha1Parser)
import PktLine (PktLine, getPktLineData, isPktLineSpecial)
import PktLineParse (pktLinesParser)
import Text.Parsec (ParseError, anyChar, char, many, parse, string)
import Text.Parsec.ByteString.Lazy (Parser)

gitServerCapabilitiesParser :: Parser [BL.ByteString]
gitServerCapabilitiesParser = onlyRelevantData <$> pktLinesParser

refLineParser :: Parser Ref
refLineParser = do
    refSha1 <- sha1Parser
    _ <- string " "
    refName <- BLC.pack <$> many anyChar
    pure $ Ref{..}

lsResultParser :: BL.ByteString -> Either ParseError [Ref]
lsResultParser input = do
    pktLines <- parse pktLinesParser "" input
    let releventData = onlyRelevantData pktLines
    mapM (parse refLineParser "") releventData

packLineParser :: Parser BL.ByteString
packLineParser = do
    _ <- char '\x01'
    line <- many anyChar
    pure $ BLC.pack line

fetchOutput :: BL.ByteString -> Either ParseError BL.ByteString
fetchOutput input = do
    pktLines <- parse pktLinesParser "" input
    let releventData = map getPktLineData . tail . init $ pktLines
    packLines <- mapM (parse packLineParser "") releventData
    pure $ BLC.concat packLines

-- Removes the special lines and drops the \n at the end of each pkt line
onlyRelevantData :: [PktLine] -> [BL.ByteString]
onlyRelevantData = map (BL.dropEnd 1 . getPktLineData) . filter (not . isPktLineSpecial)
