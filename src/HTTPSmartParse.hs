{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HTTPSmartParse (
    gitServerCapabilitiesParser,
    lsResultParser,
    fetchOutput,
) where

import Data.Attoparsec.ByteString.Char8 (char)
import Data.Attoparsec.ByteString.Lazy (Parser, parseOnly, string, takeLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import HTTPSmartCommand (Ref (..))
import ParsingUtils (ParseError, sha1StrParser)
import PktLine (PktLine, getPktLineData, isPktLineSpecial)
import PktLineParse (pktLinesParser)

gitServerCapabilitiesParser :: BL.ByteString -> Either ParseError [BL.ByteString]
gitServerCapabilitiesParser = parseOnly (onlyRelevantData <$> pktLinesParser)

refLineParser :: Parser Ref
refLineParser = do
    refSha1 <- sha1StrParser
    _ <- string " "
    refName <- takeLazyByteString
    pure $ Ref{..}

lsResultParser :: BL.ByteString -> Either ParseError [Ref]
lsResultParser input = do
    pktLines <- parseOnly pktLinesParser input
    let releventData = onlyRelevantData pktLines
    mapM (parseOnly refLineParser) releventData

packLineParser :: Parser BL.ByteString
packLineParser = char '\x01' *> takeLazyByteString

fetchOutput :: BL.ByteString -> Either ParseError BL.ByteString
fetchOutput input = do
    pktLines <- parseOnly pktLinesParser input
    let releventData = map getPktLineData . tail . init $ pktLines
    packLines <- mapM (parseOnly packLineParser) releventData
    pure $ BLC.concat packLines

-- Removes the special lines and drops the \n at the end of each pkt line
onlyRelevantData :: [PktLine] -> [BL.ByteString]
onlyRelevantData = map (BL.dropEnd 1 . getPktLineData) . filter (not . isPktLineSpecial)
