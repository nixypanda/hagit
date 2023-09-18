module HTTPSmartParse where

import Data.ByteString.Lazy as BL (ByteString, dropEnd)
import PktLine (getPktLineData, isPktLineSpecial)
import PktLineParse (pktLinesParser)
import Text.Parsec.ByteString.Lazy (Parser)

gitServerCapabilitiesParser :: Parser [BL.ByteString]
gitServerCapabilitiesParser =
    map (BL.dropEnd 1 . getPktLineData) . filter (not . isPktLineSpecial)
        <$> pktLinesParser
