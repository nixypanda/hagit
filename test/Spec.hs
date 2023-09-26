import HTTPSmartCommandTests (commandTests)
import HTTPSmartParseTests (smartParserTests)
import ObjectParseTests (parserTests)
import ObjectTests (objectTests)
import PackfileParsingTests (packfileTests)
import PktLineParseTests (pktLineParserTests)
import PktLineTests (pktLineTests)
import Test.HUnit

tests :: Test
tests =
    TestList $
        parserTests
            <> objectTests
            <> pktLineParserTests
            <> pktLineTests
            <> commandTests
            <> smartParserTests
            <> packfileTests

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()
