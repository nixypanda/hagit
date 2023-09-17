import ObjectParseTests (parserTests)
import ObjectTests (objectTests)
import PktLineParseTests (pktLineParserTests)
import Test.HUnit

tests :: Test
tests =
    TestList $
        parserTests
            <> objectTests
            <> pktLineParserTests

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()
