import HTTPSmartParseTests (httpSmartParserTests)
import ObjectParseTests (parserTests)
import ObjectTests (objectTests)
import Test.HUnit

tests :: Test
tests =
    TestList $
        parserTests
            <> objectTests
            <> httpSmartParserTests

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()
