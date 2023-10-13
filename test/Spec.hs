import System.IO.Silently (capture_)
import System.Exit (exitFailure)
import Test.HUnit
import Lib (someFunc, insertTraceShows, addTrace)
import Language.Haskell.Exts
import Data.List (tails, isPrefixOf)  -- <-- Added this import

-- Helper function to check if a string contains a substring
contains :: String -> String -> Bool
contains str substr = any (== substr) (tails str)

-- Helper function to check if a string represents an error
isError :: String -> Bool
isError = ("Error" `isPrefixOf`)

-- Test for someFunc
testSomeFunc :: Test
testSomeFunc = TestCase $ do
    output <- capture_ someFunc
    assertEqual "Should print 'someFunc\\n'" "someFunc\n" output

-- Test for insertTraceShows
testInsertTraceShows :: Test
testInsertTraceShows = TestList [
    TestCase $ do
        let input = "main = print 42"
        let output = insertTraceShows input
        assertBool "Should insert trace statements" (output `contains` "traceShow"),
    TestCase $ do
        let input = "main = print 42;"
        let output = insertTraceShows input
        assertBool "Should fail on syntax errors" (isError output)
  ]

-- Test for addTrace
testAddTrace :: Test
testAddTrace = TestCase $ do
    let input = parseExp "x + y"
    let output = case input of
                    ParseOk expr -> prettyPrint (addTrace expr)
                    ParseFailed _ _ -> "Error"
    assertEqual "Should add trace to expression" "traceShow (x + y) (x + y)" output

main :: IO ()
main = do
    counts <- runTestTT $ TestList [testSomeFunc, testInsertTraceShows, testAddTrace]
    if errors counts + failures counts == 0
        then return ()
        else exitFailure  -- You need to import System.Exit for this
