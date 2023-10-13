import qualified System.Exit as Exit
 
import Control.Exception  -- Imports everything
import Control.Exception (ArithException(..))

import Control.Monad (guard)
import Lib (insertTraceShows, addTrace)  -- Importing functions from your main code
import Language.Haskell.Exts
import Data.Generics

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Silently (capture_)
import Language.Haskell.Exts
import Lib (someFunc, insertTraceShows, addTrace)  -- Import your Lib module here

main :: IO ()
main = defaultMain $ testGroup "Lib Tests"
    [ testCase "test someFunc" testSomeFunc
    , testCase "test insertTraceShows" testInsertTraceShows
    , testCase "test addTrace" testAddTrace
    ]

testSomeFunc :: Assertion
testSomeFunc = do
    output <- capture_ someFunc
    output @?= "someFunc\n"

testInsertTraceShows :: Assertion
testInsertTraceShows = do
    let input = "let x = 5 in x + 3"
    let expected = "your expected output here"  -- Specify the expected output
    let output = insertTraceShows input
    output @?= expected

testAddTrace :: Assertion
testAddTrace = do
    let inputExp = parseValidExp "5 + 3"
    let expectedExp = parseValidExp "traceShow (5 + 3) (5 + 3)"
    let outputExp = addTrace inputExp
    prettyPrint outputExp @?= prettyPrint expectedExp

-- Utility function to parse an expression and assume it's valid.
parseValidExp :: String -> Exp SrcSpanInfo
parseValidExp str = case parseExp str of
    ParseOk exp -> exp
    ParseFailed srcLoc err -> error $ show srcLoc ++ ": " ++ err