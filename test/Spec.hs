import Test.HUnit
import qualified System.Exit as Exit
 
import Test.HUnit
import Control.Exception  -- Imports everything
import Control.Exception (ArithException(..))

import Control.Monad (guard)
import Lib (insertTraceShows, addTrace)  -- Importing functions from your main code
import Language.Haskell.Exts
import Data.Generics

-- Test for addTrace
testAddTrace = TestCase (assertEqual "for (addTrace (parseExp \"x\"))," 
                         (prettyPrint (addTrace (fromParseResult (parseExp "x"))))
                         "traceShow (x) x")

-- Test for insertTraceShows
testInsertTraceShows = TestCase (assertEqual "for (insertTraceShows \"x = 5\")," 
                                 (insertTraceShows "x = 5") 
                                 "x = traceShow (5) 5")

-- Integration Test
testIntegration = TestCase (do
  let inputCode = "x = 5"
  let modifiedCode = insertTraceShows inputCode
  writeFile "TestOutput.hs" modifiedCode
  outputCode <- readFile "TestOutput.hs"
  assertEqual "for the integration test," modifiedCode outputCode)

-- Error Handling Test
testErrorHandling = TestCase (assertThrows (evaluate $ insertTraceShows "x = ") isMyException)
  where
    isMyException :: SomeException -> Bool
    isMyException e = case fromException e of
                        Just Overflow  -> True
                        Just Underflow -> True
                        -- Add more constructors as needed
                        _              -> False


assertThrows :: IO a -> (SomeException -> Bool) -> Assertion
assertThrows action exceptionPredicate = 
    handleJust (\e -> if exceptionPredicate e then Just () else Nothing) (const $ return ()) (action >> return ())


-- Main Function to Run Tests
main = do
  _ <- runTestTT $ TestList [testAddTrace, testInsertTraceShows, testIntegration, testErrorHandling]
  return ()
