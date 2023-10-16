module DewiSpec (spec) where

import Test.Hspec
import Test.HUnit
import System.IO
import Lib (insertTraceShows)

spec :: Spec
spec = do
  describe "Trace insertion" $ do
    it "reads from LibSource.hs, adds trace, and checks output" $ do

      -- Read the original source code
      originalCode <- readFile "test/resources/LibSource.hs"

      -- Add trace using your library function
      let tracedCode = insertTraceShows originalCode

      -- Write the traced code to a new file
      let outputPath = "test/resources/.bin/HERE"
      writeFile outputPath tracedCode

      -- Read the newly generated file and assert
      generatedCode <- readFile outputPath
      assertBool "Trace code should be inserted correctly" (validateTraceInsertion generatedCode)


-- Check if trace statements have been correctly inserted
validateTraceInsertion :: String -> Bool
validateTraceInsertion generatedCode = "traceShow" `elem` words generatedCode

