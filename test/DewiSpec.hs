module DewiSpec (spec) where

import Test.Hspec
import Test.HUnit
import Lib (insertTraceShows)

spec :: Spec
spec = do
  describe "Trace insertion" $  it "reads from LibSource.hs, adds trace, and checks output" $ do
      pending

      -- Read the original source code
      originalCode <- readFile "test/resources/LibSource.hs"
      putStrLn originalCode

      -- Add trace using your library function
      let tracedCode = insertTraceShows originalCode
      putStrLn tracedCode

      -- Write the traced code to a new file 
      
      let outputPath = "test/resources/.bin/HERE.hs"
      writeFile outputPath tracedCode

      -- Read the newly generated file and assert
      generatedCode <- readFile outputPath
      assertBool "Trace code should be inserted correctly" (validateTraceInsertion generatedCode)


-- Check if trace statements have been correctly inserted
validateTraceInsertion :: String -> Bool
validateTraceInsertion generatedCode = "traceShow" `elem` words generatedCode

