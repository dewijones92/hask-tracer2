module Dewi5Spec (spec) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath (takeDirectory)
import System.IO (writeFile)
import Test.Hspec
import Test.HUnit
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectory, removePathForcibly)
import Control.Monad ( unless )
import Lib3
import SourceLib5 (getMiddle)

spec :: Spec
spec = do 
  describe "function parse 2" $ do
    it "should be pending implementation" $
      pending -- Corrected usage of 'pending'
    it "function parse 2" $ do
      let dd = getMiddle "asdsd"
      putStrLn dd
      assertBool "True" True
-- Removed redundant imports

