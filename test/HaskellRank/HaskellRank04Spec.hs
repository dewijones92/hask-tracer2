module HaskellRank.HaskellRank04Spec (spec) where
import Test.Hspec ( describe, it, Spec )
import Test.Hspec
import Test.HUnit

import HaskellRank.HaskellRank04 ( getMiddle )

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

spec :: Spec
spec = do 
  describe "get middle" $ do
    it "odd length string" $ do
      let content = getMiddle "asdasd"
      assertEqual'  "da" content
    it "even length string" $ do
      let content = getMiddle "asdas"
      assertEqual'  "d" content
          -- Removed or commented out redundant imports

