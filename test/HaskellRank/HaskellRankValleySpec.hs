module HaskellRank.HaskellRankValleySpec (spec) where
import Test.Hspec ( describe, it, Spec )
import Test.Hspec
import Test.HUnit

import HaskellRank.HaskellRankValley ( solve )

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

spec :: Spec
spec = do 
  describe "go" $ do
    it "odd length string" $ do
      let valleyCount = solve "DUUDUUUUUD"
      assertEqual'  "10" valleyCount

