module HaskellRank.HaskellRankElectronicsSpec (spec) where
import Test.Hspec ( describe, it, Spec )
import Test.Hspec
import Test.HUnit

import HaskellRank.HaskellRankElectronics ( solve )

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

spec :: Spec
spec = do 
  describe "testing most expensive combo that I can afford" $ do
    it "Buget too low" $ do
      let maxPrice = solve 1 [1..10] [1..100]
      assertEqual "should be nothing" Nothing maxPrice
    it "Empty keyboard and drives should produce Nothing" $ do
      let maxPrice = solve 1 [] []
      assertEqual "should be nothing" Nothing maxPrice

