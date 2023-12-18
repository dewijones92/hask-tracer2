module HaskellRank.HaskellRankMagicSquareSpec (spec) where
import Test.Hspec ( describe, it, Spec )
import Test.Hspec
import Test.HUnit
import Test.QuickCheck

import HaskellRank.HaskellRankMagicSquare ( solve )

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

spec :: Spec
spec = do 
  describe "magic square" $ do
    it "Buget too low" $ do
      let maxPrice = solve [[1,2,3], [4,5,6], [7,8,9]]
      assertEqual "should be nothing" 24 maxPrice

