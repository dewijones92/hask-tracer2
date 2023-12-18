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
      let maxPrice = solve [[4,9,2], [3,5,7], [8,1,5]]
      assertEqual "should be nothing" 1 maxPrice

