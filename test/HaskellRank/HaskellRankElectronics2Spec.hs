module HaskellRank.HaskellRankElectronics2Spec (spec) where
import Test.Hspec ( describe, it, Spec )
import Test.Hspec
import Test.HUnit
import Test.QuickCheck

import HaskellRank.HaskellRankElectronics ( solve2 )

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

spec :: Spec
spec = do 
  describe "testing most expensive combo that I can afford" $ do
    it "Buget too low" $ do
      let maxPrice = solve2 1 [1..10] [1..100]
      assertEqual "should be nothing" Nothing maxPrice
    it "Empty keyboard and drives should produce Nothing" $ do
      let maxPrice = solve2 1 [] []
      assertEqual "should be nothing" Nothing maxPrice
    it "She should be able to afford" $ do
      let maxPrice = solve2 60 [40,50,60] [5,8,12]
      assertEqual "yes" (Just 58) maxPrice
    it "satisfies the max budget constraint" $
      quickCheck prop_MaxBudgetConstraint `shouldReturn` ()

prop_MaxBudgetConstraint :: Integer -> [Integer] -> [Integer] -> Property
prop_MaxBudgetConstraint budget keyboards drives = 
    property $ 
        case solve2 budget keyboards drives of
            Just total -> total <= budget
            Nothing -> True


