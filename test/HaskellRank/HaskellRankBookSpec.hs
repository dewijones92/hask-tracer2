module HaskellRank.HaskellRankBookSpec (spec) where
import Test.Hspec ( describe, it, Spec )
import Test.Hspec
import Test.HUnit

import HaskellRank.HaskellRankBook ( solve, PageCount, PageNumber )

assertEqual' :: (Eq a, Show a) => a -> a -> Assertion
assertEqual' = assertEqual ""

spec :: Spec
spec = do 
  describe "go" $ do
    it "odd length string" $ do
      let result = solve (PageCount 3) (PageNumber 4)
      let ff = case result of PageCount n -> n

      assertEqual' 10 ff
