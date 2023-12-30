module HaskellRank.HaskellRankJsonSpec (spec) where
import Test.Hspec ( describe, it, Spec )
import Test.Hspec
import Test.HUnit
import Test.QuickCheck

import JsonParser.Main
import JsonParser.Main
import JsonParser.Main

-- Test for Identity Law
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

-- Test for Composition Law
functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x = (fmap g . fmap f $ x) == fmap (g . f) x

-- Instances for Arbitrary Compose (needed for QuickCheck)
instance (Arbitrary (f (g a)), Functor f, Functor g) => Arbitrary (Compose f g a) where
    arbitrary = Compose <$> arbitrary

-- Helper function to test Functor instance
testFunctor :: IO ()
testFunctor = do
    quickCheck (functorIdentity :: Compose [] Maybe Int -> Bool)
    quickCheck (functorCompose :: Fun Int Int -> Fun Int Int -> Compose [] Maybe Int -> Bool)

main :: IO ()
main = testFunctor

