module HaskellRank.HaskellRankJsonSpec (spec) where
import Test.Hspec
import Test.QuickCheck
    ( Testable(property), quickCheck, Arbitrary(arbitrary), Fun(..) )

import JsonParser.Main

-- Test for Identity Law
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

-- Test for Composition Law
functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x = (fmap g . fmap f $ x) == fmap (g . f) x

instance (Eq (f (g a)), Functor f, Functor g) => Eq (Compose f g a) where
    (Compose x) == (Compose y) = x == y


-- Instances for Arbitrary Compose (needed for QuickCheck)
instance (Arbitrary (f (g a)), Functor f, Functor g) => Arbitrary (Compose f g a) where
    arbitrary = Compose <$> arbitrary

-- Helper function to test Functor instance
testFunctor :: IO ()
testFunctor = do
    quickCheck (functorIdentity :: Compose [] Maybe Int -> Bool)
    quickCheck (functorCompose :: Fun Int Int -> Fun Int Int -> Compose [] Maybe Int -> Bool)

spec :: Spec
spec = describe "Your Test Suite" $ do
    -- Here you can define your test cases
    it "should pass the Functor Identity Law" $
        property (functorIdentity :: Compose [] Maybe Int -> Bool)
    it "should pass the Functor Composition Law" $
        property (functorCompose :: Fun Int Int -> Fun Int Int -> Compose [] Maybe Int -> Bool)


