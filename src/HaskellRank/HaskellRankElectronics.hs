module HaskellRank.HaskellRankElectronics
    ( main , solve
    ) where
import Data.Semigroup
import RIO.List (maximumMaybe)

solve :: (Num a, Ord a) => a -> [a] -> [a] -> Maybe a
solve budget keyboards drives = maximumMaybe [k + d | k <- keyboards, d <- drives, k + d <= budget]



main :: IO ()
main = do
    putStrLn $ show $ solve 2 [1..10] [1..10]
