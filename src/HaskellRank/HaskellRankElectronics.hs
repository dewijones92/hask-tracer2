module HaskellRank.HaskellRankElectronics
    ( main , solve, solve2
    ) where
import Data.Semigroup
import RIO.List (maximumMaybe)
import Control.Monad
import Data.Maybe
import Data.List

solve :: (Num a, Ord a) => a -> [a] -> [a] -> Maybe a
solve budget keyboards drives = maximumMaybe [k + d | k <- keyboards, d <- drives, k + d <= budget]

solve2 :: (Num a, Ord a) => a -> [a] -> [a] -> Maybe a
solve2 buget keyboards drives = listToMaybe  $ filter (<= buget) $ sortBy (flip compare) $ liftM2 (+) keyboards drives

main :: IO ()
main = do
    [[buget, _], keyboards, drives] <- replicateM 3 readLn :: IO [[Int]]
    putStrLn $ show $ solve2 buget keyboards drives