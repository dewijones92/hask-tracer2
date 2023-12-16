module HaskellRank.HaskellRankElectronics
    ( main , solve
    ) where
import Data.Semigroup

solve :: (Num a, Ord a) =>
 a -- buget
 -> [a] -- keyboards
 -> [a] -- drives
 -> Maybe a -- most expensive combo
solve buget keyboards drives
 | null allPossiblePrices = Just maxComboCanAfford
 | otherwise = Nothing
 where
    allPossiblePrices =  [k + d | k <- keyboards, d <- drives]
    maxComboCanAfford = maximum $ filter (< buget) allPossiblePrices

main :: IO ()
main = do
    putStrLn $ show $ solve 2 [1..10] [1..10]
