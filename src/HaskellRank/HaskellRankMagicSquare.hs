module HaskellRank.HaskellRankMagicSquare
    ( main , solve
    ) where
import Data.Semigroup
import RIO.List (maximumMaybe)
import Control.Monad
import Data.Maybe
import Data.List
import Data.List (permutations)
import Data.List (transpose)

type Square = [[Int]]

allMagic :: [Square]
allMagic = undefined

pp :: Square -> IO ()
pp = putStrLn . unlines . map (unwords . map show)

-- Function to generate all rotations and reflections of a square
allRotationsAndReflections :: Square -> [Square]
allRotationsAndReflections sq = [f r | r <- take 4 (iterate rotate90 sq), f <- [id, reverse, map reverse, reverse . map reverse]]
  where
    rotate90 = reverse . transpose

magic :: Square
magic = [[8,1,6],
                [3,5,7],
                [4,9,2]]
rot90 :: Square -> Square
rot90 = undefined

refl :: Square -> Square
refl = undefined


solve :: Int
solve = undefined


main :: IO ()
main = do
    square <- replicateM 3 readLn :: IO [[Int]]
    putStrLn $ show $ 3