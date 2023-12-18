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

rot90 :: Square -> Square
rot90 = map reverse . transpose

pp :: Square -> IO ()
pp = putStrLn . unlines . map (unwords . map show)

magic :: Square
magic = [[8,1,6],
                [3,5,7],
                [4,9,2]]

refl :: Square -> Square
refl = transpose

allMagic :: [Square]
allMagic = (take 4 $ iterate rot90 magic) ++ (take 4 $ iterate rot90 $ refl magic)

distance :: Square -> Square -> Int
distance s1 s2 =  sum $ map abs $ zipWith (-) (concat s1) (concat s2)

solve :: Square -> Int
solve s = minimum $ map (distance s) allMagic 

main :: IO ()
main = do
    square <- replicateM 3 readLn :: IO [[Int]]
    putStrLn $ show $ 3