module HaskellRank.HaskellRankKangeroo
    ( main
    ) where
import Control.Arrow (ArrowChoice(right))
import Control.Monad (replicateM)
import HaskellRank.HaskellRankBook  as HRB


excludeNth :: Int -> [a] -> [a]
excludeNth n xs = left  ++ tail right
  where (left, right) = splitAt n xs

getList :: Read a => IO [a]
getList = do
  line <- getLine
  return $ map read $ words line


solve :: Int --k
  -> [Int]   -- bill
  -> Int      -- b
  -> Maybe Int
solve k bill b
 | b > actualPrice = Just (b - actualPrice)
 | otherwise = Nothing
 where actualPrice = sum (excludeNth k bill) `div` 2


dewi :: IO ()
dewi = do
    let result = HRB.solve (PageCount 3) (PageNumber 4)
    let ff = case result of HRB.PageCount n -> n
    [numberOfPages, targetPage] <- replicateM 2 readLn :: IO [Int]
    putStrLn $ show $ HRB.solve (PageCount numberOfPages) (PageNumber targetPage)

main :: IO ()
main = do
    [[_, k] , bill , [b]] <- replicateM 3 getList
    let result = solve k bill b
    putStrLn $ maybe "Bon Appetit" show result

