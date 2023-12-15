module HaskellRank.HaskellRankBook
    (
        solve, PageCount, PageNumber, main
    ) where

import Control.Monad

newtype StkType a = Stk [a]  

newtype PageCount = PageCount Int deriving (Eq, Show)
newtype PageNumber = PageNumber Int deriving (Eq, Show)







solve :: PageCount -> PageNumber -> PageCount
solve (PageCount totalPages) (PageNumber targetPage) = PageCount $ min pageTurnsFromFront pageTurnsFromBack
  where
    pageTurnsFromFront = targetPage `div` 2
    pageTurnsFromBack = if even totalPages 
                        then (totalPages - targetPage) `div` 2
                        else (totalPages - targetPage - 1) `div` 2

main :: IO ()
main = do
    let result = solve (PageCount 3) (PageNumber 4)
    let ff = case result of PageCount n -> n
    [numberOfPages, targetPage] <- replicateM 2 readLn :: IO [Int]
    putStrLn $ show $ solve (PageCount numberOfPages) (PageNumber targetPage)