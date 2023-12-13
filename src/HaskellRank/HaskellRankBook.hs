module HaskellRank.HaskellRankBook
    (
    ) where
import Control.Monad


newtype PageCount = PageCount Int deriving (Eq, Show)
newtype PageNumber = PageNumber Int deriving (Eq, Show)

solve :: PageCount -> PageNumber -> PageCount
solve totalNumberOfPages targetPageNumber = PageCount $ min pageTurnsFromFront pageTurnsFromBack
 where pageTurnsFromFront = (totalNumberOfPages `div` 2) - totalNumberOfPages
             pageTurnsFromBack = totalNumberOfPages `div` 2


main :: IO ()
main = do
    [numberOfPages, targetPage] <- replicateM 2 readLn :: IO [Int]
    putStrLn $ show $ solve (PageCount numberOfPages) (PageNumber targetPage)
