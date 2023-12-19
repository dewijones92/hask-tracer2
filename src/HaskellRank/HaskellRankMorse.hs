module HaskellRank.HaskellRankMorse
    ( main , solve
    ) where
import Data.Semigroup
import RIO.List (maximumMaybe)
import Control.Monad
import Data.Maybe
import Data.List
import Data.List (permutations)
import Data.List (transpose)
import Data.Char
import Data.Function
import RIO
import Control.Applicative (Alternative(empty))
import qualified Data.Map as DM

morseCodes :: DM.Map String String
morseCodes = DM.fromList [("....", "M"), (".", "M"), ("-.q--", "M"), ("-.--", "M"), ("..-", "M"), ("-....", "M"), (".", "M"), (".---", "M"), ("-..", "M")]


testdata = ".... . -.... .--- ..- -.. ."

solve :: String -> String
solve  = fromMaybe "" . fmap concat . sequence . map (flip DM.lookup morseCodes ) . filter (/= " ") . groupBy ((==) `on` isSpace)


main :: IO ()
main = do
    putStrLn "2"