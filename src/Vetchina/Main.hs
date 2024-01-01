module Vetchina.Main
    ( main
    ) where

import qualified Data.Map as M
import Data.Foldable
import Data.Maybe
 
newtype Bow = Bow  { bowToMap :: M.Map String Int } deriving (Show )

wordToBow :: String -> Bow
wordToBow w = Bow $ M.fromList [(w,1)]
 
wordsCount :: Bow -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

wordProbability :: String -> Bow -> Float
wordProbability w bow = fromIntegral n / fromIntegral (wordsCount bow)
    where n = fromMaybe 0 $ M.lookup w $ bowToMap bow

emptyBow = Bow M.empty

instance Semigroup Bow where
    Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where 
    mempty = emptyBow

main :: IO ()
main  = undefined 