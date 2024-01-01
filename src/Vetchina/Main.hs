module Vetchina.Main
    ( main
    ) where

import qualified Data.Map as M
import qualified Data.Text as T
 
newtype Bow = Bow  { bowToMap :: M.Map T.Text Int } deriving (Show)

wordToBow :: T.Text -> Bow
wordToBow w = Bow $ M.fromList [(w,1)]

emptyBow = Bow M.empty

instance Semigroup Bow where
    Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where 
    mempty = emptyBow

main :: IO ()
main  = undefined 