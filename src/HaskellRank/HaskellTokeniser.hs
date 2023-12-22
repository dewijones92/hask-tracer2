module HaskellRank.HaskellTokeniser
    ( 
    ) where

import Data.Array

import Data.Array
import Data.Foldable ( asum )
import Data.Monoid
import Data.Semigroup

type Indexed a = (Min Int, a)
type Indexed2 a = (Int, a)


mkToken :: [Indexed Char] -> Indexed [Char]
mkToken [] = error "The input is empty"
mkToken xs@((i,_):_) = (i, map snd xs)

mkToken2 :: [Indexed Char] -> Indexed [Char]
mkToken2 = sequenceA

token :: [Indexed Char] -> [Indexed String]
token = undefined