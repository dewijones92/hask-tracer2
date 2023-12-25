module HaskellRank.HaskellMonoid
    (  main
    ) where

import Data.Array

import Data.Array
import Data.Foldable 
import Data.Monoid
import Data.Semigroup
import Data.Char
import Data.List


isBraille :: Char -> Bool
isBraille = undefined

isEmoji :: Char -> Bool
isEmoji = undefined

isForbidden :: Char -> Bool
isForbidden x = isBraille x || isEmoji x

f :: (Char ->  Bool) -> (Char -> Bool) -> Char -> Bool
f = undefined


data DewiNonEmpty a = a :^^^ [a]
    deriving ( Eq  -- ^ @since 4.9.0.0
           , Ord -- ^ @since 4.9.0.
           , Show
           )




hh :: Int -> String
hh  x
    | x == 2 = "is 2"
    | x /= 2 = "not 2"

type Task = (String, Bool)  -- (Task Description, Is Completed?)

tasks :: [Task]
tasks = [("Write code", True), ("Review PR", False), ("Team meeting", True), ("Documentation", False)]

-- Function to check for incomplete tasks
hasIncompleteTasks :: [Task] -> Bool
hasIncompleteTasks = not . all snd





main = do
    putStrLn "asd"