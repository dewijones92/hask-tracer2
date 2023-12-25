module HaskellRank.HaskellMonad
    (  main
    ) where

import Data.Array

import Data.Array
import Data.Foldable ( asum )
import Data.Monoid
import Data.Semigroup

data World = World

type WorldT a = World -> (a, World)
readStrT :: WorldT  String
readStrT = readStr

printStrT :: String -> WorldT  ()
printStrT s w = ((), printStr s w)

printStr :: String -> World -> World
printStr = undefined

readStr :: World -> (String, World)
readStr = undefined

main :: IO ()
main = do
    putStrLn "hey" 