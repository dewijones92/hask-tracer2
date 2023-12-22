module HaskellRank.HaskellMonad
    (  main
    ) where

import Data.Array

import Data.Array
import Data.Foldable ( asum )
import Data.Monoid
import Data.Semigroup

whatIsYourName :: IO ()
whatIsYourName = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Your name is: " ++ name

main :: IO ()
main = whatIsYourName