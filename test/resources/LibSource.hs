module LibSource
    ( factorial
    , fibonacci
    , quicksort
    , gcdEuclidean
    ) where

-- Factorial Function
-- Given a non-negative integer n, returns n! (n factorial)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Fibonacci Function
-- Given an index n, returns the nth Fibonacci number
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Quicksort Function
-- Given a list of comparable elements, sorts the list
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- GCD using Euclidean Algorithm
-- Given two integers a and b, returns their greatest common divisor
gcdEuclidean :: Integer -> Integer -> Integer
gcdEuclidean a 0 = a
gcdEuclidean a b = gcdEuclidean b (a `mod` b)