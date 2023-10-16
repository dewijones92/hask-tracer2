module LibSource
    ( factorial
    ) where

-- Factorial Function
-- Given a non-negative integer n, returns n! (n factorial)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
