module HaskellRank.HaskellRankValley
    ( main, countingValleys, solve
    ) where
import Control.Monad (replicateM)

-- Define custom data types
-- Define custom data types
newtype Altitude = Altitude Int deriving (Eq, Show)
data Step = Up | Down deriving (Eq, Show)

-- Convert Char to Step
charToStep :: Char -> Maybe Step
charToStep 'U' = Just Up
charToStep 'D' = Just Down
charToStep _   = Nothing

-- Main function with enhanced type safety
countingValleys :: Altitude -> [Step] -> Int
countingValleys _ [] = 0
countingValleys (Altitude altitude) (step:path) =
    let newAltitude = case step of
                        Up   -> Altitude (altitude + 1)
                        Down -> Altitude (altitude - 1)
        addValley = if altitude == -1 && step == Up then 1 else 0
    in addValley + countingValleys newAltitude path



solve :: String -> String
solve input = do
    let steps = [step | Just step <- map charToStep input]
    let initialAltitude = Altitude 0
    show $ countingValleys initialAltitude steps

main :: IO ()
main = do
    [_, terrain] <- replicateM 2 readLn
    putStrLn $ solve terrain
