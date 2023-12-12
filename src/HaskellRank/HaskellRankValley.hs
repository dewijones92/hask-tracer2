module HaskellRank.HaskellRankValley
    ( main, countingValleys
    ) where

countingValleys :: Int -> [Char] -> Int
countingValleys _ [] = 0
countingValleys altitude (step:path) = 
    let newAltitude = if step == 'U' then altitude + 1 else altitude - 1
        addValley = if altitude == -1 && step == 'U' then 1 else 0
    in addValley + countingValleys newAltitude path



main :: IO ()
main = do
    inputLine <- readLn
    putStrLn $ show $ countingValleys 0 inputLine

