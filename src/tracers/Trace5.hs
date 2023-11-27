module Lib4
    ( someFunc, insertTraceShows, addTrace
    ) where


import Debug.Trace (traceShowWith)

getMiddle2 :: String -> String
getMiddle2 sourceString  
  | traceShowWith id isOddLength = takeMiddle 1
  | otherwise = takeMiddle 2
 where 
  sourceLength = traceShowWith id $ length sourceString 
  middleIndex = traceShowWith id $ sourceLength `div` 2
  isOddLength = traceShowWith id $ sourceLength `mod` 2 == 1
  takeMiddle n = traceShowWith id $ take n . drop (middleIndex - (n `div` 2)) $ sourceString
