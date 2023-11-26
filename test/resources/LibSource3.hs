module LibSource
    ( getMiddle
    ) where

import Debug.Trace

getMiddle :: String -> String
getMiddle sourceString  
  | traceShow ("Checking isOddLength:", isOddLength) isOddLength  =  
      traceShow ("Taking middle 1:", takeMiddle 1) takeMiddle 1
  | otherwise = 
      traceShow ("Taking middle 2:", takeMiddle 2) takeMiddle 2
 where 
  sourceLength = traceShow ("Source Length:", length sourceString) length sourceString
  isOddLength = traceShow ("Is Odd Length:", sourceLength `mod` 2 /= 0) (sourceLength `mod` 2 /= 0)
  middleIndex = traceShow ("Middle Index:", sourceLength `div` 2) (sourceLength `div` 2)
  takeMiddle n = traceShow ("Take Middle " ++ show n ++ ":", take n . drop (middleIndex - if n == 1 then 0 else 1) $ sourceString) 
                 (take n . drop (middleIndex - if n == 1 then 0 else 1) $ sourceString)

