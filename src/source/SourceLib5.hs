module SourceLib5
    ( getMiddle2
    ) where



getMiddle2 :: String -> String
getMiddle2 sourceString
  | isOddLength  = takeMiddle 1
  | otherwise = takeMiddle 2
  where
    sourceLength = length sourceString
    isOddLength = sourceLength `mod` 2 /= 0
    middleIndex = sourceLength `div` 2
    takeMiddle n
      | isOddLength = [sourceString !! middleIndex]
      | otherwise   = take n . drop (middleIndex - 1) $ sourceString

