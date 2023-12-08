module Source.SourceLib5
    ( getMiddle
    ) where

getMiddle :: String -> String
getMiddle sourceString
  | isOddLength  = takeMiddle 1
  | otherwise = takeMiddle 2
  where
    sourceLength = length sourceString
    isOddLength = odd sourceLength
    middleIndex = sourceLength `div` 2
    takeMiddle n
      | isOddLength = [sourceString !! middleIndex]
      | otherwise   = take n . drop (middleIndex - 1) $ sourceString

