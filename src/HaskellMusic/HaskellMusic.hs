module HaskellMusic.HaskellMusic
    ( main
    ) where

import qualified Data.ByteString.Lazy as B.L
import qualified Data.ByteString.Builder as B.B
import qualified Data.Foldable as Data
import Data.Foldable

volume :: Float
volume = 0.5

wave :: [Float]
wave = map (*volume) $ map sin $ map (*step) [0.0 .. 4800]
  where step = 0.01

save :: IO ()
save = B.L.writeFile "output.bin" $ B.B.toLazyByteString $ fold $ map B.B.floatBE  wave

main :: IO ()
main = undefined