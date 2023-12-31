module HaskellMusic.HaskellMusic
    ( main
    ) where

import qualified Data.ByteString.Lazy as B.L
import qualified Data.ByteString.Builder as B.B
import qualified Data.Foldable as Data
import Data.Foldable

volume :: Float
volume = 0.5

sampleRate :: Float
sampleRate = 48000

frequency :: Float
frequency = 440 -- Middle A

wave :: [Float]
wave = map (*volume) $ map sin $ map (*step) [0.0 .. sampleRate]
  where step = 2 * pi * frequency / sampleRate

save :: IO ()
save = B.L.writeFile "output.bin" $ B.B.toLazyByteString $ fold $ map B.B.floatBE wave

main :: IO ()
main = save
