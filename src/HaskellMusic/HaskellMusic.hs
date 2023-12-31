module HaskellMusic.HaskellMusic
    ( main
    ) where

import qualified Data.ByteString.Lazy as B.L
import qualified Data.ByteString.Builder as B.B
import qualified Data.Foldable as Data
import Data.Foldable
import Test.QuickCheck (sample)

type Seconds = Float
type Samples = Float
type Hz = Float
type Pulse = Float

volume :: Float
volume = 0.5

sampleRate :: Samples
sampleRate = 48000

frequency :: Float
frequency = 440 -- Middle A



freq :: Hz -> Seconds -> [Pulse]
frez hz duration = map ((*volume).sin.(*step)) [0.0 .. sampleRate * duration]
  where
    step = (hz * 2 * pi) / sampleRate

wave :: [Pulse]
wave = freq hz duration
  where 
    duration :: Seconds
    duration = 2.0
    step = (hz * 2 * pi) / sampleRate
    hz :: Float
    hz = 440.0



save :: IO ()
save = B.L.writeFile "output.bin" $ B.B.toLazyByteString $ fold $ map B.B.floatBE wave

main :: IO ()
main = save
