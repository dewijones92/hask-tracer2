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
type Semitones = Float
type Pulse = Float

volume :: Float
volume = 0.5

sampleRate :: Samples
sampleRate = 48000

pitchStandard :: Hz
pitchStandard = 440.0

frequency :: Float
frequency = 440 -- Middle A

f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Semitones -> Seconds -> [Pulse]
note n duration = freq (f n) duration

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map ((*volume).sin.(*step)) [0.0 .. sampleRate * duration]
  where
    step = (hz * 2 * pi) / sampleRate

wave :: [Pulse]
wave = concat $ [ note  (2*i) duration| i <- [0..10]]
  where duration = 1.0




save :: IO ()
save = B.L.writeFile "output.bin" $ B.B.toLazyByteString $ fold $ map B.B.floatBE wave

main :: IO ()
main = save
