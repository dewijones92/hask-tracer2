module Vetchina.Main
    ( main
    ) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Char
import Data.Maybe

import Data.Foldable
import Data.Maybe
import qualified Data.ByteString as BS
import Graphics.Gloss (text)
import System.Directory
import Data.List (sortBy)
import Text.Printf
import Data.Function
import qualified GHC.Base as IO

newtype Word'= Word' T.Text deriving (Show, Read, Eq, Ord)

mkWord :: T.Text -> Word'
mkWord = Word' . T.toUpper

wordToText :: Word' -> T.Text
wordToText (Word' t) = t

normalizeTextToWords :: T.Text -> [Word']
normalizeTextToWords =
    map mkWord .
    T.words .
    T.map (\x ->
                    if isAlphaNum x
                        then x
                        else ' ')

newtype Bow = Bow  { bowToMap :: M.Map Word' Int } deriving (Show, Read )

wordToBow :: Word' -> Bow
wordToBow w = Bow $ M.fromList [(w,1)]

textToBow :: T.Text -> Bow
textToBow = foldMap wordToBow . normalizeTextToWords

wordsCount :: Bow -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

summaryBow :: Bow -> IO ()
summaryBow (Bow bow) = do
    forM_ (sortBy (compare `on` snd) $ M.toList bow) $
         \(w,f) -> printf "%s -> %d\n" (wordToText w) f

wordProbability :: Word' -> Bow -> Float
wordProbability w bow = fromIntegral n / fromIntegral (wordsCount bow)
    where n = fromMaybe 0 $ M.lookup w $ bowToMap bow

seenWord :: SpamFilter -> Word' -> Bool
seenWord (SpamFilter (Bow spamBow) (Bow hamBow)) w =  isJust sm || isJust hm
 where
     sm = M.lookup w  spamBow
     hm = M.lookup w  hamBow



emptyBow = Bow M.empty

instance Semigroup Bow where
    Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where
    mempty = emptyBow

bowFromFile :: FilePath -> IO Bow
bowFromFile  filePath = textToBow. E.decodeUtf8
                                             <$> BS.readFile filePath

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath = do
    fileNames <- listDirectory folderPath
    bows <- mapM (bowFromFile . (folderPath <>)) fileNames
    return $ fold bows

baseDir = "/home/dewi/Downloads/Email-Classification-Spam-or-Ham/E-mail_Classification/"

data SpamFilter = SpamFilter { spamBow :: Bow,
                                                                    hamBow :: Bow}

spamFilter :: IO SpamFilter
spamFilter =  do
    spam <- bowFromFolder (baseDir ++ "train/spam/")
    ham <-   bowFromFolder (baseDir ++ "train/ham/")
    return $ SpamFilter spam ham



wordProbabilitySpam :: SpamFilter -> Word' -> Maybe Float
wordProbabilitySpam sf@(SpamFilter spamBow hamBow) w
    | seenWord sf w =
        let
            pws = wordProbability w spamBow
            phs = wordProbability w hamBow
            ps = pws + phs
        in Just (pws / (pws/phs))
    | otherwise = Nothing

textProbabilitySpam :: T.Text -> SpamFilter -> Float
textProbabilitySpam text sm = pp / (pp + product ips)
        where
            ws = normalizeTextToWords text
            ps = mapMaybe (wordProbabilitySpam sm) ws
            ips = map (1.0 -) ps
            pp = product ps

main :: IO ()
main  = do
    undefined