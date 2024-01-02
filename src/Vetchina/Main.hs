module Vetchina.Main
    ( main
    ) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Data.Foldable
import Data.Maybe
import qualified Data.ByteString as T
import Graphics.Gloss (text)
import System.Directory
 
newtype Bow = Bow  { bowToMap :: M.Map T.Text Int } deriving (Show, Read )

wordToBow :: T.Text -> Bow
wordToBow w = Bow $ M.fromList [(w,1)]

textToBow :: T.Text -> Bow
textToBow = foldMap wordToBow . T.words
 
wordsCount :: Bow -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

wordProbability :: T.Text -> Bow -> Float
wordProbability w bow = fromIntegral n / fromIntegral (wordsCount bow)
    where n = fromMaybe 0 $ M.lookup w $ bowToMap bow

emptyBow = Bow M.empty

instance Semigroup Bow where
    Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where 
    mempty = emptyBow

bowFromFile :: FilePath -> IO Bow
bowFromFile  filePath = textToBow. E.decodeUtf8
                                             <$> T.readFile filePath

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath = do
    fileNames <- listDirectory folderPath
    bows <- mapM (bowFromFile . (folderPath <>)) fileNames
    return $ fold bows

baseDir = "/home/dewi/Downloads/Email-Classification-Spam-or-Ham/E-mail_Classification/"

spamBow :: IO Bow
spamBow = bowFromFolder  (baseDir ++ "train/spam/")

hamBow :: IO Bow
hamBow = bowFromFolder  (baseDir ++ "train/ham/")

main :: IO ()
main  = do
    undefined