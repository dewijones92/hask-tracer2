{-# LANGUAGE OverloadedStrings #-}
module Lib2
    (factorialParser
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void (Void)

type Parser = Parsec Void Text

data FactorialFunction = FactorialFunction
    { functionName :: String
    , parameter    :: String
    , returnType   :: String
    , definition   :: String
    } deriving (Show)

factorialParser :: Parser FactorialFunction
factorialParser = do
    _ <- string "factorial :: "
    returnType <- many (noneOf ['\n'])
    _ <- char '\n'
    _ <- string "factorial "
    param <- many (noneOf ['='])
    _ <- string " = "
    definition <- many (noneOf  ['\n'])
    return $ FactorialFunction "factorial" param returnType definition

go :: IO ()
go = do
    let code = "factorial :: Integer -> Integer\nfactorial 0 = 1"
    print $ parse factorialParser "" code
