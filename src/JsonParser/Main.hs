{-# LANGUAGE LambdaCase #-}
module JsonParser.Main
    ( main
    ) where
import Data.Char (isDigit)
import Data.Char (isDigit, digitToInt)
import Text.Read.Lex (Number)
import Control.Arrow

data JsonValue =  JsonNull
                               | JsonBool Bool
                               | JsonNumber Integer
                               | JsonString String
                               | JsonArray [JsonValue]
                               | JsonObject [(String, JsonValue)]
                               deriving (Show, Eq)

jsonValue :: Parser JsonValue
jsonValue = undefined

--stringP :: [Char] -> Parser [Char]
--stringP = sequenceA . map charP

instance Functor Parser where
      fmap f (Parser p) = Parser $ \input -> fmap (second f) (p input)



newtype Parser a =
     Parser { runParser :: String -> Maybe (String, a) }

jsonNull :: Parser JsonValue
jsonNull = undefined

charP :: Char -> Parser Char
charP x = Parser $ \case
      y:ys | y == x -> Just (ys, x)
      _ -> Nothing

-- Parses a single digit and returns the rest of the string
digitParser :: Parser Int
digitParser = Parser $ \input -> case input of
    (x:xs) | isDigit x -> Just (xs, digitToInt x)
    _ -> Nothing


dewi :: Int -> Int
dewi p =
    let valuex = 1
        valuey = 2
    in valuex + valuey


main :: IO ()
main = undefined