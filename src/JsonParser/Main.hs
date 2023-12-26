{-# LANGUAGE LambdaCase #-}
module JsonParser.Main
    ( main
    ) where
import Data.Char
import Data.Char (isDigit, digitToInt)
import Text.Read.Lex (Number)
import Control.Arrow
import GHC.IO.Handle (NewlineMode(inputNL))
import Control.Applicative
import Text.Megaparsec.Byte (string)

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
      fmap f (Parser p) = Parser $ \input -> do
                        (input', x) <- p input
                        return (input', f x)


instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =Parser $  \input -> do
                                                    (input', f ) <- p1 input
                                                    (input'', a) <- p2 input'
                                                    Just (input'', f a)

newtype Parser a =
     Parser { runParser :: String -> Maybe (String, a) }

jsonNull :: Parser JsonValue
jsonNull =  (\_ -> JsonNull) <$> stringP "null"

charP :: Char -> Parser Char
charP x = Parser $ \case
      y:ys | y == x -> Just (ys, x)
      _ -> Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

-- Parses a single digit and returns the rest of the string
digitParser :: Parser Int
digitParser = Parser $ \input -> case input of
    (x:xs) | isDigit x -> Just (xs, digitToInt x)
    _ -> Nothing


-- Sample parser that parses an integer
parseInt :: Parser Int
parseInt = Parser $ \s -> Just (tail s, read [head s])



main :: IO ()
main = undefined
