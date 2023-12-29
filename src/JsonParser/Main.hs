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
import Text.Megaparsec.Byte (string, char)
import Text.Read (readMaybe)

data JsonValue =  JsonNull
                               | JsonBool Bool
                               | JsonNumber Integer
                               | JsonString String
                               | JsonArray [JsonValue]
                               | JsonObject [(String, JsonValue)]
                               deriving (Show, Eq)

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element  <*>  many (sep *> element ) <|> pure []


jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *>
                                                 elements
                                                 <* ws <* charP ']')
            where
                elements = sepBy sep jsonValue
                sep = ws *> charP ',' <* ws

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> 
             sepBy (ws *> charP ',' <* ws) pair
                  <* ws <*
                  charP '}')
    where
        pair = (\key _ value -> (key, value)) <$> stringLiteral <*>
               (ws *> charP ':' <* ws) <*>
               jsonValue


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

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
                                                        p1 input <|> p2 input

newtype Parser a =
     Parser { runParser :: String -> Maybe (String, a) }

jsonNull :: Parser JsonValue
jsonNull =  (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool  = f <$> (stringP "true" <|> stringP "false")
                    where
                        f "true" = JsonBool True
                        f "false" = JsonBool False
                        f _ = undefined

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs
            then Nothing
            else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull(spanP isDigit)
  where
    f ds = JsonNumber $ read ds  -- Or JsonError "Invalid number"

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$>  (charP '"' *> stringLiteral <* charP '"')

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
                     let (token, rest) = span f input
                     in Just (rest, token)



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
