module Tracers.Trace5
    (
        parseHaskellFile, insertTraceShow5
    ) where

import Language.Haskell.Exts -- This module is from haskell-src-exts

-- Function to insert traceShow in function definitions
import Data.List (isInfixOf)

filePath = "src/Source/SourceLib5.hs"

parseHaskellFile :: IO ()
parseHaskellFile = do
    content <- readFile filePath
    let parsedResult = parseModule content
    case parsedResult of
        ParseOk ast -> print ast
        ParseFailed srcLoc err -> putStrLn $ "Parsing failed at " ++ show srcLoc ++ ": " ++ err



insertTraceShow5 :: IO String
insertTraceShow5 = do 
  content <- readFile filePath
  return . unlines . map processLine . lines $ content
  where
    processLine line
      | "::" `isInfixOf` line = line
      | otherwise = "traceShow (" ++ show line ++ ", " ++ line ++ ") " ++ line


outputPath = "test/resources/.bin/HERE.hs"

main :: IO ()
main = do
    putStrLn "Enter the file name: "
    result <- insertTraceShow5
    writeFile outputPath result
    putStrLn "Modified file has been saved."
