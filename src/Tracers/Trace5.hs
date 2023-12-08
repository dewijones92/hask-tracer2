module Tracers.Trace5
    (
        parseHaskellFile, insertTraceShow5
    ) where

import Language.Haskell.Exts -- This module is from haskell-src-exts

-- Function to insert traceShow in function definitions
import Data.List (isInfixOf)

filePath :: String
filePath = "src/Source/SourceLib5.hs"

-- Function to read and parse the Haskell file
parseHaskellFile :: IO ()
parseHaskellFile = do
    content <- readFile filePath
    let parsedResult = parseModule content -- Parses the content of the file
    case parsedResult of
        ParseOk ast -> print ast -- Print the AST if parsing is successful
        ParseFailed srcLoc err -> putStrLn $ "Parsing failed at " ++ show srcLoc ++ ": " ++ err




insertTraceShow5 :: IO ()
insertTraceShow5 = do 
  content <- readFile filePath
  putStrLn $ unlines . map processLine . lines $ content
  where
    processLine line
      | "::" `isInfixOf` line = line
      | otherwise = "traceShow (" ++ show line ++ ", " ++ line ++ ") " ++ line


-- Main function to read, process, and write a Haskell file
main :: IO ()
main = do
    putStrLn "Enter the file name: "
    fileName <- getLine
    content <- readFile fileName
    let modifiedContent = insertTraceShow content
    writeFile (fileName ++ ".modified.hs") modifiedContent
    putStrLn "Modified file has been saved."
