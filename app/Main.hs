module Main where
import Lib   -- Import from Lib

main :: IO ()
main = do
    let inputFilePath = "Input.hs"
    let outputFilePath = "Output.hs"
    inputCode <- readFile inputFilePath
    let modifiedCode = insertTraceShows inputCode
    writeFile outputFilePath modifiedCode