module Lib3
    ( someFunc, insertTraceShows, addTrace
    ) where
import Language.Haskell.Exts
import Data.Generics
import Source.SourceLib5


someFunc :: IO ()
someFunc = putStrLn "someFunc"


insertTraceShows :: String -> String
insertTraceShows code = 
    let parsedModule = parseModule code
     in case parsedModule of
          ParseOk mod -> prettyPrint $ everywhere (mkT addTrace) mod
          ParseFailed srcLoc err -> error $ show srcLoc ++ ": " ++ err

addTrace :: Exp SrcSpanInfo -> Exp SrcSpanInfo
addTrace expr = 
    case parseExp $ "traceShow (" ++ prettyPrint expr ++ ") " ++ prettyPrint expr of
        ParseOk tracedExp -> tracedExp
        ParseFailed srcLoc err -> error $ show srcLoc ++ ": " ++ err