module Parser.Utils (parse, parseAndAnalyze, parseFile, debug) where

import Common.AST (AST)
import Common.PrintAST (pretty)
import Lexer.Lexer (AlexPosn)
import Parser.Parser (calc)
import Parser.ParserM (Error, parseString)
import Parser.ParserState (ParserState(..))
import Semantics.Semantics (SemanticTag, analyzeAST)

-- The parsing function
parse :: String -> Either Error (AST AlexPosn)
parse = fst . parseString calc

-- The parsing and semantic analysis function
parseAndAnalyze :: String -> (Either Error (AST SemanticTag), ParserState)
parseAndAnalyze = parseString $ calc >>= analyzeAST

-- Util function for debugging end to end
debug :: String -> IO ()
debug s = do
  let (res, state) = parseAndAnalyze s
  putStrLn "Symbol Table"
  putStrLn $ pretty (symbols state)
  putStrLn ""
  print res

-- Parsing utils for files
parseFile :: FilePath -> IO ()
parseFile f = do
  s <- readFile f
  let res = parse s
  case res of
    Left err -> print err
    Right p  -> print p
