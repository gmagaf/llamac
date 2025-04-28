module Parser.Utils (parse, parseAndAnalyze, parseFile) where

import Common.AST (AST)
import Lexer.Lexer (AlexPosn)
import Parser.Parser (calc)
import Parser.ParserM (Error, ParserState, parseString)
import Semantics.Semantics (SemanticTag, analyzeAST)

-- The parsing function
parse :: String -> Either Error (AST AlexPosn)
parse = fst . parseString calc

-- The parsing and semantic analysis function
parseAndAnalyze :: String -> (Either Error (AST SemanticTag), ParserState)
parseAndAnalyze = parseString $ calc >>= analyzeAST

-- Parsing utils for files
parseFile :: FilePath -> IO ()
parseFile f = do
  s <- readFile f
  let res = parse s
  case res of
    Left err -> print err
    Right p  -> print p
