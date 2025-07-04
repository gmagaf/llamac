module Parser.Utils (parse, analyze, parseAndAnalyze, readFileB, parseFile, debug, debugRepl) where

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Common.AST (AST)
import Common.PrintAST (pretty, debugPrint)
import Lexer.Lexer (AlexPosn)
import Parser.Parser (calc)
import Parser.ParserM (Error, parseString)
import Parser.ParserState (ParserState(..))
import Semantics.Utils (SemanticTag)
import Semantics.Semantics (analyzeAST)

-- The parsing function
parse :: String -> Either Error (AST AlexPosn)
parse = fst . parseString calc

analyze :: String -> Either Error (AST SemanticTag)
analyze = fst . parseAndAnalyze

-- The parsing and semantic analysis function
parseAndAnalyze :: String -> (Either Error (AST SemanticTag), ParserState)
parseAndAnalyze = parseString (calc >>= analyzeAST)

-- Util function for debugging end to end
debug :: String -> IO ()
debug s = do
  let (res, state) = parseAndAnalyze s
  putStrLn "Semantic State"
  print (sem_state state)
  putStrLn "Symbol Table"
  putStrLn $ pretty (symbols state)
  case res of
    Left err  -> print err
    Right ast -> debugPrint ast

debugRepl :: IO ()
debugRepl = do
  s <- getLine
  debug s
  debugRepl

-- Parsing utils for files
readFileB :: String -> IO String
readFileB fileName = do
  bts <- B.readFile fileName
  return (T.unpack . T.decodeUtf8 $ bts)

parseFile :: FilePath -> IO ()
parseFile f = do
  s <- readFileB f
  let res = parse s
  case res of
    Left err -> print err
    Right p  -> print p
