module Parser.Utils (scanM, parseM, analyzeM, initAnalyzeM, genM,
                     parseString, parseFile, parseLine,
                     parse, analyze,
                     debug, debugRepl) where

import Control.Lens (view)
import Data.Text.Lazy (Text)

import Common.Token (Token)
import Common.AST (AST)
import Common.PrintAST (pretty, debugPrint)
import Common.FileUtils (readFileB)
import Lexer.Lexer (AlexPosn, alexTokens)
import Parser.Parser (calc)
import Parser.ParserM (Error, Parser, runParser, liftAlex)
import Parser.ParserState (ParserState, sem_state, symbols, initParserState)
import Semantics.Utils (SemanticTag)
import Semantics.Semantics (analyzeAST)
import RunTime.LibHeaders (initSymbolTable)
import IR.CodeGen (genAST)
import IR.Utils (codegenProgram)

-- Various useful parsers
scanM :: Parser [Token]
scanM = liftAlex alexTokens

parseM :: Parser (AST AlexPosn)
parseM = calc

analyzeM :: Parser (AST SemanticTag)
analyzeM = calc >>= analyzeAST

initAnalyzeM :: Parser (AST SemanticTag)
initAnalyzeM = initSymbolTable >> calc >>= analyzeAST

genM :: String -> Parser Text
genM s = initSymbolTable >> calc >>= analyzeAST >>= genAST >> codegenProgram s

-- Util that initilizes a parser state and runs a parser monad
parseString :: Parser a -> String -> (Either Error a, ParserState)
parseString m s = runParser initState m where
  initState :: ParserState
  initState = initParserState s

-- Parse a file
parseFile :: Parser a -> FilePath -> IO (Either Error a)
parseFile m f = do
  inp <- readFileB f
  let (res, _) = parseString m inp
  return res

-- Parse a line
parseLine :: Parser a -> IO (Either Error a)
parseLine m = do
  line <- getLine
  let (res, _) = parseString m line
  return res

-- Some util functions for parsing strings
parse :: String -> Either Error (AST AlexPosn)
parse = fst . parseString parseM

analyze :: String -> Either Error (AST SemanticTag)
analyze = fst . parseString initAnalyzeM

-- Util function for debugging end to end
debug :: String -> IO ()
debug s = do
  let (res, state) = parseString initAnalyzeM s
  -- let (res, state) = parseString (initParseAnalyzeGenM "debug from ghci") s
  putStrLn "Semantic State"
  print (view sem_state state)
  -- putStrLn "Code Gen State"
  -- print (view cgen_state state)
  putStrLn "Symbol Table"
  putStrLn $ pretty (view symbols state)
  case res of
    Left err  -> print err
    Right ast -> debugPrint ast

debugRepl :: IO ()
debugRepl = do
  s <- getLine
  debug s
  debugRepl
