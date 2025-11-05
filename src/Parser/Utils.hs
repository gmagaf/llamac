module Parser.Utils (parseAnalyzeM, initParseAnalyzeM, parseString,
                     parse, analyze, parseAndAnalyze,
                     readFileB, safeReadFile, parseFile,
                     debug, debugRepl) where

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import Control.Lens (view)
import Data.Text.Lazy (Text)

import Common.AST (AST)
import Common.PrintAST (pretty, debugPrint)
import Lexer.Lexer (AlexPosn)
import Parser.Parser (calc)
import Parser.ParserM (Error, Parser, runParser)
import Parser.ParserState (ParserState, sem_state, symbols, cgen_state, initParserState)
import Semantics.Utils (SemanticTag)
import Semantics.Semantics (analyzeAST)
import Control.Exception (IOException, handle)
import RunTime.LibHeaders (initSymbolTable)
import IR.CodeGen (genAST)
import IR.Utils (codegenProgram)

-- Various useful parsers
parseAnalyzeM :: Parser (AST SemanticTag)
parseAnalyzeM = calc >>= analyzeAST

initParseAnalyzeM :: Parser (AST SemanticTag)
initParseAnalyzeM = initSymbolTable >> calc >>= analyzeAST

initParseAnalyzeGenM :: String -> Parser Text
initParseAnalyzeGenM s = initSymbolTable >> calc >>= analyzeAST >>= genAST >> codegenProgram s

-- Util that initilizes a parser state and runs a parser monad
parseString :: Parser a -> String -> (Either Error a, ParserState)
parseString m s = runParser initState m where
  initState :: ParserState
  initState = initParserState s

  -- The parsing function
parse :: String -> Either Error (AST AlexPosn)
parse = fst . parseString calc

analyze :: String -> Either Error (AST SemanticTag)
analyze = fst . parseAndAnalyze

-- The parsing and semantic analysis function
parseAndAnalyze :: String -> (Either Error (AST SemanticTag), ParserState)
parseAndAnalyze = parseString initParseAnalyzeM

-- Util function for debugging end to end
debug :: String -> IO ()
debug s = do
  let (res, state) = parseString (initParseAnalyzeGenM "debug from ghci") s
  putStrLn "Semantic State"
  print (view sem_state state)
  putStrLn "Code Gen State"
  print (view cgen_state state)
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

-- Parsing utils for files
readFileB :: String -> IO String
readFileB fileName = do
  bts <- B.readFile fileName
  return (T.unpack . T.decodeUtf8 $ bts)

safeReadFile :: String -> IO (Either String String)
safeReadFile fileName = handle handleEx (Right <$> readFileB fileName)
  where handleEx :: IOException -> IO (Either String String)
        handleEx e = return (Left (show e))

parseFile :: FilePath -> IO ()
parseFile f = do
  s <- readFileB f
  let res = parse s
  case res of
    Left err -> print err
    Right p  -> print p
