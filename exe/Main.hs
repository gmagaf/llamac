{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Control.Monad (when)
import System.Console.CmdArgs.Implicit hiding (args)

import Common.FileUtils (safeReadFile)
import Common.PrintAST (debugPrint)
import Parser.ParserM (Parser)
import Parser.ParserState (ParserState)
import Parser.Utils (scanM, parseM, initAnalyzeM, genM, parseString)

data Args = Args
  { stage :: Stage
  , debug :: Bool
  , optim :: Int
  , file  :: FilePath
  , output :: Maybe FilePath
  }
  deriving (Show, Data, Typeable)

data Stage = Lex | Parse | Sem | Gen
  deriving (Show, Data, Typeable)

sample :: Args
sample = Args
  { stage  = enum [Sem &= ignore, Lex &= help "Run the lexer",Parse &= help "Run the parser", Sem &= help "Run the semantic analysis (default)", Gen &= help "Run the code generation"]
  , debug  = def &= help "Print debug information"
  , optim  = def &= name "O" &= typ "NUM" &= help "The optimization level of the compiler"
  , file   = def  &= argPos 0 &= typFile
  , output = def &= help "Output file" &= typFile
  } &= program "llamac"
    &= summary "Llamac"
    &= help "A compiler for the language llama"
    &= versionArg [ignore]

main :: IO ()
main = do
  args <- cmdArgs sample
  let f = file args
  let outF = output args
  s <- safeReadFile f
  case s of
    Left err -> putStrLn err
    Right code -> do
      state <- case stage args of
            Lex   -> printResult scanM "Tokens:\n" code outF
            Parse -> printResult parseM "AST:\n" code outF
            Sem   -> printResult initAnalyzeM "Annotated AST:\n" code outF
            Gen   -> printResult (genM f) "" code outF
      when (debug args) $ print state

printResult :: Show a => Parser a -> String -> String -> Maybe FilePath -> IO ParserState
printResult parserM msg code outF = do
  let (r, state) = parseString parserM code
  case r of
    Left err  -> print err
    Right res -> do
      case outF of
        Nothing -> do
          putStr msg
          debugPrint res
        Just f -> writeFile f (msg ++ show res)
  return state
