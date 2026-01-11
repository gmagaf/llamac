{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Control.Monad (when, unless)
import System.Exit (exitFailure)
import System.Console.CmdArgs.Implicit hiding (args)

import Common.FileUtils (safeReadFile)
import Common.PrintAST (debugPrint)
import Common.SymbolType (Source (FileIn))
import Parser.ParserM (Parser)
import Parser.ParserState (ParserState)
import Parser.Utils (scanM, parseM, initAnalyzeM, genM, parseString)

data Args = Args
  { stageFlag        :: Stage
  , debugFlag        :: Bool
  , optimizationFlag :: Int
  , fileArg          :: FilePath
  , outputArg        :: Maybe FilePath
  }
  deriving (Show, Data, Typeable)

data Stage = Lex | Parse | Sem | Gen
  deriving (Show, Data, Typeable)

sample :: Args
sample = Args
  { stageFlag        = enum [ Sem   &= ignore
                            , Lex   &= help "Run the lexer"
                            , Parse &= help "Run the parser"
                            , Sem   &= help "Run the semantic analysis (default)"
                            , Gen   &= help "Run the code generation" ]
  , debugFlag        = def &= help "Print debug information"
  , optimizationFlag = def &= name "O" &= typ "NUM" &= help "The optimization level of the compiler"
  , fileArg          = def &= argPos 0 &= typFile
  , outputArg        = def &= help "Output file" &= typFile
  } &= program "llamac"
    &= summary "Llamac"
    &= help "A compiler for the language llama"
    &= versionArg [ignore]

main :: IO ()
main = do
  args <- cmdArgs sample
  let f = fileArg args
  let src = FileIn f
  let outF = outputArg args
  s <- safeReadFile f
  case s of
    Left err -> do
      putStrLn err
      exitFailure
    Right code -> do
      (success, state) <- case stageFlag args of
            Lex   -> parseAndPrint scanM "Tokens:\n" (src, code) outF
            Parse -> parseAndPrint parseM "AST:\n" (src, code) outF
            Sem   -> parseAndPrint initAnalyzeM "Annotated AST:\n" (src, code) outF
            Gen   -> parseAndPrint (genM f) "" (src, code) outF
      when (debugFlag args) $ print state
      unless success exitFailure

parseAndPrint :: Show a => Parser a -> String -> (Source, String) -> Maybe FilePath -> IO (Bool, ParserState)
parseAndPrint parserM msg (src, code) outF = do
  let (r, state) = parseString parserM src code
  case r of
    Left err  -> do
      print err
      return (False, state)
    Right res -> do
      printOut outF msg res
      return (True, state)

printOut :: Show a => Maybe FilePath -> String -> a -> IO ()
printOut outF msg res = do
  case outF of
    Nothing -> do
      putStr msg
      debugPrint res
    Just f -> do
      writeFile f (msg ++ show res)