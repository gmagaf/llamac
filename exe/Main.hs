{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Control.Monad (when, unless)
import Control.Lens (view)
import System.Exit (exitFailure)
import System.Console.CmdArgs.Implicit hiding (args)

import Common.Source (Source (FileIn))
import Common.FileUtils (safeReadFile)
import Common.DebugPrint (Debug (debugPrint, debugWrite))
import Parser.ParserM (Parser)
import Parser.ParserState (ParserState, symbols)
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
            Lex   -> parseAndPrint scanM (src, code) outF
            Parse -> parseAndPrint parseM (src, code) outF
            Sem   -> parseAndPrint initAnalyzeM (src, code) outF
            Gen   -> parseAndPrint (genM f) (src, code) outF
      when (debugFlag args) $ do
        debugPrint state
        putStrLn "Symbol Table"
        debugPrint (view symbols state)
      unless success exitFailure

parseAndPrint :: Debug a => Parser a -> (Source, String) -> Maybe FilePath -> IO (Bool, ParserState)
parseAndPrint parserM (src, code) outF = do
  let (r, state) = parseString parserM src code
  case r of
    Left err  -> do
      print err
      return (False, state)
    Right res -> do
      case outF of
        Nothing -> debugPrint res
        Just f  -> debugWrite f res
      return (True, state)
