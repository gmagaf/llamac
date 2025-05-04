module Parser.ParserM (Parser,
                       getAlexPos, getTokenPosn,
                       getSymbols, putSymbols,
                       getSemState, putSemState,
                       Error, throwError, throwParsingError, throwSemanticError,
                       runParser, parseString, evalParser,
                       lexerWrap) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, runExceptT)
import Control.Monad.Trans.State (State, evalState, runState, state)

import Lexer.Lexer (Alex(..), AlexState(..), AlexPosn,
      alexMonadScan, tokenPosnOfAlexState)
import Common.Token (Token)
import Common.SymbolTable (SymbolTable)
import Parser.ParserState (ParserState(..), SemanticState, initParserState)

-- This module defines the Parser monad

-- The compiler's errors
data Error = Error String
           | LexicalError String
           | ParsingError String
           | SemanticError String
    deriving Eq

instance Show Error where
  show (Error s)          = s
  show (LexicalError s)   = "Lexical Error: " ++ s
  show (ParsingError s)   = "Parser Error: " ++ s
  show (SemanticError s)  = "Semantic Error: " ++ s

-- The monad definition
type Parser a = ExceptT Error (State ParserState) a

-- Monad utils
get :: Parser ParserState
get = ExceptT $ state $ \s -> (Right s, s)

getAlexState :: Parser AlexState
getAlexState = alex_state <$> get

getAlexPos :: Parser AlexPosn
getAlexPos = alex_pos <$> getAlexState

getTokenPosn :: Parser AlexPosn
getTokenPosn = tokenPosnOfAlexState <$> getAlexState

getSymbols :: Parser SymbolTable
getSymbols = symbols <$> get

getSemState :: Parser SemanticState
getSemState = sem_state <$> get

put :: ParserState -> Parser ()
put s = ExceptT $ state $ const (Right (), s)

putAlexState :: AlexState -> Parser ()
putAlexState s = do
  ps <- get
  put ps{alex_state = s}

putSymbols :: SymbolTable -> Parser ()
putSymbols s = do
  ps <- get
  put ps{symbols = s}

putSemState :: SemanticState -> Parser ()
putSemState s = do
  ps <- get
  put ps{sem_state = s}

-- Utils for running a Parser
evalParser :: ParserState -> Parser a -> Either Error a
evalParser s p = evalState (runExceptT p) s

runParser :: ParserState -> Parser a -> (Either Error a, ParserState)
runParser s p = runState (runExceptT p) s

-- Util that initilizes a parser state and runs a parser monad
parseString :: Parser a -> String -> (Either Error a, ParserState)
parseString m s = runParser initState m where
  initState :: ParserState
  initState = initParserState s

-- Utils for error handling
throwError :: String -> Parser a
throwError = throwE . Error

throwLexicalError :: String -> Parser a
throwLexicalError = throwE . LexicalError

throwParsingError :: String -> Parser a
throwParsingError = throwE . ParsingError

throwSemanticError :: String -> Parser a
throwSemanticError = throwE . SemanticError

-- Utils to facilitate the communication with the lexer
changeMonad :: Alex a -> Parser a
changeMonad (Alex f) = do
  aState <- getAlexState
  case f aState of
    Right (aState', a) -> do
      putAlexState aState'
      return a
    Left lexErr        -> throwLexicalError lexErr

parserMonadScan :: Parser Token
parserMonadScan = changeMonad alexMonadScan

lexerWrap :: (Token -> Parser a) -> Parser a
lexerWrap cont = do
  a <- parserMonadScan
  cont a