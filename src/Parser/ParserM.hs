module Parser.ParserM (ExceptState, Parser,
                       get, put,
                       getAlexPos, getTokenPosn,
                       getSymbols, putSymbols,
                       getSemState, putSemState,
                       Error, throwError, throwAtPosn, stackTrace,
                       throwInternalError,
                       throwParsingError, throwSemanticError,
                       run, parseString, eval,
                       lexerWrap) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, catchE, runExceptT, withExceptT)
import Control.Monad.Trans.State (State, evalState, runState, state)

import Lexer.Lexer (Alex(..), AlexState(..), AlexPosn,
      alexMonadScan, tokenPosnOfAlexState, printPosn)
import Common.Token (Token)
import Common.SymbolTable (SymbolTable)
import Parser.ParserState (ParserState(..), SemanticState, initParserState)

-- This module defines the Parser monad

-- The compiler's errors
data Error = Error {msg :: String}
           | InternalError {msg :: String}
           | LexicalError {msg :: String}
           | ParsingError {msg :: String}
           | SemanticError {msg :: String}
    deriving Eq

instance Show Error where
  show (Error s)          = s
  show (InternalError s)  = "Internal Compiler Error" ++ s ++
    ". If you see this error please contact the maintainers"
  show (LexicalError s)   = "Lexical Error: " ++ s
  show (ParsingError s)   = "Parser Error: " ++ s
  show (SemanticError s)  = "Semantic Error: " ++ s

-- The monad definition
type ExceptState e s a = ExceptT e (State s) a

type Parser a = ExceptState Error ParserState a

-- Monad utils
get :: ExceptState e s s
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

put :: s -> ExceptState e s ()
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
eval :: s -> ExceptState e s a -> Either e a
eval s p = evalState (runExceptT p) s

run :: s -> ExceptState e s a -> (Either e a, s)
run s p = runState (runExceptT p) s

-- Util that initilizes a parser state and runs a parser monad
parseString :: Parser a -> String -> (Either Error a, ParserState)
parseString m s = run initState m where
  initState :: ParserState
  initState = initParserState s

-- Utils for error handling
throwError :: String -> Parser a
throwError = throwE . Error

throwAtPosn :: AlexPosn -> Parser a -> Parser a
throwAtPosn p = withExceptT (\e -> e{msg = msg e ++ " at " ++ printPosn p})

throwInternalError :: String -> Parser a
throwInternalError = throwE . InternalError

throwLexicalError :: String -> Parser a
throwLexicalError = throwE . LexicalError

throwParsingError :: String -> Parser a
throwParsingError = throwE . ParsingError

throwSemanticError :: String -> Parser a
throwSemanticError = throwE . SemanticError

stackTrace :: String -> Parser a -> Parser a
stackTrace s p = catchE p (throwE . \e -> e{msg = msg e ++ "\n\t\t" ++ s})

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