module Parser.ParserM (Parser,
                       getAlexPos, getTokenPosn, putAlexState,
                       getSymbols, putSymbols,
                       getSemState, putSemState,
                       Error, throwError, throwAtPosn, stackTrace,
                       throwInternalError,
                       throwParsingError, throwSemanticError,
                       run, runParser, eval, evalParser, parseString,
                       lexerWrap) where

import Data.Functor.Identity (Identity (..))

import Lexer.Lexer (Alex(..), AlexState(..), AlexPosn,
      alexMonadScan, tokenPosnOfAlexState, printPosn)
import Common.Token (Token)
import Common.SymbolTable (SymbolTable)
import Parser.ParserState (ParserState(..), SemanticState, initParserState)
import Parser.ParserT (ParserT, get, put, eval, run, throw, withExcept, catch)

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
  show (InternalError s)  = "Internal Compiler Error: " ++ s ++
    ". If you see this error please contact the maintainers"
  show (LexicalError s)   = "Lexical Error: " ++ s
  show (ParsingError s)   = "Parser Error: " ++ s
  show (SemanticError s)  = "Semantic Error: " ++ s


-- The monad definition
type Parser a = ParserT Error ParserState Identity a

-- Monad utils
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
evalParser s = runIdentity . eval s

runParser :: ParserState -> Parser a -> (Either Error a, ParserState)
runParser s = runIdentity . run s

-- Util that initilizes a parser state and runs a parser monad
parseString :: Parser a -> String -> (Either Error a, ParserState)
parseString m s = runParser initState m where
  initState :: ParserState
  initState = initParserState s

-- Utils for error handling
throwError :: String -> Parser a
throwError = throw . Error

throwAtPosn :: AlexPosn -> Parser a -> Parser a
throwAtPosn p = withExcept (\e -> e{msg = msg e ++ " at " ++ printPosn p})

throwInternalError :: String -> Parser a
throwInternalError = throw . InternalError

throwLexicalError :: String -> Parser a
throwLexicalError = throw . LexicalError

throwParsingError :: String -> Parser a
throwParsingError = throw . ParsingError

throwSemanticError :: String -> Parser a
throwSemanticError = throw . SemanticError

stackTrace :: String -> Parser a -> Parser a
stackTrace s = catch (throw . \e -> e{msg = msg e ++ "\n\t\t" ++ s})

-- Utils to facilitate the communication with the lexer
liftAlex :: Alex a -> Parser a
liftAlex (Alex f) = do
  aState <- getAlexState
  case f aState of
    Right (aState', a) -> do
      putAlexState aState'
      return a
    Left lexErr        -> throwLexicalError lexErr

lexerWrap :: (Token -> Parser a) -> Parser a
lexerWrap cont = do
  a <- liftAlex alexMonadScan
  cont a