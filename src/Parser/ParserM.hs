module Parser.ParserM (Parser,
                       getAlexPos, getTokenPosn, putAlexState,
                       getSymbols, putSymbols,
                       getSemState, putSemState,
                       Error, throwError, throwAtPosn, stackTrace,
                       throwInternalError,
                       throwParsingError, throwSemanticError,
                       runParser, evalParser, parseString,
                       lexerWrap) where

import Data.Functor.Identity (Identity (..))
import Control.Lens.Setter ((.=))
import Control.Lens.Getter (use)

import Lexer.Lexer (Alex(..), AlexState(..), AlexPosn,
      alexMonadScan, tokenPosnOfAlexState, printPosn)
import Common.Token (Token)
import Common.SymbolTable (SymbolTable)
import Parser.ParserState (ParserState, initParserState, alex_state, sem_state, symbols)
import Parser.ParserT (ParserT, evalParserT, runParserT, throw, withExcept, catch)
import Semantics.SemanticState (SemanticState)

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
type Parser = ParserT Error ParserState Identity

-- Monad utils
getAlexState :: Parser AlexState
getAlexState = use alex_state

getAlexPos :: Parser AlexPosn
getAlexPos = alex_pos <$> getAlexState

getTokenPosn :: Parser AlexPosn
getTokenPosn = tokenPosnOfAlexState <$> getAlexState

getSymbols :: Parser SymbolTable
getSymbols = use symbols

getSemState :: Parser SemanticState
getSemState = use sem_state

putAlexState :: AlexState -> Parser ()
putAlexState s = do
  alex_state .= s

putSymbols :: SymbolTable -> Parser ()
putSymbols s = do
  symbols .= s

putSemState :: SemanticState -> Parser ()
putSemState s = do
  sem_state .= s

-- Utils for running a Parser
evalParser :: ParserState -> Parser a -> Either Error a
evalParser s = runIdentity . flip evalParserT s

runParser :: ParserState -> Parser a -> (Either Error a, ParserState)
runParser s = runIdentity . flip runParserT s

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