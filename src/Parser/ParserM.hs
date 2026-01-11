module Parser.ParserM (Parser,
                       getSource, putSource,
                       getAlexPos, getTokenPosn, putAlexState,
                       getSemState, putSemState,
                       getCGenState, putCGenState,
                       Error, throwError, throwAtPosn, stackTrace,
                       throwInternalError,
                       throwParsingError, throwSemanticError, throwCGenError,
                       runParser, evalParser,
                       liftAlex, lexerWrap) where

import Data.Functor.Identity (Identity (..))
import Control.Lens.Setter ((.=))
import Control.Lens.Getter (use)

import Lexer.Lexer (Alex(..), AlexState(..), AlexPosn,
      alexMonadScan, tokenPosnOfAlexState, printPosn)
import Common.Token (Token)
import Parser.ParserState (ParserState, source, alex_state, sem_state, cgen_state)
import Parser.ParserT (ParserT, evalParserT, runParserT, throw, withExcept, catch)
import Semantics.SemanticState (SemanticState)
import IR.CodeGenState (CodeGenState)
import Common.SymbolType (Source)

-- This module defines the Parser monad

-- The compiler's errors
data Error = Error {msg :: String}
           | InternalError {msg :: String}
           | LexicalError {msg :: String}
           | ParsingError {msg :: String}
           | SemanticError {msg :: String}
           | CodeGenError {msg :: String}
    deriving Eq

instance Show Error where
  show (Error s)          = s
  show (InternalError s)  = "Internal Compiler Error: " ++ s ++
    ". If you see this error please contact the maintainers"
  show (LexicalError s)   = "Lexical Error: " ++ s
  show (ParsingError s)   = "Parser Error: " ++ s
  show (SemanticError s)  = "Semantic Error: " ++ s
  show (CodeGenError s)   = "Code Gen Error: " ++ s


-- The monad definition
type Parser = ParserT Error ParserState Identity

-- Monad utils
getSource :: Parser Source
getSource = use source

getAlexState :: Parser AlexState
getAlexState = use alex_state

getAlexPos :: Parser AlexPosn
getAlexPos = alex_pos <$> getAlexState

getTokenPosn :: Parser AlexPosn
getTokenPosn = tokenPosnOfAlexState <$> getAlexState

getSemState :: Parser SemanticState
getSemState = use sem_state

getCGenState :: Parser CodeGenState
getCGenState = use cgen_state

putSource :: Source -> Parser ()
putSource s = do
  source .= s

putAlexState :: AlexState -> Parser ()
putAlexState s = do
  alex_state .= s

putSemState :: SemanticState -> Parser ()
putSemState s = do
  sem_state .= s

putCGenState :: CodeGenState -> Parser ()
putCGenState s = do
  cgen_state .= s

-- Utils for running a Parser
evalParser :: ParserState -> Parser a -> Either Error a
evalParser s = runIdentity . flip evalParserT s

runParser :: ParserState -> Parser a -> (Either Error a, ParserState)
runParser s = runIdentity . flip runParserT s

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

throwCGenError :: String -> Parser a
throwCGenError = throw . CodeGenError

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