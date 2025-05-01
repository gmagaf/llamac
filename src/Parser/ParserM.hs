module Parser.ParserM (Parser, runParser, parseString,
                       ParserState(..), initParserState,
                       getAlexPos, getTokenPosn, getSymbols, putSymbols,
                       getAndIncrVarTypeC,
                       Error, throwError, throwParsingError, throwSemanticError,
                       lexerWrap, evalParser) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, runExceptT)
import Control.Monad.Trans.State (State, evalState, runState, state)
import Lexer.Lexer (Alex(..), AlexState(..), AlexPosn,
      alexMonadScan, alexInitUserState, tokenPosnOfAlexState, alexStartPos)
import Common.Token (Token)
import Common.SymbolTable (SymbolTable, emptySymbolTable)

-- This module defines the Parser monad

-- The state of the parser
data ParserState = ParserState
  { alex_state   :: AlexState   -- lexer's state
  , symbols      :: SymbolTable -- compiler's symbol table
  , varTypeC     :: Int         -- a counter to the var types used
  }

initParserState :: String -> ParserState
initParserState input =
    let initAlexState = AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_bytes = [],
                        alex_ust = alexInitUserState,
                        alex_scd = 0}
    in ParserState
       { alex_state = initAlexState
       , symbols    = emptySymbolTable
       , varTypeC   = 0
       }

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

getAndIncrVarTypeC :: Parser Int
getAndIncrVarTypeC = do
  ps <- get
  let c = varTypeC ps
  put ps{varTypeC = c + 1}
  return c

evalParser :: ParserState -> Parser a -> Either Error a
evalParser s p = evalState (runExceptT p) s

runParser :: ParserState -> Parser a -> (Either Error a, ParserState)
runParser s p = runState (runExceptT p) s

-- Util that initilizes a parser state and runs a parser monad
parseString :: Parser a -> String -> (Either Error a, ParserState)
parseString m s = runParser initState m where
  initState :: ParserState
  initState = initParserState s

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