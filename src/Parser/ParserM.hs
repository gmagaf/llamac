{-# Language GeneralizedNewtypeDeriving #-}
module Parser.ParserM (ParserT(..), Parser,
                       parserT, pureParserT,
                       get, put,
                       getAlexPos, getTokenPosn, putAlexState,
                       getSymbols, putSymbols,
                       getSemState, putSemState,
                       throw, withExcept, catch,
                       Error, throwError, throwAtPosn, stackTrace,
                       throwInternalError,
                       throwParsingError, throwSemanticError,
                       run, runParser, eval, evalParser, parseString,
                       lexerWrap) where

import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Trans.Except as Except (ExceptT(ExceptT), throwE, catchE, runExceptT, withExceptT)
import qualified Control.Monad.Trans.State as State (StateT(StateT, runStateT), get, put, evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity (Identity (..))

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
  show (InternalError s)  = "Internal Compiler Error: " ++ s ++
    ". If you see this error please contact the maintainers"
  show (LexicalError s)   = "Lexical Error: " ++ s
  show (ParsingError s)   = "Parser Error: " ++ s
  show (SemanticError s)  = "Semantic Error: " ++ s

-- The monad transformation definition
newtype ParserT e s m a = ParserT { getParserT :: Except.ExceptT e (State.StateT s m) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    )

-- Constructors for our parser
parserT :: (s -> m (Either e a, s)) -> ParserT e s m a
parserT = ParserT . Except.ExceptT . State.StateT

pureParserT :: Monad m => (s -> (Either e a, s)) -> ParserT e s m a
pureParserT f = ParserT . Except.ExceptT . State.StateT $ (return . f)

-- The monad definition
type Parser a = ParserT Error ParserState Identity a

-- Monad utils
get :: Monad m => ParserT e s m s
get = ParserT $ lift State.get

put :: Monad m => s -> ParserT e s m ()
put s = ParserT $ lift (State.put s)

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
eval :: Monad m => s -> ParserT e s m a -> m (Either e a)
eval s p = State.evalStateT (Except.runExceptT (getParserT p)) s

run :: s -> ParserT e s m a -> m (Either e a, s)
run s p = State.runStateT (Except.runExceptT (getParserT p)) s

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
throw :: Monad m => e -> ParserT e s m a
throw = ParserT . Except.throwE

withExcept :: Monad m => (e -> e') -> ParserT e s m a -> ParserT e' s m a
withExcept f = ParserT . Except.withExceptT f . getParserT

catch :: Monad m => (e -> ParserT e' s m a) -> ParserT e s m a -> ParserT e' s m a
catch handle p = ParserT $ Except.catchE (getParserT p) (getParserT . handle)

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