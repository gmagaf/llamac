module Parser.ParserM (Parser, runParser, ParserState, initParserState,
                       getAlexPos,
                       Error, throwError, throwParsingError, throwSemanticsError,
                       lexerWrap) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, runExceptT)
import Control.Monad.Trans.State (State, evalState, state)
import Lexer.Lexer (Alex(..), AlexState(..), AlexPosn,
      alexStartPos, alexMonadScan, alexInitUserState )
import Common.Token (Token)

-- This module defines the Parser monad

-- The state of the parser
data ParserState = ParserState
  { alex_state   :: AlexState  -- the lexer's state
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
       { alex_state = initAlexState }

-- The compiler's errors
data Error = Error String
           | LexicalError String
           | ParsingError String
           | SemanticsError String
    deriving Eq

instance Show Error where
  show (Error s)          = s
  show (LexicalError s)   = "Lexical Error: " ++ s
  show (ParsingError s)   = "Parser Error: " ++ s
  show (SemanticsError s) = "Semantics Error: " ++ s

-- The monad definition
type Parser a = ExceptT Error (State ParserState) a

-- Monad utils
get :: Parser ParserState
get = ExceptT $ state $ \s -> (Right s, s)

getAlexState :: Parser AlexState
getAlexState = alex_state <$> get

getAlexPos :: Parser AlexPosn
getAlexPos = alex_pos <$> getAlexState

runParser :: ParserState -> Parser a -> Either Error a
runParser s p = evalState (runExceptT p) s

throwError :: String -> Parser a
throwError = throwE . Error

throwParsingError :: String -> Parser a
throwParsingError = throwE . ParsingError

throwSemanticsError :: String -> Parser a
throwSemanticsError = throwE . SemanticsError

-- Utils to facilitate the communication with the lexer
changeMonad :: Alex a -> Parser a
changeMonad (Alex f) = ExceptT (state g) where
    g pState = let aState = alex_state pState in
        case f aState of
            Right (aState', b) -> (Right b, ParserState {alex_state = aState'})
            Left err           -> (Left $ LexicalError err, pState)

parserMonadScan :: Parser Token
parserMonadScan = changeMonad alexMonadScan

lexerWrap :: (Token -> Parser a) -> Parser a
lexerWrap cont = do
  a <- parserMonadScan
  cont a