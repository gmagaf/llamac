module Parser.ParserState (ParserState(..), SemanticState(..), Unifier,
                          initParserState) where

import Common.SymbolTable (SymbolTable, emptySymbolTable)
import Lexer.Lexer (AlexState(..), alexStartPos, alexInitUserState)
import Semantics.SemanticState (SemanticState(..), Unifier, initSemanticState)


-- The state of the parser
data ParserState = ParserState
  { alex_state   :: AlexState     -- lexer's state
  , sem_state    :: SemanticState -- semantic analysis state
  , symbols      :: SymbolTable   -- compiler's symbol table
  }

instance Show ParserState where
  show s = "ParserState {" ++
    "alex_state = _"
    ++ ", " ++ "sem_state = " ++ show (sem_state s)
    ++ ", " ++ "symbols = " ++ show (symbols s)
    ++ "}"

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
       , sem_state  = initSemanticState
       , symbols    = emptySymbolTable
       }