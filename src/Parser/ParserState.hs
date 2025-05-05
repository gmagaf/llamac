module Parser.ParserState (ParserState(..), SemanticState(..), initParserState) where

import Lexer.Lexer (AlexState(..), AlexPosn, alexStartPos, alexInitUserState)
import Common.SymbolTable (SymbolTable, emptySymbolTable)


-- The state of the parser
data ParserState = ParserState
  { alex_state   :: AlexState     -- lexer's state
  , sem_state    :: SemanticState -- semantic analysis state
  , symbols      :: SymbolTable   -- compiler's symbol table
  }

data SemanticState = SemanticState
  { varTypeC     :: Int      -- a counter to the var types used
  , posnOfSem    :: AlexPosn -- a posn to the current place of analysis, used for error messages
  } deriving Show

initSemanticState :: SemanticState
initSemanticState = SemanticState
                {
                  varTypeC = 0
                , posnOfSem = alexStartPos
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
       , sem_state  = initSemanticState
       , symbols    = emptySymbolTable
       }