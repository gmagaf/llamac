{-# LANGUAGE TemplateHaskell #-}
module Parser.ParserState (ParserState,
                           source, alex_state, sem_state,
                           symbols, cgen_state,
                           initAlexState, initParserState) where

import Control.Lens (makeLenses)
import Control.Lens.Getter (view)

import Common.DebugPrint
import Common.SymbolType (Source, printSource)
import Common.SymbolTable (SymbolTable, emptySymbolTable)
import Lexer.Lexer (AlexState(..), alexStartPos, alexInitUserState)
import Semantics.SemanticState (SemanticState, initSemanticState)
import IR.CodeGenState (CodeGenState, initCodeGenState)


-- The state of the parser
data ParserState = ParserState
  { _source       :: Source        -- code source
  , _alex_state   :: AlexState     -- lexer's state
  , _sem_state    :: SemanticState -- semantic analysis state
  , _symbols      :: SymbolTable   -- compiler's symbol table
  , _cgen_state   :: CodeGenState  -- code generator's state
  }
makeLenses ''ParserState

instance Show ParserState where
  show s = "ParserState {"
            ++ "source = " ++ printSource (view source s)
    ++ ", " ++ "alex_state = " ++ show (view alex_state s)
    ++ ", " ++ "sem_state = " ++ show (view sem_state s)
    ++ ", " ++ "cgen_state = " ++ show (view cgen_state s)
    -- ++ ", " ++ "symbols = " ++ show (view symbols s)
    ++ "}"

instance DebugPrint ParserState where
  debugPrint = debugIO False True

initAlexState :: String -> AlexState
initAlexState input = AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_bytes = [],
                        alex_ust = alexInitUserState,
                        alex_scd = 0}

initParserState :: Source -> String -> ParserState
initParserState src input =
    ParserState
       { _source     = src
       , _alex_state = initAlexState input
       , _sem_state  = initSemanticState
       , _symbols    = emptySymbolTable
       , _cgen_state = initCodeGenState
       }
