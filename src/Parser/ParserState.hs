{-# LANGUAGE TemplateHaskell #-}
module Parser.ParserState (ParserState,
                           alex_state, sem_state,
                           symbols, cgen_state,
                           initAlexState, initParserState) where

import Control.Lens (makeLenses, over)
import Control.Lens.Getter (view)

import Common.SymbolType (TypeScheme(MonoType), constTypeToSymbolType)
import Common.SymbolTable (SymbolTable, TableEntry (FunEntry), NameSpace,
                           emptySymbolTable, openScope, mkBasicEntry, names, partialInsert)
import Lexer.Lexer (AlexState(..), alexStartPos, alexInitUserState)
import Semantics.SemanticState (SemanticState, initSemanticState)
import RunTime.LibHeaders (RunTimeLibSib, libSigs)
import IR.CodeGenState (CodeGenState, initCodeGenState)


-- The state of the parser
data ParserState = ParserState
  { _alex_state   :: AlexState     -- lexer's state
  , _sem_state    :: SemanticState -- semantic analysis state
  , _symbols      :: SymbolTable   -- compiler's symbol table
  , _cgen_state   :: CodeGenState  -- code generator's state
  }
makeLenses ''ParserState

instance Show ParserState where
  show s = "ParserState {" ++
    "alex_state = _"
    ++ ", " ++ "sem_state = " ++ show (view sem_state s)
    ++ ", " ++ "cgen_state = " ++ show (view cgen_state s)
    ++ ", " ++ "symbols = " ++ show (view symbols s)
    ++ "}"

initAlexState :: String -> AlexState
initAlexState input = AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_bytes = [],
                        alex_ust = alexInitUserState,
                        alex_scd = 0}

initParserState :: String -> ParserState
initParserState input =
    ParserState
       { _alex_state = initAlexState input
       , _sem_state  = initSemanticState
       , _symbols    = initSymbolTable
       , _cgen_state = initCodeGenState
       }

initSymbolTable :: SymbolTable
initSymbolTable =
  let addRunTimeLibSib :: RunTimeLibSib -> NameSpace -> NameSpace
      addRunTimeLibSib (i, t, ps) = partialInsert mkBasicEntry i (FunEntry (MonoType . constTypeToSymbolType $ t) ps)
  in over names ((\ns -> foldr addRunTimeLibSib ns libSigs) . openScope) emptySymbolTable