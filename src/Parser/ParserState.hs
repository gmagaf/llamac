module Parser.ParserState (ParserState(..), SemanticState(..), Unifier,
                          initAlexState, initParserState) where

import Common.SymbolType (TypeScheme(MonoType), constTypeToSymbolType)
import Common.SymbolTable (SymbolTable (..), TableEntry (FunEntry), NameSpace,
                           emptySymbolTable, insert,openScope)
import Lexer.Lexer (AlexState(..), alexStartPos, alexInitUserState)
import Semantics.SemanticState (SemanticState(..), Unifier, initSemanticState)
import RunTime.LibHeaders (RunTimeLibSib, libSigs)


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
       { alex_state = initAlexState input
       , sem_state  = initSemanticState
       , symbols    = initSymbolTable
       }

initSymbolTable :: SymbolTable
initSymbolTable =
  let addRunTimeLibSib :: RunTimeLibSib -> NameSpace -> NameSpace
      addRunTimeLibSib (i, t, ps) = insert i (FunEntry (MonoType . constTypeToSymbolType $ t) ps)
      empty = emptySymbolTable
      open = openScope (names empty)
      finalNamesSpace = foldr addRunTimeLibSib open libSigs
  in empty {names = finalNamesSpace}