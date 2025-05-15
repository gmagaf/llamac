module Parser.ParserState (ParserState(..), SemanticState(..), Unifier,
                          initParserState) where

import Common.PrintAST (Pretty(pretty))
import Common.SymbolType (SymbolType (..))
import Common.SymbolTable (SymbolTable, emptySymbolTable)
import Lexer.Lexer (AlexState(..), AlexPosn, alexStartPos, alexInitUserState)
import Data.List (intercalate)


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

type Unifier = SymbolType -> Maybe SymbolType

data SemanticState = SemanticState
  { varTypeC     :: Int      -- a counter to the var types used
  , posnOfSem    :: AlexPosn -- a posn to the current place of analysis, used for error messages
  , unifier      :: Unifier  -- this should always be the most
                             -- general unifier and it should keep track
                             -- of variables not in scope. This should work
                             -- if all the fresh variables have a new name.
                             -- This is ensured by the varTypeC. However, the
                             -- result for each type in scope should be
                             -- a type in scope.
  }

instance Show SemanticState where
  show s = "SemanticState {" ++
    "varTypeC = " ++ show (varTypeC s)
    ++ ", " ++ "posnOfSem = " ++ show (posnOfSem s)
    ++ ", " ++ "unifier = " ++ showUnifierVals (unifier s) (varTypeC s)
    ++ "}"

showUnifierVals :: Unifier -> Int -> String
showUnifierVals _ 0 = "_"
showUnifierVals f n = intercalate ", " $ map (showUnifierVal f . TVar) [0..(n - 1)]

showUnifierVal :: Unifier -> SymbolType -> String
showUnifierVal f st = "U(" ++ pretty st ++ ")=" ++ val where
  val = maybe "Nothing" pretty (f st)

initSemanticState :: SemanticState
initSemanticState = SemanticState
                {
                  varTypeC = 0
                , posnOfSem = alexStartPos
                , unifier = initUnifier
                }

initUnifier :: SymbolType -> Maybe SymbolType
initUnifier (SymType tf) = SymType <$> mapM initUnifier tf
initUnifier (TVar _) = Nothing

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