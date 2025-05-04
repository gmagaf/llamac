module Semantics.Utils (module Semantics.Utils) where

import qualified Data.Set as Set

import Common.AST (TypeF(..))
import Common.SymbolTable(TableEntry(..), SymbolType(..),
    query, insert, update, openScope, closeScope)
import Lexer.Lexer (AlexPosn, printPosn)
import Parser.ParserM (Parser,
    getSymbols, getSemState, putSymbols, putSemState,
    throwSemanticError)
import Parser.ParserState (SemanticState(..)) 

-- This module contains semantic analysis tools

-- Functions for dealing with the Semantic state of the parser
getAndIncrVarTypeC :: Parser Int
getAndIncrVarTypeC = do
  sState <- getSemState
  let c = varTypeC sState
  putSemState sState{varTypeC = c + 1}
  return c

getSemPosn :: Parser AlexPosn
getSemPosn = posnOfSem <$> getSemState

putSemPosn :: AlexPosn -> Parser ()
putSemPosn p = do
    s <- getSemState
    putSemState s{posnOfSem = p}

-- Error handling functions
throwSemAtPosn :: String -> AlexPosn -> Parser a
throwSemAtPosn s p = throwSemanticError $ s ++ " at " ++ printPosn p

throwSem :: String -> Parser a
throwSem s = do
    p <- getSemPosn
    throwSemanticError $ s ++ " at " ++ printPosn p

-- Parser symbol table utiles
insertSymbols :: String -> TableEntry -> Parser ()
insertSymbols k entry = do
    symbols <- getSymbols
    putSymbols $ insert k entry symbols

queryAndRun :: String -> (TableEntry -> Parser a) -> Parser a
queryAndRun k run = do
    symbols <- getSymbols
    case query k symbols of
        Just entry -> run entry
        _ -> throwSem ("Symbol " ++ k ++ " is not in scope")

updateSymbols :: String -> TableEntry -> Parser ()
updateSymbols k entry = do
    symbols <- getSymbols
    putSymbols $ update k entry symbols

checkTypeInScope :: AlexPosn -> String -> Parser ()
checkTypeInScope p k = do
    symbols <- getSymbols
    case query k symbols of
        Just (TypeEntry _) -> return ()
        _ -> throwSemAtPosn ("Type symbol " ++ k ++ " is not in scope") p

openScopeInTable :: Parser ()
openScopeInTable = do
    symbols <- getSymbols
    putSymbols $ openScope symbols

closeScopeInTable :: Parser ()
closeScopeInTable = do
    symbols <- getSymbols
    putSymbols $ closeScope symbols

-- Other util functions
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

paramsToFunType :: [SymbolType] -> SymbolType -> SymbolType
paramsToFunType [] out = out
paramsToFunType (t:ts) out = SymType (FunType t (paramsToFunType ts out))
