module Semantics.Utils (module Semantics.Utils) where

import qualified Data.Set as Set

import Common.AST (TypeF(..))
import Common.SymbolTable(TableEntry(..), query, insert, update, openScope, closeScope, varTypeKey,)
import Lexer.Lexer (AlexPosn, printPosn)
import Parser.ParserM (Parser,
    getSymbols, getSemState, putSymbols, putSemState,
    throwSemanticError)
import Parser.ParserState (SemanticState(..))
import Common.SymbolType
import Common.Token (Identifier, ConstrIdentifier)
import Control.Monad ((>=>))

-- This module contains semantic analysis tools

-- Functions for dealing with the Semantic state of the parser
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

freshTVar :: Parser SymbolType
freshTVar = do
    sState <- getSemState
    let c = varTypeC sState
    let v = TVar c
    putSemState sState{varTypeC = c + 1}
    symbols <- getSymbols
    putSymbols $ insert (varTypeKey c) (TVarEntry v) symbols
    return v

updateSymbols :: String -> TableEntry -> Parser ()
updateSymbols k entry = do
    symbols <- getSymbols
    putSymbols $ update k entry symbols

updateTVar :: Int -> TableEntry -> Parser ()
updateTVar v = updateSymbols (varTypeKey v)

querySymbols :: String -> Parser (Maybe TableEntry)
querySymbols k = query k <$> getSymbols

findSymbols :: String -> Parser TableEntry
findSymbols k = do
    symbols <- getSymbols
    case query k symbols of
        Just entry -> return entry
        _ -> throwSem ("Symbol " ++ k ++ " is not in scope")

findTVar :: Int -> Parser SymbolType
findTVar v = do
    symbols <- getSymbols
    let k = varTypeKey v
    case query k symbols of
        Just (TVarEntry t) -> return t
        _ -> throwSem ("Type var " ++ k ++ " is not in scope")

findType :: Identifier -> Parser [(ConstrIdentifier, [ConstType])]
findType i = do
    symbols <- getSymbols
    case query i symbols of
        Just (TypeEntry constrs) -> return constrs
        _ -> throwSem ("Type symbol " ++ i ++ " is not in scope")

checkTypeInScope :: Identifier -> Parser ()
checkTypeInScope = findType >=> const (return ())

checkVarType :: Int -> Parser ()
checkVarType = findTVar >=> const (return ())

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

paramsToConstFunType :: [ConstType] -> ConstType -> ConstType
paramsToConstFunType [] out = out
paramsToConstFunType (t:ts) out = ConstType (FunType t (paramsToConstFunType ts out))
