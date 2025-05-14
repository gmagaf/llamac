module Semantics.Utils (module Semantics.Utils) where

import qualified Data.Set as Set
import Control.Monad ((>=>))

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST (Node(..))
import Common.SymbolType (SymbolType(..), ConstType(..), TypeScheme (..))
import Common.SymbolTable
     (closeScope,
      insert,
      openScope,
      query,
      update,
      varTypeKey,
      SymbolTable(types, names),
      TableEntry(..),
      TypeTableEntry(..),
      NameSpace,
      TypeSpace)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser,
    getSymbols, getSemState, putSymbols, putSemState,
    throwSemanticError, throwAtPosn)
import Parser.ParserState (SemanticState(..))

-- This module contains semantic analysis tools
data TypeInfo = NotTypable
              | DefType TypeScheme
              | NodeType SymbolType
    deriving Show

data SemanticTag = SemTag {
                    posn     :: AlexPosn,
                    typeInfo :: TypeInfo
                    }
    deriving Show

cpPosn :: AlexPosn -> SemanticTag
cpPosn p = SemTag {posn = p, typeInfo = NotTypable}

getNodeType :: Node n => n SemanticTag -> Parser SymbolType
getNodeType n = case typeInfo (tag n) of
    NodeType t -> return t
    _          -> do
        let p = posn $ tag n
        throwSemAtPosn "Unable to compute type of node" p

getDefScheme :: Node n => n SemanticTag -> Parser TypeScheme
getDefScheme n = case typeInfo (tag n) of
    DefType t -> return t
    _         -> do
        let p = posn $ tag n
        throwSemAtPosn "Unable to compute type scheme of node" p

-- Functions for dealing with the Semantic state of the parser
getNames :: Parser NameSpace
getNames = names <$> getSymbols

getTypes :: Parser TypeSpace
getTypes = types <$> getSymbols

putNames :: NameSpace -> Parser ()
putNames ns = do
    s <- getSymbols
    putSymbols s{names = ns}

putTypes :: TypeSpace -> Parser ()
putTypes ts = do
    s <- getSymbols
    putSymbols s{types = ts}

getSemPosn :: Parser AlexPosn
getSemPosn = posnOfSem <$> getSemState

putSemPosn :: AlexPosn -> Parser ()
putSemPosn p = do
    s <- getSemState
    putSemState s{posnOfSem = p}

getAndIncrTVarC :: Parser Int
getAndIncrTVarC = do
    sState <- getSemState
    let c = varTypeC sState
    putSemState sState{varTypeC = c + 1}
    return c

-- Error handling functions
throwSemAtPosn :: String -> AlexPosn -> Parser a
throwSemAtPosn s p = throwAtPosn p (throwSemanticError s)

throwSem :: String -> Parser a
throwSem s = do
    p <- getSemPosn
    throwAtPosn p (throwSemanticError s)

-- Parser symbol table utiles
insertName :: String -> TableEntry -> Parser ()
insertName k entry = do
    symbols <- getNames
    putNames $ insert k entry symbols

insertType :: String -> TypeTableEntry -> Parser ()
insertType k entry = do
    symbols <- getTypes
    putTypes $ insert k entry symbols

-- The only way to create a new tvar
freshTVar :: Parser SymbolType
freshTVar = do
    c <- getAndIncrTVarC
    let v = TVar c
    symbols <- getTypes
    putTypes $ insert (varTypeKey c) (TVarEntry v) symbols
    return v

-- Update symbol if exists else throw error
updateName :: String -> TableEntry -> Parser ()
updateName key entry = do
    symbols <- getNames
    case query key symbols of
        Just _ -> putNames $ update key entry symbols
        _ -> throwSem ("Cannot update symbol " ++ key ++ " as it is not in scope")

updateTVar :: Int -> SymbolType -> Parser ()
updateTVar v t = do
    let key = varTypeKey v
    symbols <- getTypes
    case query key symbols of
        Just (TVarEntry _) -> putTypes $ update key (TVarEntry t) symbols
        _ -> throwSem ("Type var " ++ key ++ " is not in scope")

-- Query for a symbol that may or may not be in scope
queryName :: String -> Parser (Maybe TableEntry)
queryName k = query k <$> getNames

queryType :: String -> Parser (Maybe TypeTableEntry)
queryType k = query k <$> getTypes

-- Find symbol if exists else throw error
findName :: String -> Parser TableEntry
findName k = do
    symbols <- getNames
    case query k symbols of
        Just entry -> return entry
        _ -> throwSem ("Symbol " ++ k ++ " is not in scope")

findTVar :: Int -> Parser SymbolType
findTVar v = do
    symbols <- getTypes
    let k = varTypeKey v
    case query k symbols of
        Just (TVarEntry t) -> return t
        _ -> throwSem ("Type var " ++ k ++ " is not in scope")

findType :: Identifier -> Parser [(ConstrIdentifier, [ConstType])]
findType i = do
    symbols <- getTypes
    case query i symbols of
        Just (TypeEntry constrs) -> return constrs
        _ -> throwSem ("Type symbol " ++ i ++ " is not in scope")

-- Check that type is in scope else throw error
checkTypeInScope :: Identifier -> Parser ()
checkTypeInScope = findType >=> const (return ())

checkVarType :: Int -> Parser ()
checkVarType = findTVar >=> const (return ())

-- Handle scopes
openScopeInTypes :: Parser ()
openScopeInTypes = do
    symbols <- getTypes
    putTypes $ openScope symbols

closeScopeInTypes :: Parser ()
closeScopeInTypes = do
    symbols <- getTypes
    putTypes $ closeScope symbols

openScopeInNames :: Parser ()
openScopeInNames = do
    symbols <- getNames
    putNames $ openScope symbols

openScopeInTable :: Parser ()
openScopeInTable = do
    openScopeInNames
    openScopeInTypes

closeScopeInNames :: Parser ()
closeScopeInNames = do
    symbols <- getNames
    putNames $ closeScope symbols

closeScopeInTable :: Parser ()
closeScopeInTable = do
    closeScopeInNames
    closeScopeInTypes

-- Other util functions
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list