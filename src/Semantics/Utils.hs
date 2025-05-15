module Semantics.Utils (module Semantics.Utils) where

import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing)
import Control.Monad ((>=>), when)

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST (Node(..), TypeF(..))
import Common.PrintAST
import Common.SymbolType (SymbolType(..), ConstType(..), TypeScheme (..),
                          substScheme, cataMSymbolType )

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
import Parser.ParserState (SemanticState(..), Unifier)
import Debug.Trace (trace)

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

getUnifier :: Parser Unifier
getUnifier = unifier <$> getSemState

addUnifier :: Int -> Parser ()
addUnifier v = do
    let tv = TVar v
    f <- getUnifier
    when (isJust (f tv)) $
        throwSem ("Fresh variable " ++ pretty tv ++ " has been used before")
    let g t@(TVar _)   = if t == tv then Just tv else f t
        g (SymType tf) = SymType <$> mapM g tf
    s <- getSemState
    putSemState s{unifier = g}

putUnifier :: Int -> SymbolType -> Parser ()
putUnifier v t = do
    let tv = TVar v
    f <- getUnifier
    when (isNothing (f tv)) $
        throwSem ("Variable " ++ pretty tv ++ " has been used before")
    when (isNothing (f t)) $
        throwSem ("Type " ++ pretty t ++ " contains variables never used before")
    let g t'@(TVar v') = if v' == v then f t else f t'
        g (SymType tf) = SymType <$> mapM g tf
    s <- getSemState
    putSemState s{unifier = g}

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
    putTypes $ insert (varTypeKey c) TVarEntry symbols
    addUnifier c
    return v

-- instantiate a type scheme to a monotype
-- by substituting all bound variables with new free ones
inst :: TypeScheme -> Parser SymbolType
inst (MonoType t)  = return t
inst (AbsType v t) = do
    v' <- freshTVar
    let substt = substScheme v v' t
    inst substt

-- generalize a monotype to a type scheme
-- by bounding all free variables not found
-- in scope
gen :: SymbolType -> Parser TypeScheme
gen t =
    let varNotInScope :: Int -> Parser [Int]
        varNotInScope v = do
            entry <- queryType (varTypeKey v)
            case entry of
                Just TVarEntry -> return []
                _              -> return [v]
        alg :: TypeF [Int] -> Parser [Int]
        alg (FunType f1 f2) = return (f1 ++ f2)
        alg (ArrayType _ f) = return f
        alg (RefType f)     = return f
        alg _               = return []
    in do
        varsNotInScope <- cataMSymbolType (alg, varNotInScope) t
        return $ foldr AbsType (MonoType t) $ Set.fromList varsNotInScope

-- Update symbol if exists else throw error
updateName :: String -> TableEntry -> Parser ()
updateName key entry = do
    symbols <- getNames
    case query key symbols of
        Just _ -> putNames $ update key entry symbols
        _ -> throwSem ("Cannot update symbol " ++ key ++ " as it is not in scope")

-- Query for a symbol that may or may not be in scope
queryName :: String -> Parser (Maybe TableEntry)
queryName k = query k <$> getNames

queryType :: String -> Parser (Maybe TypeTableEntry)
queryType k = query k <$> getTypes

-- Resolution utils
resolveType :: SymbolType -> Parser SymbolType
resolveType st = do
    f <- getUnifier
    case f st of
        Just t  -> do
            checkTVarsInScope t
            return t where
            checkTVarsInScope :: SymbolType -> Parser ()
            checkTVarsInScope (TVar v) = checkVarType v
            checkTVarsInScope (SymType tf) = mapM_ checkTVarsInScope tf
        Nothing -> throwSem $ "Unable to resolve type" ++ pretty st

resolveNodeType :: Node n => n SemanticTag -> Parser (n SemanticTag)
resolveNodeType n = case typeInfo (tag n) of
    NodeType t -> do
        rt <- resolveType t
        let mn = mapTag (\tg -> tg{typeInfo = NodeType rt}) n
        return mn
    _          -> do
        let p = posn $ tag n
        throwSemAtPosn "Unable to compute type of node" p

resolveTypeScheme :: TypeScheme -> Parser TypeScheme
resolveTypeScheme s = do
    openScopeInTable
    t <- inst s
    rt <- resolveType t
    closeScopeInTable
    gen rt

-- Find symbol if exists else throw error
findName :: String -> Parser TableEntry
findName k = let
    aux k' = do
        symbols <- getNames
        case query k' symbols of
            Just entry -> return entry
            _ -> throwSem ("Symbol " ++ k' ++ " is not in scope")
    in do
    entry <- aux k
    case entry of
        MutableEntry t -> do
            rt <- resolveType t
            let updated = MutableEntry rt
            if rt == t then return entry
            else do
                updateName k (trace ("UPDATE ON FIND" ++ k) updated)
                return updated
        ArrayEntry t dim -> do
            rt <- resolveType t
            let updated = ArrayEntry rt dim
            if rt == t then return entry
            else do
                updateName k (trace ("UPDATE ON FIND" ++ k) updated)
                return updated
        FunEntry scheme ps -> do
            rScheme <- resolveTypeScheme scheme
            let updated = FunEntry rScheme ps
            updateName k (trace ("UPDATE ON FIND" ++ k) updated)
            return updated
        ParamEntry t i -> do
            rt <- resolveType t
            let updated = ParamEntry rt i
            if rt == t then return entry
            else do
                updateName k (trace ("UPDATE ON FIND" ++ k) updated)
                return updated
        -- constructors can not have var types
        ConstrEntry {} -> return entry

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
checkVarType v = do
    symbols <- getTypes
    let k = varTypeKey v
    case query k symbols of
        Just TVarEntry -> return ()
        _ -> throwSem ("Type var " ++ k ++ " is not in scope")

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