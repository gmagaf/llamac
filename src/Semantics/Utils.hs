module Semantics.Utils (module Semantics.Utils) where

import Data.Foldable (foldrM)
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing)
import Control.Monad ((>=>), when)

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST (Node(..), TypeF(..))
import Common.PrintAST
import Common.SymbolType (SymbolType(..), ConstType(..), TypeScheme (..),
                          substScheme, cata, tvarsInType, freeVarsInScheme)

import Common.SymbolTable
     (closeScope,
      insert,
      openScope,
      query,
      update,
      foldContext,
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
    let g t'@(TVar v') = if v' == v then Just t else Just t'
        g (SymType tf) = SymType <$> mapM g tf
    s <- getSemState
    putSemState s{unifier = f >=> g}

-- Error handling functions
throwSemAtPosn :: String -> AlexPosn -> Parser a
throwSemAtPosn s p = throwAtPosn p (throwSemanticError s)

throwSem :: String -> Parser a
throwSem s = do
    p <- getSemPosn
    throwAtPosn p (throwSemanticError s)

-- The only way to create a new tvar
freshTVar :: Parser SymbolType
freshTVar = do
    c <- getAndIncrTVarC
    let v = TVar c
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
gen :: Set.Set Int -> SymbolType -> TypeScheme
gen freeVars t =
    let varNotInScope :: Int -> [Int]
        varNotInScope v = ([v | not (Set.member v freeVars)])
        alg :: TypeF [Int] -> [Int]
        alg (FunType f1 f2) = f1 ++ f2
        alg (ArrayType _ f) = f
        alg (RefType f)     = f
        alg _               = []
        removeDuplicates :: Set.Set Int -> [Int] -> [Int]
        removeDuplicates _ []     = []
        removeDuplicates s (x:xs) = if Set.member x s
                                    then removeDuplicates s xs
                                    else x:removeDuplicates (Set.insert x s) xs
        varsNotInScope = cata (alg, varNotInScope) t
        varsToBound = removeDuplicates Set.empty varsNotInScope
    in foldr AbsType (MonoType t) varsToBound

-- Parser symbol table utiles
insertName :: String -> TableEntry -> Parser ()
insertName k entry = do
    symbols <- getNames
    putNames $ insert k entry symbols

insertType :: String -> TypeTableEntry -> Parser ()
insertType k entry = do
    symbols <- getTypes
    putTypes $ insert k entry symbols

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

freeVarsInScope :: Parser [Int]
freeVarsInScope = foldContext g [] <$> getNames where
    g :: TableEntry -> [Int] -> [Int]
    g entry acc = case entry of
        MutableEntry st -> tvarsInType st ++ acc
        ArrayEntry st _ -> tvarsInType st ++ acc
        FunEntry sch _  -> freeVarsInScheme sch ++ acc
        ParamEntry st _ -> tvarsInType st ++ acc
        ConstrEntry {}  -> acc

-- Resolution utils
resolveFreeVars :: [Int] -> Parser (Set.Set Int)
resolveFreeVars fv = do
    f <- getUnifier
    let g v s = case f (TVar v) of
            Just st -> return $ Set.union s (Set.fromList $ tvarsInType st)
            Nothing -> throwSem $ "Unable to resolve type var " ++ pretty (TVar v)
    foldrM g Set.empty fv

resolveType :: SymbolType -> Parser SymbolType
resolveType st = do
    f <- getUnifier
    case f st of
        Just t  -> return t
        Nothing -> throwSem $ "Unable to resolve type " ++ pretty st

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
    f <- getUnifier
    aux f s where
        aux :: Unifier -> TypeScheme -> Parser TypeScheme
        aux f (MonoType t')  = case f t' of
            Just t  -> return (MonoType t)
            Nothing -> throwSem $ "Unable to resolve type" ++ pretty t'
        aux f (AbsType v s') =
            let g t@(TVar _)   = if t == TVar v then Just t else f t
                g (SymType tf) = SymType <$> mapM g tf
            in AbsType v <$> aux g s'

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
                updateName k updated
                return updated
        ArrayEntry t dim -> do
            rt <- resolveType t
            let updated = ArrayEntry rt dim
            if rt == t then return entry
            else do
                updateName k updated
                return updated
        FunEntry scheme ps -> do
            rScheme <- resolveTypeScheme scheme
            let updated = FunEntry rScheme ps
            updateName k updated
            return updated
        ParamEntry t i -> do
            rt <- resolveType t
            let updated = ParamEntry rt i
            if rt == t then return entry
            else do
                updateName k updated
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

closeScopeInNames :: Parser ()
closeScopeInNames = do
    symbols <- getNames
    putNames $ closeScope symbols

-- Other util functions
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list