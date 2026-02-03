module Semantics.Utils (module Semantics.Utils) where

import qualified Data.IntSet as S
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing)
import Control.Monad ((>=>), when, unless)
import Control.Lens.Setter ((<~))

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST (Node(..), Type (Type), TypeF (UserDefinedType))
import Common.PrintAST
import Common.SymbolType (SymbolType(..), ConstType(..), TypeScheme (..), PosnId (identifier), printTypePosn, tvarsInType, typeTo2)
import Common.SymbolTable (Context, NameSpace, FullTableEntry, TableEntry(..), TypeTableEntry(..), names)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser,
    getSemState, putSemState,
    throwSemanticError, throwAtPosn, putPosn, getPosn)
import Parser.ParserState (symbols)
import Parser.SymbolTableUtils (getNames, getTypes, queryP, updateP, insertNameP, insertTypeP)
import Semantics.TypeConstraints (ConstraintsMap)
import Semantics.SemanticState (SemanticState(..), Unifier)

-- This module contains semantic analysis tools
data TypeInfo = NotTypable
              | DefType TypeScheme
              | NodeType SymbolType
    deriving (Show, Eq)

data SemanticTag = SemTag {
                    posn     :: AlexPosn,
                    typeInfo :: TypeInfo
                    }
    deriving (Show, Eq)

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
getSemPosn :: Parser AlexPosn
getSemPosn = getPosn

putSemPosn :: AlexPosn -> Parser ()
putSemPosn = putPosn

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
        throwSem ("Variable " ++ pretty tv ++ " has never been used before")
    when (isNothing (f t)) $
        throwSem ("Type " ++ pretty t ++ " contains variables never used before")
    let g t'@(TVar v') = if v' == v then Just t else Just t'
        g (SymType tf) = SymType <$> mapM g tf
    s <- getSemState
    putSemState s{unifier = f >=> g}

getConstraints :: Parser ConstraintsMap
getConstraints = constraints <$> getSemState

getFreeTVars :: Parser S.IntSet
getFreeTVars = freeTVars <$> getSemState

putFreeTVars :: S.IntSet -> Parser ()
putFreeTVars set = do
    s <- getSemState
    putSemState s{freeTVars = set}

addFreeTVars :: SymbolType -> Parser ()
addFreeTVars (TVar v) = do
    vars <- getFreeTVars
    putFreeTVars (S.insert v vars)
addFreeTVars (SymType tf) = mapM_ addFreeTVars tf

putConstraints :: ConstraintsMap -> Parser ()
putConstraints c = do
    s <- getSemState
    putSemState s{constraints = c}

-- Error handling functions
throwSemAtPosn :: String -> AlexPosn -> Parser a
throwSemAtPosn s p = throwAtPosn p (throwSemanticError s)

throwSem :: String -> Parser a
throwSem s = do
    p <- getSemPosn
    throwAtPosn p (throwSemanticError s)

-- The only way to create a new tvar
freshTVarC :: Parser (SymbolType, Int)
freshTVarC = do
    sState <- getSemState
    let c = varTypeC sState
    putSemState sState{varTypeC = c + 1}
    let v = TVar c
    addUnifier c
    return (v, c)

freshTVar :: Parser SymbolType
freshTVar = fst <$> freshTVarC

-- Parser symbol table utiles
-- function aliases
query :: Ord k => k -> Context k (FullTableEntry e g) -> Maybe e
query = queryP

update :: String -> TableEntry -> NameSpace -> NameSpace
update = updateP

-- Update symbol if exists else throw error
updateName :: String -> TableEntry -> Parser ()
updateName key entry = do
    ns <- getNames
    symbols . names <~ case query key ns of
        Just _ -> return $ update key entry ns
        _ -> throwSem ("Cannot update symbol " ++ key ++ " as it is not in scope")

-- Resolution utils
resolveFreeVars :: Parser ()
resolveFreeVars = do
    f <- getUnifier
    fvs <- getFreeTVars
    let g s v = case f (TVar v) of
            Just st -> S.union (S.fromList $ tvarsInType st) <$> s
            Nothing -> throwSem $ "Unable to resolve type var " ++ pretty (TVar v)
    fvs' <- S.foldl' g (pure S.empty) fvs
    putFreeTVars fvs'

resolveType :: SymbolType -> Parser SymbolType
resolveType st = do
    f <- getUnifier
    case f st of
        Just t  -> return t
        Nothing -> throwSem $ "Unable to resolve type " ++ pretty st

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

resolveTableEntry :: TableEntry -> Parser TableEntry
resolveTableEntry entry = case entry of
    MutableEntry t -> do
        rt <- resolveType t
        let updated = MutableEntry rt
        return updated
    ArrayEntry t dim -> do
        rt <- resolveType t
        let updated = ArrayEntry rt dim
        return updated
    FunEntry scheme ps -> do
        rScheme <- resolveTypeScheme scheme
        let updated = FunEntry rScheme ps
        return updated
    ParamEntry t i -> do
        rt <- resolveType t
        let updated = ParamEntry rt i
        return updated
    PatternEntry t -> do
        rt <- resolveType t
        let updated = PatternEntry rt
        return updated
    -- constructors can not have var types
    ConstrEntry {} -> return entry

-- Find symbol if exists else throw error
findName :: String -> Parser TableEntry
findName k = do
    ns <- getNames
    case query k ns of
        Just entry -> resolveTableEntry entry
        _ -> throwSem ("Symbol " ++ k ++ " is not in scope")

findType :: ConstType -> Parser [(ConstrIdentifier, [ConstType])]
findType t@(ConstType tf) = do
    case tf of
        UserDefinedType posnId -> do
            let i = identifier posnId
            ts <- getTypes
            case query i ts of
                Just (TypeEntry posnId' constrs) -> do
                    unless (posnId == posnId') $ throwSem ("Type symbol " ++ i ++ " defined at: " ++ printTypePosn posnId ++ " is not in scope")
                    return constrs
                _ -> throwSem ("Type symbol " ++ i ++ " is not in scope")
        _ -> throwSem ("Symbol table only contains user defined types. Not: " ++ pretty t)

-- Check that type is in scope else throw error
checkTypeInScope :: ConstType -> Parser ()
checkTypeInScope = findType >=> const (return ())

-- function aliases
insertType :: String -> TypeTableEntry -> Parser ()
insertType = insertTypeP

insertName :: String -> TableEntry -> Parser ()
insertName = insertNameP

-- Other util functions
typeToSymbolType :: Type SemanticTag -> Parser SymbolType
typeToSymbolType = typeTo2 aux where
    aux (Type _ tg) i = do
        putSemPosn (posn tg)
        findTypeId i

findTypeId :: Identifier -> Parser PosnId
findTypeId i = do
    ts <- getTypes
    case query i ts of
        Just (TypeEntry posnId _) -> return posnId
        _ -> throwSem ("Type symbol " ++ i ++ " is not in scope")

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

removeDuplicates :: S.IntSet -> [Int] -> [Int]
removeDuplicates _ []     = []
removeDuplicates s (x:xs) = if S.member x s
                            then removeDuplicates s xs
                            else x:removeDuplicates (S.insert x s) xs