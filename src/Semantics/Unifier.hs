module Semantics.Unifier (bottom, inst, gen,
                          resolveType, resolveTypeScheme,
                          resolveNodeType, findResolve,
                          unify) where

import qualified Data.Set as Set

import Common.AST (Node(..), TypeF(..))
import Common.SymbolTable (TypeTableEntry(TVarEntry), TableEntry(..), varTypeKey)
import Common.PrintAST (pretty)
import Common.SymbolType (TypeScheme(..), SymbolType(..))
import Parser.ParserM (Parser)
import Semantics.Utils (SemanticTag(..), TypeInfo(..),
    throwSemAtPosn, throwSem, freshTVar, updateTVar, queryType, findTVar,
    openScopeInTable, closeScopeInTable, findName, updateName, getAndIncrTVarC)


-- Recursion Utils
bottomUp :: (SymbolType -> SymbolType) -> SymbolType -> SymbolType
bottomUp f (SymType t) = f (SymType $ fmap (bottomUp f) t)
bottomUp f v@(TVar _)  = f v

-- bottomUpM :: Monad m => (SymbolType -> m SymbolType) -> SymbolType -> m SymbolType
-- bottomUpM f (SymType t) = mapM (bottomUpM f) t >>= f . SymType
-- bottomUpM f v@(TVar _)  = f v

-- cata :: (TypeF a -> a, Int -> a) -> SymbolType -> a
-- cata alg@(f, _) (SymType t) = f $ fmap (cata alg) t
-- cata (_, g) (TVar v) = g v

cataM :: Monad m => (TypeF a -> m a, Int -> m a) -> SymbolType -> m a
cataM alg@(f, _) (SymType t) = mapM (cataM alg) t >>= f
cataM (_, g) (TVar v) = g v

-- Type theoretic utils
bottom :: Parser TypeScheme
bottom = do
    c <- getAndIncrTVarC
    let vt = TVar c
    return $ AbsType c (MonoType vt)

subst :: Int -> SymbolType -> SymbolType -> SymbolType
subst v t = bottomUp f where
    f :: SymbolType -> SymbolType
    f s'@(TVar u) | v == u = t
                  | otherwise = s'
    f s' = s'

substScheme :: Int -> SymbolType -> TypeScheme -> TypeScheme
substScheme v r (MonoType t) = MonoType $ subst v r t
substScheme v r (AbsType u t) | v == u    = AbsType u t
                              | otherwise = AbsType u $ substScheme v r t

tvarInType :: Int -> SymbolType -> Bool
tvarInType v (TVar u)
                | v == u = True
                | otherwise = False
tvarInType v (SymType t) = any (tvarInType v) t

notVarInType :: Int -> SymbolType -> Bool
notVarInType v = not . tvarInType v

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
                Just (TVarEntry _) -> return []
                _                  -> return [v]
        alg :: TypeF [Int] -> Parser [Int]
        alg (FunType f1 f2) = return (f1 ++ f2)
        alg (ArrayType _ f) = return f
        alg (RefType f)     = return f
        alg _               = return []
    in do
        varsNotInScope <- cataM (alg, varNotInScope) t
        return $ foldr AbsType (MonoType t) $ Set.fromList varsNotInScope

-- Resolution utils
resolveType :: SymbolType -> Parser SymbolType
resolveType st@(TVar v) = do
    vt <- findTVar v
    if vt == st then return vt
    else do
        rt <- resolveType vt
        updateTVar v rt
        return rt
resolveType (SymType tf) = SymType <$> mapM resolveType tf

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

findResolve :: String -> Parser TableEntry
findResolve k = do
    entry <- findName k
    case entry of
        MutableEntry t -> do
            rt <- resolveType t
            let updated = MutableEntry rt
            updateName k updated
            return updated
        ArrayEntry t dim -> do
            rt <- resolveType t
            let updated = ArrayEntry rt dim
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
            updateName k updated
            return updated
        -- constructors can not have var types
        ConstrEntry {} -> return entry

-- This function is used to unify two types
-- Given a type constraint it extends the unifier
-- by resolving the constraint.
-- The constraint is solved in favor of the less
-- variable if possible
-- The result is saved in SymbolTable
unify :: (SymbolType, SymbolType) -> Parser ()
unify (st1, st2) = do
    rt <- resolveType st1
    rs <- resolveType st2
    case (rt, rs) of
        (t, s) | t == s -> return ()
        (TVar v, TVar u) | v < u -> unify (rs, rt)
        (TVar v, s) | notVarInType v s -> updateTVar v s
        (t, TVar v) | notVarInType v t -> updateTVar v t
        (SymType (FunType t1 t2), SymType (FunType s1 s2)) -> do
            unify (t1, s1)
            unify (t2, s2)
        (SymType (ArrayType dimT t), SymType (ArrayType dimS s)) | dimT == dimS -> do
            unify (t, s)
        (SymType (RefType t), SymType (RefType s)) -> do
            unify (t, s)
        _ -> throwSem $ "Unable to unify type " ++ pretty st1 ++ " with " ++ pretty st2 ++
            " resolved as " ++ pretty rt ++ " and " ++ pretty rs ++ " respectively"
