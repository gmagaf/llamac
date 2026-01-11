{-# LANGUAGE GADTs #-}
module Semantics.Unifier (inst, gen, checkConstraint, unify) where

import Data.Maybe (isNothing)
import Control.Monad (when, unless)
import Data.Foldable (forM_)
import qualified Data.IntSet as S

import Common.AST (TypeF(..))
import Common.PrintAST (pretty)
import Common.SymbolType(SymbolType(..), TypeScheme (..), constTypeToSymbolType, notVarInType, substScheme, cataM, PosnId (..), printTypePosn)
import Parser.ParserM (Parser)
import Semantics.Utils (throwSem, getUnifier, putUnifier,
                        getConstraints, putConstraints,
                        resolveType, freshTVar, getFreeTVars, removeDuplicates)
import Semantics.TypeConstraints
import Prelude hiding (traverse)


{-
    This module contains all the type theoretic
    functions that we need:
    - instantiation of a type scheme
    - generalization of a type to a type scheme
    - copying the type constraints of two type vars
    - check if a type complies with a type constraint
    - apply the type constraints of two unified types to each other
    - unify two types
-}

-- instantiate a type scheme to a monotype
-- by substituting all bound variables with new free ones
inst :: TypeScheme -> Parser SymbolType
inst (MonoType t)  = return t
inst (AbsType v t) = do
    v' <- freshTVar
    copyConstraints (TVar v, v')
    let substt = substScheme v v' t
    inst substt

-- generalize a monotype to a type scheme
-- by bounding all free variables not found
-- in scope
gen :: SymbolType -> Parser TypeScheme
gen t =
    let varNotInScope :: Int -> Parser [Int]
        varNotInScope v = do
            isFree <- S.member v <$> getFreeTVars
            if isFree then return []
            else do
                mCSet <- lookupConstrTg v NotPolymorphicVarTg <$> getConstraints
                return $ maybe [v] (const []) mCSet
        alg :: TypeF p [Int] -> Parser [Int]
        alg (FunType f1 f2) = return $ f1 ++ f2
        alg (ArrayType _ f) = return f
        alg (RefType f)     = return f
        alg _               = return []
    in do
        varsNotInScope <- cataM (either varNotInScope alg) t
        let varsToBound = removeDuplicates S.empty varsNotInScope
        return $ foldr AbsType (MonoType t) varsToBound

-- Copy the type constraints between two type variables
copyConstraints :: (SymbolType, SymbolType) -> Parser ()
copyConstraints (TVar v, TVar u) = do
    c <- getConstraints
    case (lookupConstr v c, lookupConstr u c) of
        (Nothing, Nothing) -> return ()
        (Nothing, Just cs) -> putConstraints $ insertConstr v cs c
        (Just cs, Nothing) -> putConstraints $ insertConstr u cs c
        (Just vc, Just uc) ->
            let finalC = insertConstrWith union u vc (insertConstrWith union v uc c)
            in putConstraints finalC
copyConstraints _ = return ()

-- Check if a type complies with a type constraint
-- If it doesn't throw an error
checkConstraint :: SymbolType -> TypeConstraint t -> Parser ()
checkConstraint t@(SymType ft) tc = case (ft, tc) of
    (FunType {}, NotAllowedFunType s) ->
        throwSem $ "Type constraint failed: " ++ s
    (_, NotAllowedFunType _) -> return ()
    (ArrayType {}, NotAllowedArrayType s) ->
        throwSem $ "Type constraint failed: " ++ s
    (_, NotAllowedArrayType _) -> return ()
    (ArrayType d _, ArrayOfAtLeastDim l s) ->
        when (d < l) $ throwSem $ "Type constraint failed: " ++ s
    (_, ArrayOfAtLeastDim _ s) ->
        throwSem $ "Type constraint failed: " ++ s
    (_, AllowedTypes ts s) ->
        let eqTypes = any (\ct -> t == constTypeToSymbolType ct) ts
        in unless eqTypes . throwSem $ "Type constraint failed for type " ++ pretty t ++ ": " ++ s
    (UserDefinedType {}, AllowedUserDefinedType _) -> return ()
    (_, AllowedUserDefinedType s) ->
        throwSem $ "Type constraint failed: for type " ++ pretty t ++ ". " ++ s
    (_, NotPolymorphicVar {}) -> return () -- This constraint only makes sense for type variables
checkConstraint (TVar v) c = do
    let tv = TVar v
    f <- getUnifier
    when (isNothing (f tv)) $
        throwSem ("Unable to add constraint: " ++ show c ++
                  " . Variable " ++ pretty tv ++ " has never been used before")
    cs <- getConstraints
    putConstraints $ insertConstrWith union v (singletonSet c) cs

-- Apply the type constraints of a type variable
-- to the unified type
applyConstraints :: (SymbolType, SymbolType) -> Parser ()
applyConstraints (TVar v, TVar u) = copyConstraints (TVar v, TVar u)
applyConstraints (TVar v, t) = do
    c <- getConstraints
    forM_ (lookupConstr v c) (traverse (\ _ -> checkConstraint t))
applyConstraints (t, TVar v) = do
    c <- getConstraints
    forM_ (lookupConstr v c) (traverse (\ _ -> checkConstraint t))
applyConstraints _ = return ()

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
    applyConstraints (rt, rs)
    case (rt, rs) of
        (t, s) | t == s -> return ()
        (TVar v, TVar u) | v < u -> unify (rs, rt)
        (TVar v, s) | notVarInType v s -> putUnifier v s
        (t, TVar v) | notVarInType v t -> putUnifier v t
        (SymType (FunType t1 t2), SymType (FunType s1 s2)) -> do
            unify (t1, s1)
            unify (t2, s2)
        (SymType (ArrayType dimT t), SymType (ArrayType dimS s)) | dimT == dimS -> do
            unify (t, s)
        (SymType (RefType t), SymType (RefType s)) -> do
            unify (t, s)
        (SymType (UserDefinedType tId), SymType (UserDefinedType sId))
            | identifier tId == identifier sId && tId /= sId -> do
            throwSem $ "Unable to unify type " ++
                pretty rt ++ " defined at " ++ printTypePosn tId ++
                " with " ++
                pretty rs ++ " defined at " ++ printTypePosn sId
        _ -> throwSem $ "Unable to unify type " ++ pretty rt ++ " with " ++ pretty rs
