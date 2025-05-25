module Semantics.Unifier (checkConstraint, unify) where

import Data.Maybe (isNothing)
import qualified Data.Map as Map
import Control.Monad (when, unless)
import Data.Foldable (forM_)

import Common.AST (TypeF(..))
import Common.PrintAST (pretty)
import Common.SymbolType(SymbolType(..), constTypeToSymbolType, notVarInType)
import Parser.ParserM (Parser)
import Semantics.Utils (throwSem, getUnifier, putUnifier,
                        getConstraints, putConstraints,
                        resolveType, copyConstraints)
import Semantics.SemanticState (TypeConstraint (..), constraintsToList,
                                addConstraints, fromConstraint)

checkConstraint :: SymbolType -> TypeConstraint -> Parser ()
checkConstraint (SymType (FunType _ _)) (NotAllowedFunType s) =
    throwSem $ "Type constraint failed: " ++ s
checkConstraint (SymType _) (NotAllowedFunType _) = return ()
checkConstraint (SymType (ArrayType _ _)) (NotAllowedArrayType s) =
    throwSem $ "Type constraint failed: " ++ s
checkConstraint (SymType _) (NotAllowedArrayType _) = return ()
checkConstraint (SymType (ArrayType d _)) (ArrayOfAtLeastDim l s) =
    when (d < l) $ throwSem $ "Type constraint failed: " ++ s
checkConstraint (SymType _) (ArrayOfAtLeastDim _ _) = return ()
checkConstraint t@(SymType _) (AllowedTypes ts s) =
    let eqTypes = any (\ct -> t == constTypeToSymbolType ct) ts
    in unless eqTypes . throwSem $ "Type constraint failed for type " ++ pretty t ++ ": " ++ s
checkConstraint (SymType (UserDefinedType _)) (AllowedUserDefinedType _) = return ()
checkConstraint t@(SymType _) (AllowedUserDefinedType s) =
    throwSem $ "Type constraint failed: for type " ++ pretty t ++ ". " ++ s
checkConstraint (TVar v) c = do
    let tv = TVar v
    f <- getUnifier
    when (isNothing (f tv)) $
        throwSem ("Unable to add constraint: " ++ show c ++
                  " . Variable " ++ pretty tv ++ " has never been used before")
    cs <- getConstraints
    let g = addConstraints
    putConstraints $ Map.insertWith g v (fromConstraint c) cs

applyConstraints :: (SymbolType, SymbolType) -> Parser ()
applyConstraints (TVar v, TVar u) = copyConstraints (TVar v, TVar u)
applyConstraints (TVar v, t) = do
    c <- getConstraints
    forM_ (Map.lookup v c) (mapM_ (checkConstraint t) . constraintsToList)
applyConstraints (t, TVar v) = do
    c <- getConstraints
    forM_ (Map.lookup v c) (mapM_ (checkConstraint t) . constraintsToList)
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
        _ -> throwSem $ "Unable to unify type " ++ pretty rt ++ " with " ++ pretty rs
