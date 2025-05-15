module Semantics.Unifier (unify) where

import Common.AST (TypeF(..))
import Common.PrintAST (pretty)
import Common.SymbolType(SymbolType(..), notVarInType)
import Parser.ParserM (Parser)
import Semantics.Utils (throwSem, putUnifier, resolveType)

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
