module Semantics.Semantics (Analyzable, sem, semTag,
                            TypeAble, infer, typeCheck,
                            analyzeAST) where

import Control.Monad ((>=>))

import Common.Token (Identifier)
import Common.AST
import Common.PrintAST
import Common.SymbolTable
import Common.SymbolType
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser)
import Semantics.Utils
import Semantics.Unifier
import Semantics.TypeAnalysis

-- This module contains the semantic analysis of the nodes
-- and decorates them with the semantic tag

-- Organize the semantic analyzable nodes in a class
class Node f => Analyzable f where
    sem :: f AlexPosn -> Parser (f SemanticTag)
    semTag :: f AlexPosn -> Parser SemanticTag
    semTag f = tag <$> sem f

class Analyzable f => TypeAble f where
    infer :: f AlexPosn -> Parser SymbolType
    infer f = do
        tg <- semTag f
        case symType tg of
            Nothing -> throwSemAtPosn "Could not infer type of expr" (posn tg)
            Just t  -> return t
    typeCheck :: f AlexPosn -> SymbolType -> Parser Bool
    typeCheck f t = (t ==) <$> infer f

-- Functions for analyzing nodes
analyzeAST :: AST AlexPosn -> Parser (AST SemanticTag)
analyzeAST []                 = return []
analyzeAST (Left def : ast)   = (:) . Left <$> sem def <*> analyzeAST ast
analyzeAST (Right tDef : ast) = (:) . Right <$> sem tDef <*> analyzeAST ast

instance Analyzable TypeDef where
    sem = analyzeTypeDef
instance Analyzable Type where
    sem = analyzeType

-- Semantic analysis of definitions
instance Analyzable LetDef where
    -- TODO: Define
    sem (Let defs p) = do
        preSems <- mapM preSemDef defs
        let semDefs = map fst preSems
        let defEntries = map snd preSems
        putSemPosn p
        mapM_ (uncurry insertName) defEntries
        return $ Let semDefs SemTag{posn = p, symType = Nothing}
    sem (LetRec _ _) = undefined

preSemDef :: Def AlexPosn -> Parser (Def SemanticTag, (Identifier, TableEntry))
preSemDef (VarDef x p) = do
    putSemPosn p
    tv <- freshTVar
    let varType = SymType . RefType $ tv
    let entry = (x, MutableEntry varType)
    let semDef = VarDef x SemTag{posn = p, symType = Nothing}
    return (semDef, entry)
preSemDef (VarDefTyped x t p) = do
    putSemPosn p
    semT <- sem t
    let varType = SymType . RefType $ typeToSymbolType semT
    let entry = (x, MutableEntry varType)
    let semDef = VarDefTyped x semT SemTag{posn = p, symType = Nothing}
    return (semDef, entry)
preSemDef (FunDefTyped i ps t e p) = do
    putSemPosn p
    semT <- sem t
    openScopeInTable
    semPs <- mapM (semParam i) ps
    se <- sem e
    outT <- getExprType se
    paramTypes <- mapM getExprType semPs
    let fT = paramsToFunType paramTypes outT
    putSemPosn p
    unify (typeToSymbolType semT, fT)
    closeScopeInTable
    let paramNames = map ide ps
    let entry = (i, FunEntry (MonoType $ typeToSymbolType t) paramNames)
    let semDef = FunDefTyped i semPs semT se SemTag{posn = p, symType = Nothing}
    return (semDef, entry)
preSemDef (FunDef i ps e p) = do
    putSemPosn p
    openScopeInTable
    semPs <- mapM (semParam i) ps
    se <- sem e
    outT <- (getExprType >=> resolveType) se -- Is this needed??
    paramTypes <- mapM (getExprType >=> resolveType) semPs
    let fType = paramsToFunType paramTypes outT
    rft <- resolveType fType
    closeScopeInTable
    scheme <- gen rft
    let paramNames = map ide ps
    let entry = (i, FunEntry scheme paramNames)
    let semDef = FunDef i semPs se SemTag{posn = p, symType = Nothing}
    return (semDef, entry)
preSemDef _ = undefined

semParam :: Identifier -> Param AlexPosn ->Parser (Param SemanticTag)
semParam fun (TypedParam param t p) = do
    putSemPosn p
    semT <- sem t
    let st = typeToSymbolType t
    insertName param (ParamEntry st fun)
    return $ TypedParam param semT SemTag{posn = p, symType = Just st}
semParam fun (Param param p) = do
    putSemPosn p
    vt <- freshTVar
    insertName param (ParamEntry vt fun)
    return $ Param param SemTag{posn = p, symType = Just vt}

getExprType :: Node n => n SemanticTag -> Parser SymbolType
getExprType e = case symType (tag e) of
    Nothing -> do
        let p = posn $ tag e
        throwSemAtPosn "Unable to compute type of expression" p
    Just t  -> return t

resolveTagType :: Node n => n SemanticTag -> Parser SymbolType
resolveTagType e = case symType (tag e) of
    Nothing -> do
        let p = posn $ tag e
        throwSemAtPosn "Unable to compute type of expression" p
    Just t  -> resolveType t

resolveExpr :: Expr SemanticTag -> Parser (Expr SemanticTag)
resolveExpr e@(Expr ef tg) = do
    rt <- resolveTagType e
    return $ Expr ef tg{symType = Just rt}

retE :: ExprF SemanticTag (Expr SemanticTag) -> SymbolType -> Parser (Expr SemanticTag)
retE ef t = do
    p <- getSemPosn
    return $ Expr ef SemTag{posn = p, symType = Just t}

semE :: ExprF SemanticTag (Expr SemanticTag) -> Parser (Expr SemanticTag)
semE (IntCExpr c)    = retE (IntCExpr c) (SymType IntType)
semE (FloatCExpr c)  = retE (FloatCExpr c) (SymType FloatType)
semE (CharCExpr c)   = retE (CharCExpr c) (SymType CharType)
semE (StringCExpr c) = retE (StringCExpr c) (SymType (ArrayType 1 (SymType CharType)))
semE TrueCExpr       = retE TrueCExpr (SymType BoolType)
semE FalseCExpr      = retE FalseCExpr (SymType BoolType)
semE UnitCExpr       = retE UnitCExpr (SymType UnitType)
semE (ConstExpr i) = do
    entry <- findName i
    case entry of
        FunEntry ft _ -> do
            t <- inst ft
            retE (ConstExpr i) t
        ParamEntry t _ -> do
            rt <- resolveType t
            retE (ConstExpr i) rt
        MutableEntry t -> do
            rt <- resolveType t
            retE (ConstExpr i) rt
        ArrayEntry t _ -> do
            rt <- resolveType t
            retE (ConstExpr i) rt
        ConstrEntry {} -> undefined
semE (ConstConstrExpr i) = do
    entry <- findName i
    case entry of
        FunEntry _ _ -> undefined
        ParamEntry _ _ -> undefined
        MutableEntry _ -> undefined
        ArrayEntry _ _ -> undefined
        ConstrEntry t _ _ -> retE (ConstExpr i) (constTypeToSymbolType t)
semE (FunAppExpr i es) = do
    entry <- findName i
    case entry of
        FunEntry ft ps ->
            -- need to make sure that if there are no args then the type of the function is returned
            case compare (length es) (length ps) of
                LT -> throwSem $ "Function " ++ i ++ " is applied to too few arguments"
                GT -> throwSem $ "Function " ++ i ++ " is applied to too many arguments"
                EQ -> do
                    ts <- mapM getExprType es
                    v <- freshTVar
                    openScopeInTable
                    t <- inst ft
                    let inf = paramsToFunType ts v
                    unify (t, inf)
                    res <- mapM resolveExpr es
                    rv <- resolveType v
                    closeScopeInTable
                    retE (FunAppExpr i res) rv
        ParamEntry t _ -> do
            ts <- mapM getExprType es
            v <- freshTVar
            unify (t, paramsToFunType ts v) -- Need to make sure that if the param is function is applied to all args
            res <- mapM resolveExpr es
            rv <- resolveType v
            rt <- resolveType t
            let argTypes = funTypeToArgTypes rt
            case compare (length es) (length argTypes) of
                LT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty t ++ " is applied to too few arguments"
                GT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty t ++ " is applied to too many arguments"
                EQ -> retE (FunAppExpr i res) rv
        MutableEntry _    -> throwSem $ "Cannot apply arguments to the mutable variable " ++ i
        ArrayEntry _ _    -> throwSem $ "Cannot apply arguments to array " ++ i
        ConstrEntry {}    -> throwSem $ "Cannot apply function arguments to constr " ++ i
semE e@(ArrayDim i dim) = findName i >>= run where
    run (ArrayEntry _ dims)
      | dim < 1 = throwSem $ "Cannot compute the " ++ show dim ++ " dimension of array " ++ i
      | dims < dim = throwSem $ "Cannot compute the " ++ show dim ++ " dimension of " ++ show dims ++ "-dim array " ++ i
      | otherwise = retE e (SymType IntType)
    run _ = throwSem $ "No array " ++ i ++ " in scope"
semE ef@(LetIn _ e) = do
    closeScopeInTable
    t <- getExprType e
    retE ef t
-- TODO: Define
semE _ = undefined

semExprF :: (ExprF SemanticTag (Expr SemanticTag) -> Parser (Expr SemanticTag))
         -> Expr AlexPosn
         -> Parser (Expr SemanticTag)
semExprF g (Expr ef p) = do
    semEf <- mapMExprF sem sem sem (semExprF g) ef
    putSemPosn p
    g semEf

instance Analyzable Expr where
    sem = semExprF semE

instance TypeAble Expr where

instance Analyzable Clause where
    -- TODO: Define
    sem _ = undefined

instance Analyzable Pattern where
    -- TODO: Define
    sem _ = undefined
