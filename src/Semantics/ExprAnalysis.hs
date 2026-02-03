module Semantics.ExprAnalysis(analyzeExpr) where

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST
import Common.PrintAST (Pretty(pretty))
import Common.SymbolTable
import Common.SymbolType (TypeScheme(..), SymbolType(..), ConstType(..),
                          constTypeToSymbolType,
                          funToArgs, paramsToFun)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser, stackTrace, throwInternalError)
import Parser.SymbolTableUtils (openScopeInNames, closeScopeInNames)
import Semantics.TypeConstraints (TypeConstraint(..), mkAllowedTypes)
import Semantics.Utils
import Semantics.Unifier (inst, checkConstraint, unify)
import Semantics.TypeAnalysis (analyzeType)
import {-# SOURCE #-} Semantics.LetAnalysis (analyzeLet)

-- Semantic analysis of expressions

analyzeExpr :: Expr AlexPosn -> Parser (Expr SemanticTag)
analyzeExpr expr =
    let tTag        = putSemPosn
        tExpr       = indExprF analyzeExpr
        tExprF      = indSemExpr
        tType       = analyzeType
        tNewType t  = stackTrace ("while analyzing type " ++ pretty t) . semNewType $ t
        tLet        = analyzeLet
        tLetIn      = semLetIn
        tMatchExpr  = analyzeExpr
        tClause     = analyzeClause
        tMatch e cs = stackTrace ("while analyzing pattern to match " ++ pretty e) (semMatchExpr e cs)
    in stackTrace ("while analyzing expr " ++ pretty expr) $
        traverseExpr tTag (tExpr, tExprF) (tType, tNewType) (tLet, analyzeExpr, tLetIn) (tMatchExpr, tClause, tMatch) expr

-- Define a function to traverse the expression
-- The traversal goes with the following order:
-- 1. children nodes
-- 2. tag
-- 3. parent node
traverseExpr :: Monad m
             => (a -> m ())
             -> (ExprF (Expr a) -> m (ExprF (Expr b)), ExprF (Expr b) -> m (Expr b))
             -> (Type a -> m (Type b), Type b -> m (Expr b))
             -> (LetDef a -> m (LetDef b), Expr a -> m (Expr b), LetDef b -> Expr b -> m (Expr b))
             -> (Expr a -> m (Expr b), Clause a -> m (Clause b), Expr b -> [Clause b] -> m (Expr b))
             -> Expr a
             -> m (Expr b)
traverseExpr tTag (tExp, tExprF) (tType, tNewType) (tLet, tLetExp, tLetIn) (tMatchExpr, tClause, tMatch) expr = do
    case expr of
        (Expr tf a) -> do
            tTf <- tExp tf
            tTag a
            tExprF tTf
        NewType t tg -> do
            bType <- tType t
            tTag tg
            tNewType bType
        LetIn l e a -> do
            tl <- tLet l
            te <- tLetExp e
            tTag a
            tLetIn tl te
        MatchExpr e cs a -> do
            te <- tMatchExpr e
            tcs <- mapM  tClause cs
            tTag a
            tMatch te tcs

-- Inductively analyze expression functors using mapM
indExprF :: (Expr a -> Parser (Expr b)) -> ExprF (Expr a) -> Parser (ExprF (Expr b))
indExprF aexpr (ForExpr i l u e) = do
    semL <- aexpr l
    semU <- aexpr u
    openScopeInNames
    insertName i (FunEntry (MonoType . SymType $ IntType) [])
    semE <- aexpr e
    closeScopeInNames
    return (ForExpr i semL semU semE)
indExprF aexpr (ForDownExpr i u l e) = do
    semU <- aexpr u
    semL <- aexpr l
    openScopeInNames
    insertName i (FunEntry (MonoType . SymType $ IntType) [])
    semE <- aexpr e
    closeScopeInNames
    return (ForDownExpr i semU semL semE)
indExprF aexpr ef = mapM aexpr ef

-- Util for returning the result expression by updating its node type
retE :: ExprF (Expr SemanticTag) -> SymbolType -> Parser (Expr SemanticTag)
retE ef t = do
    p <- getSemPosn
    return $ Expr ef SemTag{posn = p, typeInfo = NodeType t}

indSemExpr :: ExprF (Expr SemanticTag) -> Parser (Expr SemanticTag)
indSemExpr (IntCExpr c)           = retE (IntCExpr c) (SymType IntType)
indSemExpr (FloatCExpr c)         = retE (FloatCExpr c) (SymType FloatType)
indSemExpr (CharCExpr c)          = retE (CharCExpr c) (SymType CharType)
indSemExpr (StringCExpr c)        = retE (StringCExpr c) (SymType (ArrayType 1 (SymType CharType)))
indSemExpr TrueCExpr              = retE TrueCExpr (SymType BoolType)
indSemExpr FalseCExpr             = retE FalseCExpr (SymType BoolType)
indSemExpr UnitCExpr              = retE UnitCExpr (SymType UnitType)
indSemExpr (ConstExpr i)          = semConstExpr i
indSemExpr (ConstConstrExpr i)    = semConstConstrExpr i
indSemExpr (FunAppExpr i es)      = semFunAppExpr i es
indSemExpr (ConstrAppExpr i es)   = semConstrAppExpr i es
indSemExpr (ArrayDim i dim)       = semArrayDim i dim
indSemExpr (UnOpExpr op e)        = semUnOp op e
indSemExpr (BinOpExpr op d e)     = semBinOp op d e
indSemExpr (ArrayAccess i es)     = semArrayAccess i es
indSemExpr (DeleteExpr e)         = semDeleteExpr e
indSemExpr (BeginExpr e)          = getNodeType e >>= retE (BeginExpr e)
indSemExpr (IfThenElseExpr c d e) = semIfThenElseExpr c d e
indSemExpr (IfThenExpr c e)       = semIfThenExpr c e
indSemExpr (WhileExpr c e)        = semWhileExpr c e
indSemExpr (ForExpr i l u e)      = semForExpr i l u e
indSemExpr (ForDownExpr i u l e)  = semForDownExpr i u l e

-- All functions implementing the inductive step
-- of the analysis of the expression for each case

semConstExpr :: Identifier -> Parser (Expr SemanticTag)
semConstExpr i = do
    entry <- findName i
    case entry of
        FunEntry ft _ -> do
            t <- inst ft
            retE (ConstExpr i) t
        ParamEntry t _ -> do
            retE (ConstExpr i) t
        MutableEntry t -> do
            retE (ConstExpr i) t
        ArrayEntry t _ -> do
            retE (ConstExpr i) t
        PatternEntry t -> do
            retE (ConstExpr i) t
        ConstrEntry {} -> throwInternalError $
            "Constr entry: " ++ show entry ++ " is not expected for identifier key " ++ i

semConstConstrExpr :: ConstrIdentifier -> Parser (Expr SemanticTag)
semConstConstrExpr i = do
    entry <- findName i
    case entry of
        ConstrEntry _ [] outT -> do
            retE (ConstConstrExpr i) (constTypeToSymbolType outT)
        _                 -> throwInternalError $
            "Entry: " ++ show entry ++ " is not expected for constructor identifier key " ++ i

semFunAppExpr :: Identifier -> [Expr SemanticTag] -> Parser (Expr SemanticTag)
semFunAppExpr i es = do
    entry <- findName i
    case entry of
        FunEntry ft ps ->
            case compare (length es) (length ps) of
                LT -> throwSem $ "Function " ++ i ++ " is applied to too few arguments"
                GT -> throwSem $ "Function " ++ i ++ " is applied to too many arguments"
                EQ -> do
                    ts <- mapM getNodeType es
                    v <- freshTVar
                    t <- inst ft
                    let inf = paramsToFun ts v
                    unify (t, inf)
                    checkConstraint v (NotAllowedFunType $ "Function " ++ i ++ " cannot return function type: " ++ pretty v)
                    retE (FunAppExpr i es) v
        ParamEntry t _ -> do
            ts <- mapM getNodeType es
            v <- freshTVar
            let inf = paramsToFun ts v
            unify (t, inf)
            rt <- resolveType t
            let argTypes = funToArgs rt
            case compare (length es) (length argTypes) of
                LT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty rt ++ " is applied to too few arguments"
                GT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty rt ++ " is applied to too many arguments"
                EQ -> retE (FunAppExpr i es) v
        PatternEntry t -> do
            ts <- mapM getNodeType es
            v <- freshTVar
            let inf = paramsToFun ts v
            unify (t, inf)
            rt <- resolveType t
            let argTypes = funToArgs rt
            case compare (length es) (length argTypes) of
                LT -> throwSem $ "Pattern " ++ i ++ " of type " ++ pretty rt ++ " is applied to too few arguments"
                GT -> throwSem $ "Pattern " ++ i ++ " of type " ++ pretty rt ++ " is applied to too many arguments"
                EQ -> retE (FunAppExpr i es) v
        MutableEntry _    -> throwSem $ "Cannot apply arguments to the mutable variable " ++ i
        ArrayEntry _ _    -> throwSem $ "Cannot apply arguments to array " ++ i
        ConstrEntry {}    -> throwSem $ "Cannot apply function arguments to constr " ++ i

semConstrAppExpr :: ConstrIdentifier -> [Expr SemanticTag] -> Parser (Expr SemanticTag)
semConstrAppExpr i es = do
    entry <- findName i
    case entry of
        ConstrEntry t psT outT -> do
            case compare (length es) (length psT) of
                LT -> throwSem $ "Constructor " ++ i ++ " is applied to too few arguments"
                GT -> throwSem $ "Constructor " ++ i ++ " is applied to too many arguments"
                EQ -> do
                    ts <- mapM getNodeType es
                    v <- freshTVar
                    unify (constTypeToSymbolType outT, v)
                    let inf = paramsToFun ts v
                    unify (constTypeToSymbolType t, inf)
                    checkConstraint v (NotAllowedFunType $ "Constructor " ++ i ++ " cannot return function type: " ++ pretty v)
                    retE (ConstrAppExpr i es) v
        _ -> throwInternalError $
            "Entry: " ++ show entry ++ " is not expected for constructor identifier key " ++ i

semArrayDim :: Identifier -> Int -> Parser (Expr SemanticTag)
semArrayDim i dim | dim < 1 = throwSem $ "Cannot compute the " ++ show dim ++ " dimension of array " ++ i
semArrayDim i dim = findName i >>= run where
    run (ArrayEntry _ dims)
      | dims < dim = throwSem $ "Cannot compute the " ++ show dim ++ " dimension of " ++ show dims ++ "-dim array " ++ i
      | otherwise = retE (ArrayDim i dim) (SymType IntType)
    run (ParamEntry t _) = do
        checkConstraint t (ArrayOfAtLeastDim dim $ "Cannot compute the dimension " ++ show dim ++ " for type " ++ pretty t)
        retE (ArrayDim i dim) (SymType IntType)
    run (FunEntry sch []) = do
        t <- inst sch
        checkConstraint t (ArrayOfAtLeastDim dim $ "Cannot compute the dimension " ++ show dim ++ " for type " ++ pretty t)
        retE (ArrayDim i dim) (SymType IntType)
    run (PatternEntry t) = do
        checkConstraint t (ArrayOfAtLeastDim dim $ "Cannot compute the dimension " ++ show dim ++ " for type " ++ pretty t)
        retE (ArrayDim i dim) (SymType IntType)
    run _ = throwSem $ "No array " ++ i ++ " in scope"

semLetIn :: LetDef SemanticTag -> Expr SemanticTag -> Parser (Expr SemanticTag)
semLetIn l e = do
    p <- getSemPosn
    closeScopeInNames
    t <- getNodeType e
    return $ LetIn l e SemTag{posn = p, typeInfo = NodeType t}

semUnOp :: UnOp -> Expr SemanticTag -> Parser (Expr SemanticTag)
semUnOp op e = do
    t <- getNodeType e
    v <- freshTVar
    case op of
        BangOp         -> unify (SymType (RefType v), t)
        PlusUnOp       -> unify (SymType IntType, t)
        MinusUnOp      -> unify (SymType IntType, t)
        PlusFloatUnOp  -> unify (SymType FloatType, t)
        MinusFloatUnOp -> unify (SymType FloatType, t)
        NotOp          -> unify (SymType BoolType, t)
    let finalT = if op == BangOp then v else t
    retE (UnOpExpr op e) finalT

semBinOp :: BinOp -> Expr SemanticTag -> Expr SemanticTag -> Parser (Expr SemanticTag)
semBinOp op d e = do
    s <- getNodeType d
    t <- getNodeType e
    outT <- freshTVar
    case op of
        PlusOp          -> unifyAll (SymType IntType) s t outT
        MinusOp         -> unifyAll (SymType IntType) s t outT
        TimesOp         -> unifyAll (SymType IntType) s t outT
        DivOp           -> unifyAll (SymType IntType) s t outT
        ModOp           -> unifyAll (SymType IntType) s t outT
        PlusFloatOp     -> unifyAll (SymType FloatType) s t outT
        MinusFloatOp    -> unifyAll (SymType FloatType) s t outT
        TimesFloatOp    -> unifyAll (SymType FloatType) s t outT
        DivFloatOp      -> unifyAll (SymType FloatType) s t outT
        ExpOp           -> unifyAll (SymType FloatType) s t outT
        AndOp           -> unifyAll (SymType BoolType) s t outT
        OrOp            -> unifyAll (SymType BoolType) s t outT
        SemicolonOp     -> unify (t, outT)
        AssignMutableOp -> do
            unify (s, SymType (RefType t))
            unify (SymType UnitType, outT)
        EqOp            -> unifyEq s t outT
        NotEqOp         -> unifyEq s t outT
        NatEqOp         -> unifyEq s t outT
        NotNatEqOp      -> unifyEq s t outT
        LTOp            -> unifyComp s t outT
        GTOp            -> unifyComp s t outT
        LEqOp           -> unifyComp s t outT
        GEqOp           -> unifyComp s t outT
    retE (BinOpExpr op d e) outT where
        unifyAll expected s' t' outT' = do
            unify (expected, s')
            unify (expected, t')
            unify (expected, outT')
        unifyEq s' t' outT' = do
            checkConstraint s' (NotAllowedFunType $ "Cannot apply operator " ++ pretty op ++ " to fun types")
            checkConstraint s' (NotAllowedArrayType $ "Cannot apply operator " ++ pretty op ++ " to array types")
            checkConstraint t' (NotAllowedFunType $ "Cannot apply operator " ++ pretty op ++ " to fun types")
            checkConstraint t' (NotAllowedArrayType $ "Cannot apply operator " ++ pretty op ++ " to array types")
            unify (s', t')
            unify (SymType BoolType, outT')
        unifyComp s' t' outT' = do
            let c = mkAllowedTypes [ConstType IntType, ConstType FloatType, ConstType CharType]
                    ("Operator " ++ pretty op ++ " can only be applied to int, float or char")
            checkConstraint s' c
            checkConstraint t' c
            unify (s', t')
            unify (SymType BoolType, outT')

semArrayAccess :: Identifier -> [Expr SemanticTag] -> Parser (Expr SemanticTag)
semArrayAccess i es = do
    entry <- findName i
    case entry of
        ArrayEntry t dims ->
            case compare (length es) dims of
                LT -> throwSem $ "Array " ++ i ++ " is applied to too few arguments"
                GT -> throwSem $ "Array " ++ i ++ " is applied to too many arguments"
                EQ -> do
                    ts <- mapM getNodeType es
                    mapM_ (\et -> unify (SymType IntType, et)) ts
                    v <- freshTVar
                    let inf = SymType (ArrayType dims v)
                    unify (t, inf)
                    retE (ArrayAccess i es) (SymType (RefType v))
        ParamEntry t _ -> do
            ts <- mapM getNodeType es
            mapM_ (\et -> unify (SymType IntType, et)) ts
            v <- freshTVar
            let inf = SymType (ArrayType (length es) v)
            unify (t, inf)
            retE (ArrayAccess i es) (SymType (RefType v))
        FunEntry s []  -> do
            ts <- mapM getNodeType es
            mapM_ (\et -> unify (SymType IntType, et)) ts
            t <- inst s
            v <- freshTVar
            let inf = SymType (ArrayType (length es) v)
            unify (t, inf)
            retE (ArrayAccess i es) (SymType (RefType v))
        PatternEntry t -> do
            ts <- mapM getNodeType es
            mapM_ (\et -> unify (SymType IntType, et)) ts
            v <- freshTVar
            let inf = SymType (ArrayType (length es) v)
            unify (t, inf)
            retE (ArrayAccess i es) (SymType (RefType v))
        _    -> throwSem $ "No array " ++ i ++ " found in scope"

semNewType :: Type SemanticTag -> Parser (Expr SemanticTag)
semNewType (Type (ArrayType {}) _) = throwSem "Cannot dynamically allocate memory for array types"
semNewType t = do
    p <- getSemPosn
    nt <- SymType . RefType <$> typeToSymbolType t
    return $ NewType t SemTag{posn = p, typeInfo = NodeType nt}

semDeleteExpr :: Expr SemanticTag -> Parser (Expr SemanticTag)
semDeleteExpr e = do
    t <- getNodeType e
    v <- freshTVar
    unify (SymType (RefType v), t)
    retE (DeleteExpr e) (SymType UnitType)

semIfThenElseExpr :: Expr SemanticTag
                  -> Expr SemanticTag
                  -> Expr SemanticTag
                  -> Parser (Expr SemanticTag)
semIfThenElseExpr c d e = do
    ct <- getNodeType c
    dt <- getNodeType d
    et <- getNodeType e
    unify (SymType BoolType, ct)
    unify (dt, et)
    retE (IfThenElseExpr c d e) dt

semIfThenExpr :: Expr SemanticTag
              -> Expr SemanticTag
              -> Parser (Expr SemanticTag)
semIfThenExpr c e = do
    ct <- getNodeType c
    et <- getNodeType e
    unify (SymType BoolType, ct)
    unify (SymType UnitType, et)
    retE (IfThenExpr c e) (SymType UnitType)

semWhileExpr :: Expr SemanticTag
             -> Expr SemanticTag
             -> Parser (Expr SemanticTag)
semWhileExpr c e = do
    ct <- getNodeType c
    et <- getNodeType e
    unify (SymType BoolType, ct)
    unify (SymType UnitType, et)
    retE (WhileExpr c e) (SymType UnitType)

semForExpr :: Identifier
           -> Expr SemanticTag
           -> Expr SemanticTag
           -> Expr SemanticTag
           -> Parser (Expr SemanticTag)
semForExpr i l u e = do
    lt <- getNodeType l
    ut <- getNodeType u
    et <- getNodeType e
    unify (SymType IntType, lt)
    unify (SymType IntType, ut)
    unify (SymType UnitType, et)
    retE (ForExpr i l u e) (SymType UnitType)

semForDownExpr :: Identifier
               -> Expr SemanticTag
               -> Expr SemanticTag
               -> Expr SemanticTag
               -> Parser (Expr SemanticTag)
semForDownExpr i u l e = do
    ut <- getNodeType u
    lt <- getNodeType l
    et <- getNodeType e
    unify (SymType IntType, ut)
    unify (SymType IntType, lt)
    unify (SymType UnitType, et)
    retE (ForDownExpr i u l e) (SymType UnitType)

semMatchExpr :: Expr SemanticTag
             -> [Clause SemanticTag]
             -> Parser (Expr SemanticTag)
semMatchExpr e cs = do
    p <- getSemPosn
    et <- getNodeType e
    checkConstraint et (AllowedUserDefinedType "Can only apply pattern matching to user defined type")
    let getPat (Match pat _ _) = pat
    let pats = map getPat cs
    patTs <- mapM getNodeType pats
    stackTrace ("all patterns must have the same type as the matched expression " ++ pretty e) $
        mapM_ (\t -> unify (et, t)) patTs
    outT <- freshTVar
    let getExp (Match _ expr _) = expr
    let patExps = map getExp cs
    patExpTs <- mapM getNodeType patExps
    stackTrace "all clauses must have the same type" $
        mapM_ (\t -> unify (outT, t)) patExpTs
    return $ MatchExpr e cs SemTag{posn = p, typeInfo = NodeType outT}

-- Semantic analysis of clauses

analyzeClause :: Clause AlexPosn -> Parser (Clause SemanticTag)
analyzeClause c@(Match pat e p) = do
    -- Hold the free vars outside the body
    outerScopeVars <- getFreeTVars
    openScopeInNames
    semP <- stackTrace ("while analyzing clause " ++ pretty c) $ analyzePattern pat
    semE <- stackTrace ("while analyzing clause " ++ pretty c) $ analyzeExpr e
    closeScopeInNames
    -- Restore the free tVars
    putFreeTVars outerScopeVars
    return $ Match semP semE (cpPosn p)

-- Semantic analysis of patterns

analyzePattern :: Pattern AlexPosn -> Parser (Pattern SemanticTag)
analyzePattern = recSemPattern indSemPat

recSemPattern :: (PatternF (Pattern SemanticTag) -> Parser (Pattern SemanticTag))
    -> Pattern AlexPosn
    -> Parser (Pattern SemanticTag)
recSemPattern f p@(Pattern pf psn) = do
    let aPattern = stackTrace ("while analyzing pattern " ++ pretty p) . recSemPattern f
    semPf <- mapM aPattern pf
    putSemPosn psn
    f semPf

retP :: PatternF (Pattern SemanticTag) -> SymbolType -> Parser (Pattern SemanticTag)
retP pat t = do
    p <- getSemPosn
    return $ Pattern pat SemTag{posn = p, typeInfo = NodeType t}

indSemPat :: PatternF (Pattern SemanticTag) -> Parser (Pattern SemanticTag)
indSemPat p@(IntConstPattern {}) = retP p (SymType IntType)
indSemPat p@(FloatConstPattern {}) = retP p (SymType FloatType)
indSemPat p@(CharConstPattern {}) = retP p (SymType CharType)
indSemPat TruePattern = retP TruePattern (SymType BoolType)
indSemPat FalsePattern = retP FalsePattern (SymType BoolType)
indSemPat (IdPattern x) = do
    v <- freshTVar
    addFreeTVars v
    insertName x (PatternEntry v)
    retP (IdPattern x) v
indSemPat (ConstrPattern i pats) = do
    entry <- findName i
    case entry of
        ConstrEntry _ argT outT ->
            case compare (length pats) (length argT) of
                LT -> throwSem $ "Constructor " ++ i ++ " is applied to too few patterns"
                GT -> throwSem $ "Constructor " ++ i ++ " is applied to too many patterns"
                EQ -> do
                    patTs <- mapM getNodeType pats
                    mapM_ unify (zipWith (\ct t -> (constTypeToSymbolType ct, t)) argT patTs)
                    mapM_ verifyParamPat pats
                    retP (ConstrPattern i pats) (constTypeToSymbolType outT)
            where verifyParamPat (Pattern (ConstrPattern _ []) _) = return ()
                  verifyParamPat (Pattern (ConstrPattern p _) tg) =
                    throwSemAtPosn ("Pattern param " ++ p ++
                        " cannot be a pattern of a constructor with parameters") (posn tg)
                  verifyParamPat _ = return ()
        _ -> throwInternalError $
            "Entry: " ++ show entry ++ " is not expected for constructor identifier key " ++ i