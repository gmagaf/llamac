module Semantics.ExprAnalysis(analyzeExpr) where

import Control.Monad.List (zipWithM_)

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST
import Common.PrintAST (Pretty(pretty))
import Common.SymbolTable
import Common.SymbolType (TypeScheme(..), SymbolType(..), ConstType(..),
                          constTypeToSymbolType,
                          funToArgs, paramsToFun)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser, getPosn, putPosn, throwSemanticError, throwInternalError, throwAtPosn, stackTrace)
import Parser.SymbolTableUtils (openScopeInNames, closeScopeInNames)
import Semantics.TypeConstraints (TypeConstraint(..), mkAllowedTypes)
import Semantics.Utils
import Semantics.Unifier (inst, unifyNode, unifyHere, checkConstraintHere, checkConstraintNode)
import Semantics.TypeAnalysis (analyzeType)
import {-# SOURCE #-} Semantics.LetAnalysis (analyzeLet)

-- Semantic analysis of expressions

analyzeExpr :: Expr AlexPosn -> Parser (Expr SemanticTag)
analyzeExpr expr =
    let tTag        = putPosn
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

-- Utils for easily creating error messages
throwSem :: String -> Parser a
throwSem s = do
    p <- getPosn
    throwAtPosn p (throwSemanticError s)

findName :: String -> Parser TableEntry
findName i = do
    p <- getPosn
    findNameAt p i

-- Util for returning the result expression by updating its node type
retE :: ExprF (Expr SemanticTag) -> SymbolType -> Parser (Expr SemanticTag)
retE ef t = do
    p <- getPosn
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
        ConstrEntry t _ _ -> do
            retE (ConstConstrExpr i) (constTypeToSymbolType t)
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
                    checkConstraintHere v (NotAllowedFunType $ "Function " ++ i ++ " cannot return function type: " ++ pretty v)
                    t <- inst ft
                    let inf = paramsToFun ts v
                    unifyHere t inf
                    retE (FunAppExpr i es) v
        ParamEntry t _ -> do
            ts <- mapM getNodeType es
            v <- freshTVar
            checkConstraintHere v (NotAllowedFunType $ "Param " ++ i ++ " cannot return function type: " ++ pretty v)
            let inf = paramsToFun ts v
            unifyHere t inf
            rt <- resolveType t
            let argTypes = funToArgs rt
            case compare (length es) (length argTypes) of
                LT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty rt ++ " is applied to too few arguments"
                GT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty rt ++ " is applied to too many arguments"
                EQ -> retE (FunAppExpr i es) v
        PatternEntry t -> do
            ts <- mapM getNodeType es
            v <- freshTVar
            checkConstraintHere v (NotAllowedFunType $ "Pattern " ++ i ++ " cannot return function type: " ++ pretty v)
            let inf = paramsToFun ts v
            unifyHere t inf
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
                    checkConstraintHere v (NotAllowedFunType $ "Constructor " ++ i ++ " cannot return function type: " ++ pretty v)
                    unifyHere (constTypeToSymbolType outT) v
                    let inf = paramsToFun ts v
                    unifyHere (constTypeToSymbolType t) inf
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
        checkConstraintHere t (ArrayOfAtLeastDim dim $ "Cannot compute the dimension " ++ show dim ++ " for type " ++ pretty t)
        retE (ArrayDim i dim) (SymType IntType)
    run (FunEntry sch []) = do
        t <- inst sch
        checkConstraintHere t (ArrayOfAtLeastDim dim $ "Cannot compute the dimension " ++ show dim ++ " for type " ++ pretty t)
        retE (ArrayDim i dim) (SymType IntType)
    run (PatternEntry t) = do
        checkConstraintHere t (ArrayOfAtLeastDim dim $ "Cannot compute the dimension " ++ show dim ++ " for type " ++ pretty t)
        retE (ArrayDim i dim) (SymType IntType)
    run _ = throwSem $ "No array " ++ i ++ " in scope"

semLetIn :: LetDef SemanticTag -> Expr SemanticTag -> Parser (Expr SemanticTag)
semLetIn l e = do
    p <- getPosn
    closeScopeInNames
    t <- getNodeType e
    return $ LetIn l e SemTag{posn = p, typeInfo = NodeType t}

semUnOp :: UnOp -> Expr SemanticTag -> Parser (Expr SemanticTag)
semUnOp op e = do
    v <- freshTVar
    case op of
        BangOp         -> unifyNode (SymType (RefType v)) e
        PlusUnOp       -> unifyNode (SymType IntType) e
        MinusUnOp      -> unifyNode (SymType IntType) e
        PlusFloatUnOp  -> unifyNode (SymType FloatType) e
        MinusFloatUnOp -> unifyNode (SymType FloatType) e
        NotOp          -> unifyNode (SymType BoolType) e
    t <- getNodeType e
    let finalT = if op == BangOp then v else t
    retE (UnOpExpr op e) finalT

semBinOp :: BinOp -> Expr SemanticTag -> Expr SemanticTag -> Parser (Expr SemanticTag)
semBinOp op s t = do
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
        SemicolonOp     -> unifyNode outT t
        AssignMutableOp -> do
            st <- getNodeType s
            tt <- getNodeType t
            unifyHere st (SymType (RefType tt))
            unifyHere (SymType UnitType) outT
        EqOp            -> unifyEq s t outT
        NotEqOp         -> unifyEq s t outT
        NatEqOp         -> unifyEq s t outT
        NotNatEqOp      -> unifyEq s t outT
        LTOp            -> unifyComp s t outT
        GTOp            -> unifyComp s t outT
        LEqOp           -> unifyComp s t outT
        GEqOp           -> unifyComp s t outT
    retE (BinOpExpr op s t) outT where
        unifyAll expected s' t' outT' = do
            unifyNode expected s'
            unifyNode expected t'
            unifyHere expected outT'
        unifyEq s' t' outT' = do
            checkConstraintNode s' (NotAllowedFunType $ "Cannot apply operator " ++ pretty op ++ " to fun types")
            checkConstraintNode s' (NotAllowedArrayType $ "Cannot apply operator " ++ pretty op ++ " to array types")
            checkConstraintNode t' (NotAllowedFunType $ "Cannot apply operator " ++ pretty op ++ " to fun types")
            checkConstraintNode t' (NotAllowedArrayType $ "Cannot apply operator " ++ pretty op ++ " to array types")
            st <- getNodeType s'
            tt <- getNodeType t'
            unifyHere st tt
            unifyHere (SymType BoolType) outT'
        unifyComp s' t' outT' = do
            let c = mkAllowedTypes [ConstType IntType, ConstType FloatType, ConstType CharType]
                    ("Operator " ++ pretty op ++ " can only be applied to int, float or char")
            checkConstraintNode s' c
            checkConstraintNode t' c
            st <- getNodeType s'
            tt <- getNodeType t'
            unifyHere st tt
            unifyHere (SymType BoolType) outT'

semArrayAccess :: Identifier -> [Expr SemanticTag] -> Parser (Expr SemanticTag)
semArrayAccess i es = do
    entry <- findName i
    case entry of
        ArrayEntry t dims ->
            case compare (length es) dims of
                LT -> throwSem $ "Array " ++ i ++ " is applied to too few arguments"
                GT -> throwSem $ "Array " ++ i ++ " is applied to too many arguments"
                EQ -> do
                    mapM_ (unifyNode (SymType IntType)) es
                    v <- freshTVar
                    let inf = SymType (ArrayType dims v)
                    unifyHere t inf
                    retE (ArrayAccess i es) (SymType (RefType v))
        ParamEntry t _ -> do
            mapM_ (unifyNode (SymType IntType)) es
            v <- freshTVar
            let inf = SymType (ArrayType (length es) v)
            unifyHere t inf
            retE (ArrayAccess i es) (SymType (RefType v))
        FunEntry s []  -> do
            mapM_ (unifyNode (SymType IntType)) es
            t <- inst s
            v <- freshTVar
            let inf = SymType (ArrayType (length es) v)
            unifyHere t inf
            retE (ArrayAccess i es) (SymType (RefType v))
        PatternEntry t -> do
            mapM_ (unifyNode (SymType IntType)) es
            v <- freshTVar
            let inf = SymType (ArrayType (length es) v)
            unifyHere t inf
            retE (ArrayAccess i es) (SymType (RefType v))
        _    -> throwSem $ "No array " ++ i ++ " found in scope"

semNewType :: Type SemanticTag -> Parser (Expr SemanticTag)
semNewType (Type (ArrayType {}) _) = throwSem "Cannot dynamically allocate memory for array types"
semNewType t = do
    p <- getPosn
    nt <- SymType . RefType <$> typeToSymbolType t
    return $ NewType t SemTag{posn = p, typeInfo = NodeType nt}

semDeleteExpr :: Expr SemanticTag -> Parser (Expr SemanticTag)
semDeleteExpr e = do
    v <- freshTVar
    unifyNode (SymType (RefType v)) e
    retE (DeleteExpr e) (SymType UnitType)

semIfThenElseExpr :: Expr SemanticTag
                  -> Expr SemanticTag
                  -> Expr SemanticTag
                  -> Parser (Expr SemanticTag)
semIfThenElseExpr c d e = do
    dt <- getNodeType d
    et <- getNodeType e
    unifyNode (SymType BoolType) c
    unifyHere dt et
    retE (IfThenElseExpr c d e) dt

semIfThenExpr :: Expr SemanticTag
              -> Expr SemanticTag
              -> Parser (Expr SemanticTag)
semIfThenExpr c e = do
    unifyNode (SymType BoolType) c
    unifyNode (SymType UnitType) e
    retE (IfThenExpr c e) (SymType UnitType)

semWhileExpr :: Expr SemanticTag
             -> Expr SemanticTag
             -> Parser (Expr SemanticTag)
semWhileExpr c e = do
    unifyNode (SymType BoolType) c
    unifyNode (SymType UnitType) e
    retE (WhileExpr c e) (SymType UnitType)

semForExpr :: Identifier
           -> Expr SemanticTag
           -> Expr SemanticTag
           -> Expr SemanticTag
           -> Parser (Expr SemanticTag)
semForExpr i l u e = do
    unifyNode (SymType IntType) l
    unifyNode (SymType IntType) u
    unifyNode (SymType UnitType) e
    retE (ForExpr i l u e) (SymType UnitType)

semForDownExpr :: Identifier
               -> Expr SemanticTag
               -> Expr SemanticTag
               -> Expr SemanticTag
               -> Parser (Expr SemanticTag)
semForDownExpr i u l e = do
    unifyNode (SymType IntType) u
    unifyNode (SymType IntType) l
    unifyNode (SymType UnitType) e
    retE (ForDownExpr i u l e) (SymType UnitType)

semMatchExpr :: Expr SemanticTag
             -> [Clause SemanticTag]
             -> Parser (Expr SemanticTag)
semMatchExpr e cs = do
    p <- getPosn
    et <- getNodeType e
    checkConstraintNode e (AllowedUserDefinedType "Can only apply pattern matching to user defined type")
    let getPat (Match pat _ _) = pat
    let pats = map getPat cs
    stackTrace ("all patterns must have the same type as the matched expression " ++ pretty e) $
        mapM_ (unifyNode et) pats
    outT <- freshTVar
    let getExp (Match _ expr _) = expr
    let patExps = map getExp cs
    stackTrace "all clauses must have the same type" $
        mapM_ (unifyNode outT) patExps
    return $ MatchExpr e cs SemTag{posn = p, typeInfo = NodeType outT}

-- Semantic analysis of clauses

analyzeClause :: Clause AlexPosn -> Parser (Clause SemanticTag)
analyzeClause c@(Match pat e p) = do
    -- Hold the free vars outside the body
    outerScopeVars <- getFreeTVars
    openScopeInNames
    semP <- stackTrace ("while analyzing clause " ++ pretty c) $ analyzePattern pat
    checkConstraintNode semP (AllowedUserDefinedType "Can only apply pattern matching to user defined type")
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
    putPosn psn
    f semPf

retP :: PatternF (Pattern SemanticTag) -> SymbolType -> Parser (Pattern SemanticTag)
retP pat t = do
    p <- getPosn
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
                    zipWithM_ (unifyNode . constTypeToSymbolType) argT pats
                    retP (ConstrPattern i pats) (constTypeToSymbolType outT)
        _ -> throwInternalError $
            "Entry: " ++ show entry ++ " is not expected for constructor identifier key " ++ i