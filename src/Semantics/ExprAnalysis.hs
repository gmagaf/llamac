module Semantics.ExprAnalysis(analyzeLet, analyzeExpr) where

import Control.Monad (when)
import qualified Data.Set as S
import qualified Data.Bifunctor as B

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST
import Common.PrintAST
import Common.SymbolTable
import Common.SymbolType
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser, stackTrace, throwInternalError)
import Semantics.SemanticState (TypeConstraint(..))
import Semantics.Utils
import Semantics.Unifier (checkConstraint, unify)
import Semantics.TypeAnalysis (analyzeType)

import Debug.Trace (trace)

-- Semantic analysis of definitions

{-
    Analyze let statements
    It will analyze all defs in the current typespace
    Generalize all function types
    Open a new namespace and write them in
-}
analyzeLet :: LetDef AlexPosn -> Parser (LetDef SemanticTag)
analyzeLet (Let defs p) = do
    -- First get all the free variables from the outer scope
    outerScopeVars <- freeVarsInScope
    -- Analyze the signatures of the definitions
    sigAnalysisResults <- mapM analyzeDefSig defs
    -- Analyze the body of the definitions
    semDefEntries <- mapM analyzeDefBody sigAnalysisResults
    -- Update the tvars in freeVars
    freeVars <- resolveFreeVars outerScopeVars
    -- Generalize the results
    finalRes <- mapM (genResult freeVars) semDefEntries
    -- Open the scope
    openScopeInNames
    -- Insert the definitions in the scope
    mapM_ (uncurry insertName . B.first ide) finalRes
    -- Second analysis of the definitions to get the most general signatures
    let finalSemDefs = map fst finalRes
    return $ Let finalSemDefs (cpPosn p)
analyzeLet (LetRec defs p) = do
    -- First get all the free variables from the outer scope
    outerScopeVars <- freeVarsInScope
    -- Analyze the signatures of the definitions
    sigAnalysisResults <- mapM analyzeDefSig defs
    -- Add free vars of mutables in outerScopeVars
    let mutVar (UntypedMutSig st _ _) acc   = tvarsInType st ++ acc
        mutVar (UntypedArrSig st _ _ _) acc = tvarsInType st ++ acc
        mutVar _ acc                        = acc
    let scopeVars = foldr mutVar outerScopeVars sigAnalysisResults
    -- Open the scope
    openScopeInNames
    -- Insert the definitions in the scope with placeholder type vars
    mapM_ (uncurry insertName . entryPair) sigAnalysisResults
    -- Analyze the body of the definitions
    semDefEntries <- mapM analyzeDefBody sigAnalysisResults
    -- Update the tvars in symbol table and in freeVars
    updatedEntries <- mapM (findName . ide . fst) semDefEntries
    freeVars <- resolveFreeVars scopeVars
    -- Second analysis of the definitions to get the most general signatures
    let semDefs = map fst semDefEntries
    let genInput = zip3 semDefs defs updatedEntries
    res <- mapM secondAnalysis genInput
    -- Generalize the results
    finalRes <- mapM (genResult freeVars) res
    let finalSemDefs = map fst finalRes
    -- Final update in scope
    mapM_ (uncurry updateName . B.first ide) finalRes
    return $ Let finalSemDefs (cpPosn p)

{-
    In Second analysis we only analyze Untyped fun definitions
    in order to get the most general unifier principal type
-}
secondAnalysis :: (Def SemanticTag, Def AlexPosn, TableEntry) -> Parser (Def SemanticTag, TableEntry)
secondAnalysis (_, d@(FunDef {}), FunEntry _ _) = do
    sigRes <- analyzeDefSig d
    analyzeDefBody sigRes
secondAnalysis (d, _, e) = return (d, e)

{-
    We generalize second analysis results to
    get polymorphic functions
-}
genResult :: S.Set Int -> (Def SemanticTag, TableEntry) -> Parser (Def SemanticTag, TableEntry)
genResult freeVars (FunDef i ps e tg, FunEntry (MonoType t) params) = do
    let scheme = gen freeVars t
    return (FunDef i ps e tg{typeInfo = DefType scheme}, FunEntry scheme params)
genResult _ pair = return pair

-- Util definitions for sig analysis
type KeyEntryPair = (Identifier, TableEntry)

data SigAnalysisRes =
    TypedMutSig (Type SemanticTag) KeyEntryPair AlexPosn
  | TypedArrSig (Type SemanticTag) [Expr AlexPosn] KeyEntryPair AlexPosn
  | TypedFunSig (Type SemanticTag) [Param SemanticTag] (Expr AlexPosn) KeyEntryPair AlexPosn
  | UntypedMutSig SymbolType KeyEntryPair AlexPosn
  | UntypedArrSig SymbolType [Expr AlexPosn] KeyEntryPair AlexPosn
  | UnTypedFunSig SymbolType [Param SemanticTag] (Expr AlexPosn) KeyEntryPair AlexPosn

entryPair :: SigAnalysisRes -> KeyEntryPair
entryPair (TypedMutSig _ pair _)       = pair
entryPair (TypedArrSig _ _ pair _)     = pair
entryPair (TypedFunSig _ _ _ pair _)   = pair
entryPair (UntypedMutSig _ pair _)     = pair
entryPair (UntypedArrSig _ _ pair _)   = pair
entryPair (UnTypedFunSig _ _ _ pair _) = pair

{-
    Analysis of the signature of a definition
-}
analyzeDefSig :: Def AlexPosn -> Parser SigAnalysisRes
analyzeDefSig (VarDef x p) = do
    tv <- freshTVar
    let varType = SymType . RefType $ tv
    return $ UntypedMutSig varType (x, MutableEntry varType) p
analyzeDefSig (VarDefTyped x t p) = do
    semT <- stackTrace ("while analyzing mut var " ++ x) $ analyzeType t
    let varType = SymType . RefType $ typeToSymbolType semT
    return $ TypedMutSig semT (x, MutableEntry varType) p
analyzeDefSig (ArrayDef i es p) = do
    let dims = length es
    tv <- freshTVar
    let arrayType = SymType . ArrayType dims $ tv
    return $ UntypedArrSig arrayType es (i, ArrayEntry arrayType dims) p
analyzeDefSig (ArrayDefTyped i es t p) = do
    let dims = length es
    semT <- stackTrace ("while analyzing array " ++ i) $ analyzeType t
    let arrayType = SymType . ArrayType dims $ typeToSymbolType semT
    return $ TypedArrSig semT es (i, ArrayEntry arrayType dims) p
analyzeDefSig (FunDef i ps e p) = do
    let paramNames = map ide ps
    when (hasDuplicates paramNames) $
        throwSemAtPosn ("Fun " ++ i ++ " cannot have duplicate params") p
    -- Open scope for params names and their types
    openScopeInNames
    -- Analyze params in the current scope
    semPs <- mapM (stackTrace ("while analyzing the params of " ++ i) . analyzeParam i) ps
    putSemPosn p
    paramTypes <- mapM getNodeType semPs
    -- Fresh outV is the output type of the function
    outV <- freshTVar
    let fType = paramsToFunType paramTypes outV
    -- Close scope
    closeScopeInNames
    -- fScheme <- gen fType
    return $ UnTypedFunSig fType semPs e (i, FunEntry (MonoType fType) paramNames) p
analyzeDefSig (FunDefTyped i ps t e p) = do
    let paramNames = map ide ps
    -- First we make sure that there are no duplicates in params
    when (hasDuplicates paramNames) $
        throwSemAtPosn ("Fun " ++ i ++ " cannot have duplicate params") p
    -- We analyze the overall type of the function
    semT <- stackTrace ("while analyzing fun " ++ i) $ analyzeType t
    -- Open scope for params names and their types
    openScopeInNames
    -- Analyze params and body in the current scope
    semPs <- mapM (stackTrace ("while analyzing the params of " ++ i) . analyzeParam i) ps
    eT <- freshTVar
    -- Collect the results: the new param types, the expr type and unify tv with the result fun type
    putSemPosn p
    paramTypes <- mapM getNodeType semPs
    let fType = paramsToFunType paramTypes eT
    unify (typeToSymbolType semT, fType)
    -- Close scope
    closeScopeInNames
    let fScheme = MonoType $ typeToSymbolType semT
    return $ TypedFunSig semT semPs e (i, FunEntry fScheme paramNames) p

{-
    Analyzes params by writting them to
    the symbol table
    - Checks the explicit types of params
-}
analyzeParam :: Identifier -> Param AlexPosn -> Parser (Param SemanticTag)
analyzeParam fun (TypedParam param t p) = do
    semT <- stackTrace ("while analyzing param " ++ param) $ analyzeType t
    putSemPosn p
    let st = typeToSymbolType t
    insertName param (ParamEntry st fun)
    return $ TypedParam param semT SemTag{posn = p, typeInfo = NodeType st}
analyzeParam fun (Param param p) = do
    putSemPosn p
    vt <- freshTVar
    insertName param (ParamEntry vt fun)
    return $ Param param SemTag{posn = p, typeInfo = NodeType vt}

{-
    Analysis of the body of a definition
-}
analyzeDefBody :: SigAnalysisRes -> Parser (Def SemanticTag, TableEntry)
analyzeDefBody (UntypedMutSig st (i, entry) p) =
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType st}
    in return (VarDef i tg, entry)
analyzeDefBody (TypedMutSig semT (i, entry) p) =
    let tg = SemTag{posn = p, typeInfo = DefType . MonoType . typeToSymbolType $ semT}
    in return (VarDefTyped i semT tg, entry)
analyzeDefBody (UntypedArrSig st es (i, entry) p) = do
    semEs <- mapM (stackTrace ("while analyzing the dimensions of array " ++ i) . analyzeExpr) es
    typesEs <- mapM getNodeType semEs
    putSemPosn p
    mapM_ (unify . (,) (SymType IntType)) typesEs
    rSemEs <- mapM resolveNodeType semEs
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType st}
    return (ArrayDef i rSemEs tg, entry)
analyzeDefBody (TypedArrSig semT es (i, entry) p) = do
    semEs <- mapM (stackTrace ("while analyzing the dimensions of array " ++ i) . analyzeExpr) es
    typesEs <- mapM getNodeType semEs
    putSemPosn p
    mapM_ (unify . (,) (SymType IntType)) typesEs
    rSemEs <- mapM resolveNodeType semEs
    let tg = SemTag{posn = p, typeInfo = DefType . MonoType . typeToSymbolType $ semT}
    return (ArrayDefTyped i rSemEs semT tg, entry)
analyzeDefBody (UnTypedFunSig st semPs e (i, _) p) = do
    -- Open scope for params names and their types
    openScopeInNames
    -- Insert params in the current scope
    let insertParam param = do
          t <- getNodeType param
          insertName (ide param) (ParamEntry t i)
          return t
    paramTypes <- mapM insertParam semPs
    semE <- stackTrace ("while analyzing the body of " ++ i) (analyzeExpr e)
    -- Collect the results: the new param types, the expr type and unify tv with the result fun type
    putSemPosn p
    eT <- getNodeType semE
    let fType = paramsToFunType paramTypes eT
    unify (st, fType)
    rSemPs <- mapM resolveNodeType semPs
    rSemE <- resolveNodeType semE
    reT <- getNodeType rSemE
    case reT of
        SymType (FunType _ _) -> throwSem ("Function " ++ i ++ " cannot return function type: " ++ pretty reT)
        _ -> return ()
    rst <- resolveType st
    -- Close scope
    closeScopeInNames
    let fScheme = MonoType rst
    let tg = SemTag{posn = p, typeInfo = DefType fScheme}
    return (FunDef i rSemPs rSemE tg, FunEntry fScheme (map ide rSemPs))
analyzeDefBody (TypedFunSig semT semPs e (i, _) p) = do
    -- Open scope for params names and their types
    openScopeInNames
    -- Insert params in the current scope
    let insertParam param = do
          t <- getNodeType param
          insertName (ide param) (ParamEntry t i)
          return t
    paramTypes <- mapM insertParam semPs
    semE <- stackTrace ("while analyzing the body of " ++ i) (analyzeExpr e)
    -- Collect the results: the new param types, the expr type and unify tv with the result fun type
    putSemPosn p
    eT <- getNodeType semE
    let fType = paramsToFunType paramTypes eT
    let st = typeToSymbolType semT
    unify (st, fType)
    rSemPs <- mapM resolveNodeType semPs
    rSemE <- resolveNodeType semE
    reT <- getNodeType rSemE
    case reT of
        SymType (FunType _ _) -> throwSem ("Function " ++ i ++ " cannot return function type: " ++ pretty reT)
        _ -> return ()
    -- Close scope
    closeScopeInNames
    let fScheme = MonoType st
    let tg = SemTag{posn = p, typeInfo = DefType fScheme}
    return (FunDefTyped i rSemPs semT rSemE tg, FunEntry fScheme (map ide rSemPs))

-- Semantic analysis of expressions

analyzeExpr :: Expr AlexPosn -> Parser (Expr SemanticTag)
analyzeExpr = recSemExpr indSemExpr

-- Inductively analyze expressions using mapM for expression functor
recSemExpr :: (ExprF SemanticTag (Expr SemanticTag) -> Parser (Expr SemanticTag))
         -> Expr AlexPosn
         -> Parser (Expr SemanticTag)
recSemExpr g e@(Expr ef p) = do
    let alet = stackTrace ("while analyzing expr " ++ pretty e) . analyzeLet
    let atype = stackTrace ("while analyzing expr " ++ pretty e) . analyzeType
    let aclause = stackTrace ("while analyzing expr " ++ pretty e) . analyzeClause
    let aexpr = stackTrace ("while analyzing expr " ++ pretty e) . recSemExpr g
    let afor i fore = do
            openScopeInNames
            insertName i (FunEntry (MonoType . SymType $ IntType) [])
            e' <- aexpr fore
            closeScopeInNames
            return e'
    semEf <- mapMExprF alet atype aclause afor aexpr ef
    putSemPosn p
    g semEf

-- Util for returning the result expression by updating its node type
retE :: ExprF SemanticTag (Expr SemanticTag) -> SymbolType -> Parser (Expr SemanticTag)
retE ef t = do
    p <- getSemPosn
    return $ Expr ef SemTag{posn = p, typeInfo = NodeType t}

indSemExpr :: ExprF SemanticTag (Expr SemanticTag) -> Parser (Expr SemanticTag)
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
indSemExpr (LetIn l e)            = semLetIn l e
indSemExpr (UnOpExpr op e)        = semUnOp op e
indSemExpr (BinOpExpr op d e)     = semBinOp op d e
indSemExpr (ArrayAccess i es)     = semArrayAccess i es
indSemExpr (NewType t)            = semNewType t
indSemExpr (DeleteExpr e)         = semDeleteExpr e
indSemExpr (BeginExpr e)          = getNodeType e >>= retE (BeginExpr e)
indSemExpr (IfThenElseExpr c d e) = semIfThenElseExpr c d e
indSemExpr (IfThenExpr c e)       = semIfThenExpr c e
indSemExpr (WhileExpr c e)        = semWhileExpr c e
indSemExpr (ForExpr i l u e)      = semForExpr i l u e
indSemExpr (ForDownExpr i u l e)  = semForDownExpr i u l e
indSemExpr (MatchExpr e cs)       = semMatchExpr e cs

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
        ConstrEntry t _ _ -> retE (ConstExpr i) (constTypeToSymbolType t)
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
                    let inf = paramsToFunType ts v
                    unify (t, inf)
                    res <- mapM resolveNodeType es
                    rv <- resolveType v
                    case rv of
                        SymType (FunType _ _)
                          -> throwSem ("Function " ++ i ++ " cannot return result of function type: " ++ pretty rv)
                        _ -> return ()
                    retE (FunAppExpr i res) rv
        ParamEntry t _ -> do
            ts <- mapM getNodeType es
            v <- freshTVar
            let inf = paramsToFunType ts v
            unify (t, inf)
            res <- mapM resolveNodeType es
            rv <- resolveType v
            rt <- resolveType t
            let argTypes = funTypeToArgTypes rt
            case compare (length es) (length argTypes) of
                LT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty t ++ " is applied to too few arguments"
                GT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty t ++ " is applied to too many arguments"
                EQ -> retE (FunAppExpr i res) rv
        PatternEntry t -> do
            ts <- mapM getNodeType es
            v <- freshTVar
            let inf = paramsToFunType ts v
            unify (t, inf)
            res <- mapM resolveNodeType es
            rv <- resolveType v
            rt <- resolveType t
            let argTypes = funTypeToArgTypes rt
            case compare (length es) (length argTypes) of
                LT -> throwSem $ "Pattern " ++ i ++ " of type " ++ pretty t ++ " is applied to too few arguments"
                GT -> throwSem $ "Pattern " ++ i ++ " of type " ++ pretty t ++ " is applied to too many arguments"
                EQ -> retE (FunAppExpr i res) rv
        MutableEntry _    -> throwSem $ "Cannot apply arguments to the mutable variable " ++ i
        ArrayEntry _ _    -> throwSem $ "Cannot apply arguments to array " ++ i
        ConstrEntry {}    -> throwSem $ "Cannot apply function arguments to constr " ++ i

semConstrAppExpr :: ConstrIdentifier -> [Expr SemanticTag] -> Parser (Expr SemanticTag)
semConstrAppExpr i es = do
    entry <- findName i
    case entry of
        ConstrEntry t psT _ ->
            case compare (length es) (length psT) of
                LT -> throwSem $ "Constructor " ++ i ++ " is applied to too few arguments"
                GT -> throwSem $ "Constructor " ++ i ++ " is applied to too many arguments"
                EQ -> do
                    ts <- mapM getNodeType es
                    v <- freshTVar
                    let inf = paramsToFunType ts v
                    unify (constTypeToSymbolType t, inf)
                    res <- mapM resolveNodeType es
                    rv <- resolveType v
                    case rv of
                        SymType (FunType _ _)
                          -> throwSem ("Constructor " ++ i ++ " cannot return result of function type: " ++ pretty rv)
                        _ -> return ()
                    retE (ConstrAppExpr i res) rv
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
    run (PatternEntry t) = do
        checkConstraint t (ArrayOfAtLeastDim dim $ "Cannot compute the dimension " ++ show dim ++ " for type " ++ pretty t)
        retE (ArrayDim i dim) (SymType IntType)
    run _ = throwSem $ "No array " ++ i ++ " in scope"

semLetIn :: LetDef SemanticTag -> Expr SemanticTag -> Parser (Expr SemanticTag)
semLetIn l e = do
    closeScopeInNames
    t <- getNodeType e
    retE (LetIn l e) t

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
    rE <- resolveNodeType e
    rt <- resolveType t
    let finalT = if op == BangOp then v else rt
    retE (UnOpExpr op rE) finalT

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
    rd <- resolveNodeType d
    re <- resolveNodeType e
    retE (BinOpExpr op rd re) outT where
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
            let c = AllowedTypes [ConstType IntType, ConstType FloatType, ConstType CharType]
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
                    res <- mapM resolveNodeType es
                    rv <- resolveType v
                    retE (ArrayAccess i res) (SymType (RefType rv))
        ParamEntry t _ -> do
            ts <- mapM getNodeType es
            mapM_ (\et -> unify (SymType IntType, et)) ts
            v <- freshTVar
            let inf = SymType (ArrayType (length es) v)
            unify (t, inf)
            res <- mapM resolveNodeType es
            rv <- resolveType v
            retE (ArrayAccess i res) (SymType (RefType rv))
        PatternEntry t -> do
            ts <- mapM getNodeType es
            mapM_ (\et -> unify (SymType IntType, et)) ts
            v <- freshTVar
            let inf = SymType (ArrayType (length es) v)
            unify (t, inf)
            res <- mapM resolveNodeType es
            rv <- resolveType v
            retE (ArrayAccess i res) (SymType (RefType rv))
        _    -> throwSem $ "No array " ++ i ++ " found in scope"

semNewType :: Type SemanticTag -> Parser (Expr SemanticTag)
semNewType (Type (ArrayType {}) _) = throwSem "Cannot dynamically allocate memory for array types"
semNewType t = do
    retE (NewType t) (SymType . RefType $ typeToSymbolType t)

semDeleteExpr :: Expr SemanticTag -> Parser (Expr SemanticTag)
semDeleteExpr e = do
    t <- getNodeType e
    v <- freshTVar
    unify (SymType (RefType v), t)
    re <- resolveNodeType e
    retE (DeleteExpr re) (SymType UnitType)

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
    rc <- resolveNodeType c
    rd <- resolveNodeType d
    re <- resolveNodeType e
    outT <- resolveType dt
    retE (IfThenElseExpr rc rd re) outT

semIfThenExpr :: Expr SemanticTag
              -> Expr SemanticTag
              -> Parser (Expr SemanticTag)
semIfThenExpr c e = do
    ct <- getNodeType c
    et <- getNodeType e
    unify (SymType BoolType, ct)
    unify (SymType UnitType, et)
    rc <- resolveNodeType c
    re <- resolveNodeType e
    retE (IfThenExpr rc re) (SymType UnitType)

semWhileExpr :: Expr SemanticTag
             -> Expr SemanticTag
             -> Parser (Expr SemanticTag)
semWhileExpr c e = do
    ct <- getNodeType c
    et <- getNodeType e
    unify (SymType BoolType, ct)
    unify (SymType UnitType, et)
    rc <- resolveNodeType c
    re <- resolveNodeType e
    retE (WhileExpr rc re) (SymType UnitType)

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
    rl <- resolveNodeType l
    ru <- resolveNodeType u
    re <- resolveNodeType e
    retE (ForExpr i rl ru re) (SymType UnitType)

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
    ru <- resolveNodeType u
    rl <- resolveNodeType l
    re <- resolveNodeType e
    retE (ForExpr i ru rl re) (SymType UnitType)

semMatchExpr :: Expr SemanticTag
             -> [Clause SemanticTag]
             -> Parser (Expr SemanticTag)
semMatchExpr e cs = do
    et <- getNodeType e
    checkConstraint et (AllowedUserDefinedType "Can only apply pattern matching to user defined type")
    let getPat (Match pat _ _) = pat
    let pats = map getPat cs
    patTs <- mapM getNodeType pats
    mapM_ (\t -> unify (et, t)) patTs
    outT <- freshTVar
    let getExp (Match _ expr _) = expr
    let patExps = map getExp cs
    patExpTs <- mapM getNodeType patExps
    mapM_ (\t -> unify (outT, t)) patExpTs
    re <- resolveNodeType e
    rcs <- mapM resClause cs
    routT <- resolveType outT
    retE (MatchExpr re rcs) routT where
        resClause (Match pat expr t) = do
            rPat <- resolveNodeType pat
            rExp <- resolveNodeType expr
            return (Match rPat rExp t)

-- Semantic analysis of clauses

analyzeClause :: Clause AlexPosn -> Parser (Clause SemanticTag)
analyzeClause c@(Match pat e p) = do
    openScopeInNames
    semP <- stackTrace ("while analyzing clause " ++ pretty c) $ analyzePattern pat
    semE <- stackTrace ("while analyzing clause " ++ pretty c) $ analyzeExpr e
    closeScopeInNames
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
                    rpats <- mapM resolveNodeType pats
                    mapM_ verifyParamPat rpats
                    retP (ConstrPattern i rpats) (constTypeToSymbolType outT)
            where verifyParamPat (Pattern (ConstrPattern _ []) _) = return ()
                  verifyParamPat (Pattern (ConstrPattern p _) tg) =
                    throwSemAtPosn ("Pattern param " ++ p ++
                        " cannot be a pattern of a constructor with parameters") (posn tg)
                  verifyParamPat _ = return ()
        _ -> throwInternalError $
            "Entry: " ++ show entry ++ " is not expected for constructor identifier key " ++ i