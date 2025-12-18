module Semantics.ExprAnalysis(analyzeLet, analyzeExpr) where

import Control.Monad (when, zipWithM)
import qualified Data.Bifunctor as B

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST
import Common.PrintAST
import Common.SymbolTable
import Common.SymbolType (TypeScheme(..), SymbolType(..), ConstType(..),
                          constTypeToSymbolType, typeTo,
                          funToArgs, stCoAlg, paramsToFun)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser, stackTrace, throwInternalError)
import Parser.SymbolTableUtils (openScopeInNames, closeScopeInNames, insertNameP)
import Semantics.TypeConstraints (TypeConstraint(..), mkAllowedTypes)
import Semantics.Utils
import Semantics.Unifier (checkConstraint, unify)
import Semantics.TypeAnalysis (analyzeType)

-- Semantic analysis of definitions

-- function alias
insertName :: String -> TableEntry -> Parser ()
insertName = insertNameP

{-
    Analyze let statements
    It will analyze all defs in the current typespace
    Generalize all function types
    Open a new namespace and write them in
-}
analyzeLet :: LetDef AlexPosn -> Parser (LetDef SemanticTag)
analyzeLet (Let defs p) = do
    -- Analyze each definition separately
    semDefs <- mapM semD defs
    -- Open the scope
    openScopeInNames
    -- Insert the definitions in the scope
    mapM_ (uncurry insertName . B.first ide . fst) semDefs
    mapM_ (addMutVarToFree . snd) semDefs
    return $ Let (map (fst . fst) semDefs) (cpPosn p) where
        semD def =  do
            -- Analyze the signatures of the definition
            sigAnalysisResult <- analyzeDefSig def
            -- Analyze the body of the definition
            semDefEntry <- analyzeDefBody sigAnalysisResult
            -- Generalize the result
            pair <- genResult semDefEntry
            return (pair, sigAnalysisResult)
analyzeLet (LetRec defs p) = do
    -- Analyze the signatures of the definitions
    sigAnalysisResults <- mapM analyzeDefSig defs
    -- Open the scope
    openScopeInNames
    -- Insert the definitions in the scope with placeholder type vars
    mapM_ (uncurry insertName . entryPair) sigAnalysisResults
    mapM_ addMutVarToFree sigAnalysisResults
    -- Analyze the body of the definitions
    semDefEntries <- mapM analyzeDefBody sigAnalysisResults
    -- Second analysis of the definitions to get the most general signatures
    res <- zipWithM secondAnalysis defs semDefEntries
    -- Generalize the results
    finalRes <- mapM genResult res
    -- Final update in scope
    mapM_ (uncurry updateName . B.first ide) finalRes
    let finalSemDefs = map fst finalRes
    return $ LetRec finalSemDefs (cpPosn p)

{-
    In Second analysis we only analyze Untyped fun definitions
    in order to get the most general unifier principal type
-}
secondAnalysis :: Def AlexPosn -> (Def SemanticTag, TableEntry) -> Parser (Def SemanticTag, TableEntry)
secondAnalysis d@(FunDef {}) (_, FunEntry _ _) = analyzeDefSig d >>= analyzeDefBody
secondAnalysis _ pair = return pair

{-
    We generalize second analysis results to
    get polymorphic functions
-}
genResult :: (Def SemanticTag, TableEntry) -> Parser (Def SemanticTag, TableEntry)
genResult (FunDef i ps Nothing e tg, FunEntry (MonoType t) params) = do
    -- Update all the free variables from the outer scope
    resolveFreeVars
    -- Resolve the type to generalize
    rt <- resolveType t
    -- Generalize type
    scheme <- gen rt
    return (FunDef i ps Nothing e tg{typeInfo = DefType scheme}, FunEntry scheme params)
genResult pair = return pair

-- Util definitions for sig analysis
type KeyEntryPair = (Identifier, TableEntry)

data SigAnalyzedAST =
   Mut
 | Arr [Expr AlexPosn]
 | Fun [Param SemanticTag] (Expr AlexPosn)

data SigAnalyzedType =
    Typed (Type SemanticTag)
  | Untyped SymbolType

data SigAnalysisRes = SigAnalysisRes SigAnalyzedType SigAnalyzedAST AlexPosn KeyEntryPair

entryPair :: SigAnalysisRes -> KeyEntryPair
entryPair (SigAnalysisRes _ _ _ pair) = pair

addMutVarToFree :: SigAnalysisRes -> Parser ()
addMutVarToFree (SigAnalysisRes (Untyped t) Mut _ _) = addFreeTVars t
addMutVarToFree (SigAnalysisRes (Untyped t) (Arr _) _ _) = addFreeTVars t
addMutVarToFree _ = return ()

typeToSymbolType :: Type b -> SymbolType
typeToSymbolType = typeTo SymType

{-
    Analysis of the signature of a definition
-}
analyzeDefSig :: Def AlexPosn -> Parser SigAnalysisRes
analyzeDefSig (VarDef x Nothing p) = do
    tv <- freshTVar
    checkConstraint tv (NotPolymorphicVar $ "Cannot abstract on mutable var " ++ pretty tv)
    let varType = SymType . RefType $ tv
    return $ SigAnalysisRes (Untyped varType) Mut p (x, MutableEntry varType)
analyzeDefSig (VarDef x (Just t) p) = do
    semT <- stackTrace ("while analyzing mut var " ++ x) $ analyzeType t
    let varType = SymType . RefType $ typeToSymbolType semT
    return $ SigAnalysisRes (Typed semT) Mut p (x, MutableEntry varType)
analyzeDefSig (ArrayDef i es Nothing p) = do
    let dims = length es
    tv <- freshTVar
    checkConstraint tv (NotPolymorphicVar $ "Cannot abstract on array var " ++ pretty tv)
    let arrayType = SymType . ArrayType dims $ tv
    return $ SigAnalysisRes (Untyped arrayType) (Arr es) p (i, ArrayEntry arrayType dims)
analyzeDefSig (ArrayDef i es (Just t) p) = do
    let dims = length es
    semT <- stackTrace ("while analyzing array " ++ i) $ analyzeType t
    let arrayType = SymType . ArrayType dims $ typeToSymbolType semT
    return $ SigAnalysisRes (Typed semT) (Arr es) p (i, ArrayEntry arrayType dims)
analyzeDefSig (FunDef i ps Nothing e p) = do
    let paramNames = map ide ps
    when (hasDuplicates paramNames) $
        throwSemAtPosn ("Fun " ++ i ++ " cannot have duplicate params") p
    -- Analyze params in the current scope
    semPs <- mapM (stackTrace ("while analyzing the params of " ++ i) . analyzeParam) ps
    putSemPosn p
    paramTypes <- mapM getNodeType semPs
    -- Fresh outV is the output type of the function
    outV <- freshTVar
    let fType = paramsToFun SymType paramTypes outV
    return $ SigAnalysisRes (Untyped fType) (Fun semPs e) p (i, FunEntry (MonoType fType) paramNames)
analyzeDefSig (FunDef i ps (Just t) e p) = do
    let paramNames = map ide ps
    -- First we make sure that there are no duplicates in params
    when (hasDuplicates paramNames) $
        throwSemAtPosn ("Fun " ++ i ++ " cannot have duplicate params") p
    -- We analyze the overall type of the function
    semT <- stackTrace ("while analyzing fun " ++ i) $ analyzeType t
    -- Analyze params and body in the current scope
    semPs <- mapM (stackTrace ("while analyzing the params of " ++ i) . analyzeParam) ps
    eT <- freshTVar
    -- Collect the results: the new param types, the expr type and unify tv with the result fun type
    putSemPosn p
    paramTypes <- mapM getNodeType semPs
    let fType = paramsToFun SymType paramTypes eT
    unify (typeToSymbolType semT, fType)
    let fScheme = MonoType $ typeToSymbolType semT
    return $ SigAnalysisRes (Typed semT) (Fun semPs e) p (i, FunEntry fScheme paramNames)

{-
    Analyzes params by writting them to
    the symbol table
    - Checks the explicit types of params
-}
analyzeParam :: Param AlexPosn -> Parser (Param SemanticTag)
analyzeParam (TypedParam param t p) = do
    semT <- stackTrace ("while analyzing param " ++ param) $ analyzeType t
    putSemPosn p
    let st = typeToSymbolType t
    return $ TypedParam param semT SemTag{posn = p, typeInfo = NodeType st}
analyzeParam (Param param p) = do
    putSemPosn p
    vt <- freshTVar
    return $ Param param SemTag{posn = p, typeInfo = NodeType vt}

{-
    Analysis of the body of a definition
-}
analyzeDefBody :: SigAnalysisRes -> Parser (Def SemanticTag, TableEntry)
analyzeDefBody (SigAnalysisRes (Untyped st) Mut p (i, entry)) =
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType st}
    in return (VarDef i Nothing tg, entry)
analyzeDefBody (SigAnalysisRes (Typed semT) Mut p (i, entry)) =
    let tg = SemTag{posn = p, typeInfo = DefType . MonoType . typeToSymbolType $ semT}
    in return (VarDef i (Just semT) tg, entry)
analyzeDefBody (SigAnalysisRes (Untyped st) (Arr es) p (i, entry)) = do
    semEs <- mapM (stackTrace ("while analyzing the dimensions of array " ++ i) . analyzeExpr) es
    typesEs <- mapM getNodeType semEs
    putSemPosn p
    mapM_ (unify . (,) (SymType IntType)) typesEs
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType st}
    return (ArrayDef i semEs Nothing tg, entry)
analyzeDefBody (SigAnalysisRes (Typed semT) (Arr es) p (i, entry)) = do
    semEs <- mapM (stackTrace ("while analyzing the dimensions of array " ++ i) . analyzeExpr) es
    typesEs <- mapM getNodeType semEs
    putSemPosn p
    mapM_ (unify . (,) (SymType IntType)) typesEs
    let tg = SemTag{posn = p, typeInfo = DefType . MonoType . typeToSymbolType $ semT}
    return (ArrayDef i semEs (Just semT) tg, entry)
analyzeDefBody (SigAnalysisRes (Untyped st) (Fun semPs e) p (i, _)) = do
    -- Hold the free vars outside the body
    outerScopeVars <- getFreeTVars
    -- Open scope for params names and their types
    openScopeInNames
    addFreeTVars st
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
    let fType = paramsToFun SymType paramTypes eT
    unify (st, fType)
    reT <- getNodeType semE
    checkConstraint reT (NotAllowedFunType $ "Function " ++ i ++ " cannot return function type: " ++ pretty reT)
    -- Close scope
    closeScopeInNames
    putFreeTVars outerScopeVars
    let fScheme = MonoType st
    let tg = SemTag{posn = p, typeInfo = DefType fScheme}
    return (FunDef i semPs Nothing semE tg, FunEntry fScheme (map ide semPs))
analyzeDefBody (SigAnalysisRes (Typed semT) (Fun semPs e) p (i, _)) = do
    -- Open scope for params names and their types
    openScopeInNames
    outerScopeVars <- getFreeTVars
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
    let fType = paramsToFun SymType paramTypes eT
    let st = typeToSymbolType semT
    unify (st, fType)
    reT <- getNodeType semE
    checkConstraint reT (NotAllowedFunType $ "Function " ++ i ++ " cannot return function type: " ++ pretty reT)
    -- Close scope
    closeScopeInNames
    putFreeTVars outerScopeVars
    let fScheme = MonoType st
    let tg = SemTag{posn = p, typeInfo = DefType fScheme}
    return (FunDef i semPs (Just semT) semE tg, FunEntry fScheme (map ide semPs))

-- Semantic analysis of expressions

analyzeExpr :: Expr AlexPosn -> Parser (Expr SemanticTag)
analyzeExpr = recSemExpr indSemExpr

-- Inductively analyze expressions using mapM for expression functor
recSemExpr :: (ExprF (Expr SemanticTag) -> Parser (Expr SemanticTag))
         -> Expr AlexPosn
         -> Parser (Expr SemanticTag)
recSemExpr g expr =
    let aexpr = stackTrace ("while analyzing expr " ++ pretty expr) . recSemExpr g
        atype = stackTrace ("while analyzing expr " ++ pretty expr) . analyzeType
        alet = stackTrace ("while analyzing expr " ++ pretty expr) . analyzeLet
        aclause = stackTrace ("while analyzing expr " ++ pretty expr) . analyzeClause
    in case expr of
        Expr (ForExpr i l u e) p -> do
            semL <- aexpr l
            semU <- aexpr u
            openScopeInNames
            insertName i (FunEntry (MonoType . SymType $ IntType) [])
            semE <- aexpr e
            closeScopeInNames
            putSemPosn p
            g (ForExpr i semL semU semE)
        Expr (ForDownExpr i u l e) p -> do
            semU <- aexpr u
            semL <- aexpr l
            openScopeInNames
            insertName i (FunEntry (MonoType . SymType $ IntType) [])
            semE <- aexpr e
            closeScopeInNames
            putSemPosn p
            g (ForDownExpr i semU semL semE)
        Expr ef p -> do
            semEf <- mapM aexpr ef
            putSemPosn p
            g semEf
        NewType t p -> do
            semT <- atype t
            putSemPosn p
            semNewType semT
        LetIn l e p -> do
            semL <- alet l
            semE <- aexpr e
            putSemPosn p
            semLetIn semL semE
        MatchExpr e cs p -> do
            semE <- aexpr e
            semCs <- mapM aclause cs
            putSemPosn p
            semMatchExpr semE semCs

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
        ConstrEntry t _ _ -> retE (ConstConstrExpr i) (constTypeToSymbolType t)
        _                 -> throwInternalError $
            "Entry: " ++ show entry ++ " is not expected for constructor identifier key " ++ i

semFunAppExpr :: Identifier -> [Expr SemanticTag] -> Parser (Expr SemanticTag)
semFunAppExpr i es = let funTypeToArgTypes = funToArgs stCoAlg in do
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
                    let inf = paramsToFun SymType ts v
                    unify (t, inf)
                    checkConstraint v (NotAllowedFunType $ "Function " ++ i ++ " cannot return function type: " ++ pretty v)
                    retE (FunAppExpr i es) v
        ParamEntry t _ -> do
            ts <- mapM getNodeType es
            v <- freshTVar
            let inf = paramsToFun SymType ts v
            unify (t, inf)
            rt <- resolveType t
            let argTypes = funTypeToArgTypes rt
            case compare (length es) (length argTypes) of
                LT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty rt ++ " is applied to too few arguments"
                GT -> throwSem $ "Param " ++ i ++ " of type " ++ pretty rt ++ " is applied to too many arguments"
                EQ -> retE (FunAppExpr i es) v
        PatternEntry t -> do
            ts <- mapM getNodeType es
            v <- freshTVar
            let inf = paramsToFun SymType ts v
            unify (t, inf)
            rt <- resolveType t
            let argTypes = funTypeToArgTypes rt
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
        ConstrEntry t psT _ ->
            case compare (length es) (length psT) of
                LT -> throwSem $ "Constructor " ++ i ++ " is applied to too few arguments"
                GT -> throwSem $ "Constructor " ++ i ++ " is applied to too many arguments"
                EQ -> do
                    ts <- mapM getNodeType es
                    v <- freshTVar
                    let inf = paramsToFun SymType ts v
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
    let nt = SymType . RefType $ typeToSymbolType t
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
    mapM_ (\t -> unify (et, t)) patTs
    outT <- freshTVar
    let getExp (Match _ expr _) = expr
    let patExps = map getExp cs
    patExpTs <- mapM getNodeType patExps
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