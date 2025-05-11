module Semantics.ExprAnalysis(analyzeLet, analyzeExpr) where

import Control.Monad ((>=>), when)

import Common.Token (Identifier)
import Common.AST
import Common.PrintAST
import Common.SymbolTable
import Common.SymbolType
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser)
import Semantics.Utils
import Semantics.Unifier
import Semantics.TypeAnalysis (analyzeType)
import Debug.Trace (trace)

-- Semantic analysis of definitions

analyzeLet :: LetDef AlexPosn -> Parser (LetDef SemanticTag)
analyzeLet (Let defs p) = do
    openScopeInTable
    preSems <- mapM preAnalyzeDef defs
    let keyEntryPairs = map snd preSems
    mapM_ (uncurry insertName) keyEntryPairs
    let semDefs = map fst preSems
    return $ Let semDefs (cpPosn p)
analyzeLet (LetRec _ _) = undefined

preAnalyzeDef :: Def AlexPosn -> Parser (Def SemanticTag, (Identifier, TableEntry))
preAnalyzeDef (VarDef x p) = do
    putSemPosn p
    tv <- freshTVar
    let varType = SymType . RefType $ tv
    let entry = (x, MutableEntry varType)
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType varType}
    let semDef = VarDef x tg
    return (semDef, entry)
preAnalyzeDef (VarDefTyped x t p) = do
    putSemPosn p
    semT <- analyzeType t
    let varType = SymType . RefType $ typeToSymbolType semT
    let entry = (x, MutableEntry varType)
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType varType}
    let semDef = VarDefTyped x semT tg
    return (semDef, entry)
preAnalyzeDef (ArrayDef i es p) = do
    semEs <- mapM analyzeExpr es
    typesEs <- mapM getNodeType semEs
    putSemPosn p
    mapM_ (unify . (,) (SymType IntType)) typesEs
    rSemEs <- mapM resolveNodeType semEs
    let dims = length es
    tv <- freshTVar
    let arrayType = SymType . ArrayType dims $ tv
    let entry = (i, ArrayEntry arrayType dims)
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType arrayType}
    let semDef = ArrayDef i rSemEs tg
    return (semDef, entry)
preAnalyzeDef (ArrayDefTyped i es t p) = do
    semEs <- mapM analyzeExpr es
    typesEs <- mapM getNodeType semEs
    putSemPosn p
    mapM_ (unify . (,) (SymType IntType)) typesEs
    rSemEs <- mapM resolveNodeType semEs
    let dims = length es
    semT <- analyzeType t
    let arrayType = SymType . ArrayType dims $ typeToSymbolType semT
    let entry = (i, ArrayEntry arrayType dims)
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType arrayType}
    let semDef = ArrayDef i rSemEs tg
    return (semDef, entry)
preAnalyzeDef (FunDefTyped i ps t e p) = do
    semT <- analyzeType t
    openScopeInTable
    semPs <- mapM (analyzeParam i) ps
    semE <- analyzeExpr e
    outT <- getNodeType semE
    paramTypes <- mapM getNodeType semPs
    let fType = paramsToFunType paramTypes outT
    putSemPosn p
    unify (typeToSymbolType semT, fType)
    rSemPs <- mapM resolveNodeType semPs
    rSemE <- resolveNodeType semE
    closeScopeInTable
    let paramNames = map ide ps
    let scheme = MonoType $ typeToSymbolType t
    let entry = (i, FunEntry scheme paramNames)
    let tg = SemTag{posn = p, typeInfo = DefType scheme}
    let semDef = FunDefTyped i rSemPs semT rSemE tg
    return (semDef, entry)
preAnalyzeDef (FunDef i ps e p) = do
    openScopeInTable
    semPs <- mapM (analyzeParam i) ps
    semE <- analyzeExpr e
    outT <- getNodeType semE
    rSemPs <- mapM resolveNodeType semPs
    paramTypes <- mapM getNodeType rSemPs
    let fType = paramsToFunType paramTypes outT
    closeScopeInTable
    scheme <- gen fType
    let paramNames = map ide ps
    let entry = (i, FunEntry scheme paramNames)
    let tg = SemTag{posn = p, typeInfo = DefType scheme}
    let semDef = FunDef i rSemPs semE tg
    return (semDef, entry)

analyzeParam :: Identifier -> Param AlexPosn ->Parser (Param SemanticTag)
analyzeParam fun (TypedParam param t p) = do
    putSemPosn p
    semT <- analyzeType t
    let st = typeToSymbolType t
    insertName param (ParamEntry st fun)
    return $ TypedParam param semT SemTag{posn = p, typeInfo = NodeType st}
analyzeParam fun (Param param p) = do
    putSemPosn p
    vt <- freshTVar
    insertName param (ParamEntry vt fun)
    return $ Param param SemTag{posn = p, typeInfo = NodeType vt}

insertNameDef :: Def AlexPosn -> Parser ()
insertNameDef (VarDef x p) = do
    putSemPosn p
    tv <- freshTVar
    let varType = SymType . RefType $ tv
    insertName x (MutableEntry varType)
insertNameDef (VarDefTyped x t p) = do
    putSemPosn p
    semT <- analyzeType t
    let varType = SymType . RefType $ typeToSymbolType semT
    insertName x (MutableEntry varType)
insertNameDef (ArrayDef i es p) = do
    putSemPosn p
    when (null es) $ throwSem ("Definition of array " ++ i ++ " should contain at least one dimension")
    tv <- freshTVar
    let dims = length es
    let arrayType = SymType . ArrayType dims $ tv
    insertName i (ArrayEntry arrayType dims)
insertNameDef (ArrayDefTyped i es t p) = do
    putSemPosn p
    when (null es) $ throwSem ("Definition of array " ++ i ++ " should contain at least one dimension")
    semT <- analyzeType t
    let dims = length es
    let arrayType = SymType . ArrayType dims $ typeToSymbolType semT
    insertName i (ArrayEntry arrayType dims)
insertNameDef (FunDef i ps _ p) = do
    putSemPosn p
    bot <- bottom
    let paramNames = map ide ps
    insertName i (FunEntry bot paramNames)
insertNameDef (FunDefTyped i ps t _ p) = do
    putSemPosn p
    semT <- analyzeType t
    let paramNames = map ide ps
    insertName i (FunEntry (MonoType $ typeToSymbolType semT) paramNames)


-- Semantic analysis of expressions

retE :: ExprF SemanticTag (Expr SemanticTag) -> SymbolType -> Parser (Expr SemanticTag)
retE ef t = do
    p <- getSemPosn
    return $ Expr ef SemTag{posn = p, typeInfo = NodeType t}

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
                    ts <- mapM getNodeType es
                    v <- freshTVar
                    openScopeInTable
                    t <- inst ft
                    let inf = paramsToFunType ts v
                    unify (t, inf)
                    res <- mapM resolveNodeType es
                    rv <- resolveType v
                    closeScopeInTable
                    retE (FunAppExpr i res) rv
        ParamEntry t _ -> do
            ts <- mapM getNodeType es
            v <- freshTVar
            unify (t, paramsToFunType ts v) -- Need to make sure that if the param is function is applied to all args
            res <- mapM resolveNodeType es
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
    t <- getNodeType e
    retE ef t
-- TODO: Define
semE _ = undefined

semExprF :: (ExprF SemanticTag (Expr SemanticTag) -> Parser (Expr SemanticTag))
         -> Expr AlexPosn
         -> Parser (Expr SemanticTag)
semExprF g (Expr ef p) = do
    semEf <- mapMExprF analyzeLet analyzeType analyzeClause (semExprF g) ef
    putSemPosn p
    g semEf

analyzeExpr :: Expr AlexPosn -> Parser (Expr SemanticTag)
analyzeExpr = semExprF semE

analyzeClause :: Clause AlexPosn -> Parser (Clause SemanticTag)
analyzeClause = undefined

-- Semantic analysis of patterns

analyzePattern :: Pattern AlexPosn -> Parser (Pattern SemanticTag)
analyzePattern = undefined
