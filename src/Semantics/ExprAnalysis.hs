module Semantics.ExprAnalysis(analyzeLet, analyzeExpr) where

import Control.Monad (when, (>=>))
import qualified Data.Map as M
import qualified Data.Bifunctor as B

import Common.Token (Identifier)
import Common.AST
import Common.PrintAST
import Common.SymbolTable
import Common.SymbolType
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser, throwInternalError, getSymbols)
import Semantics.Utils
import Semantics.Unifier
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
    -- Pre-analyze all defs and write type vars in current typespace
    preSemsMono <- mapM preAnalyzeDef defs
    -- Generalize all functions
    preSems <- mapM genFunTypes preSemsMono
    putSemPosn p
    let keyEntryPairs = map snd preSems
    -- Open a new names space and add all definitions
    openScopeInNames
    mapM_ (uncurry insertName) keyEntryPairs
    let semDefs = map fst preSems
    return $ Let semDefs (cpPosn p)
analyzeLet (LetRec defs p) =
    let defsC = length defs
        instScheme d = do
            sch <- getDefScheme d
            t <- inst sch
            return (ide d, t)
        removeRecTypesFromSig st =
            let sig = drop defsC $ funTypeToTypes st
            in typesToFunType sig
        getRecParamType st = typesToFunType $ take defsC $ funTypeToTypes st
    in do
    {-
        Given a recursive let:
            let rec id x = x
            and f : int ref = x
            and g x = id x
            and mutable x
            and mutable ar [id 2, g 3] : float
        we first create type variable placeholders for
        mutable vars and arrays, here x : @0
        and we transform the rec let to a simple rec
        with functions for fix point
            let id id (f : int ref) g x@Fix1 (ar : array [*, *] of float) x = x
            and f id (f : int ref) g x (ar : array [*, *] of float) = x
            and g id (f : int ref) g x@Fix1 (ar : array [*, *] of float) x = id x
            and x id (f : int ref) g x (ar : array [*, *] of float) = x
            and ar id (f : int ref) g x (ar : array [*, *] of float) =
                    let dim@Fix : int = id 2 and dim@Fix : int = g 3 in ar <- this will force
                        the analysis of id 2 and g 3 and the monomorphic type of id
                        inside let def which will be polymorphic outside of let
        This produces the following fix types
        id : forall @1. forall @2. forall @3. forall @4. @1 -> int ref -> @2 -> @3 -> array [*, *] of float -> @4 -> @4
        f  : forall @5. forall @6. forall @7. @5 -> int ref -> @6 -> @7 -> array [*, *] of float -> @7
        g  : forall @9. forall @10. forall @11. forall @12. (@11 -> @12) -> int ref -> @9 -> @10 -> array [*, *] of float -> @11 -> @12
        x  : forall @13. forall @14. forall @15. @13 -> int ref -> @14 -> @15 -> array [*, *] of float -> @15
        ar : forall @18. (int -> int) -> int ref -> (int -> int) -> @18 -> array [*, *] of float -> array [*, *] of float
        Then, we instantiate and get
        id : @21 -> int ref -> @22 -> @23 -> array [*, *] of float -> @24 -> @24
        f  : @25 -> int ref -> @26 -> @27 -> array [*, *] of float -> @27
        g  : (@30 -> @31) -> int ref -> @28 -> @29 -> array [*, *] of float -> @30 -> @31
        x  : @32 -> int ref -> @33 -> @34 -> array [*, *] of float -> @34
        ar : (int -> int) -> int ref -> (int -> int) -> @35 -> array [*, *] of float -> array [*, *] of float
        and the final types are computed as
        id : @24 -> @24
        f  : @27
        g  : @30 -> @31
        x  : @34
        ar : array [*, *] of float
        which we combine to create the fixPointType: (@24 -> @24) -> @27 -> (@30 -> @31) -> @34 -> array [*, *] of float
        which we finally unify with all the ind params:
        21 -> int ref -> @22 -> @23 -> array [*, *] of float
        @25 -> int ref -> @26 -> @27 -> array [*, *] of float
        (@30 -> @31) -> int ref -> @28 -> @29 -> array [*, *] of float
        @32 -> int ref -> @33 -> @34 -> array [*, *] of float
        (int -> int) -> int ref -> (int -> int) -> @35 -> array [*, *] of float
        and we unify the mutable tvar types
        @0 ~ @34
        And we finally get finalResolvedTypes
        id : int -> int <- notice, here id is monomorphic in let defs
        f  : int ref
        g  : int -> int
        x  : int ref
        ar : array [*, *] of float
        We insert the above in the symbol table and we
        rerun the analysis for the function definitions
        in order to get the most general polymorphic functions
    -}
        mutTVars <- mapM insertMutTVarsPlaceholders defs
        preAnalyzeFixMono <- mapM preAnalyzeDef (trace ("FIXPOINT TRANS: " ++ show (toFixPoint defs)) $ toFixPoint defs)
        preAnalyzeFix <- mapM genFunTypes preAnalyzeFixMono
        let fixDefs = map fst preAnalyzeFix
        putSemPosn p
        openScopeInTypes
        fixTypes <- mapM instScheme (trace ("FIXDEFS: " ++ show fixDefs) fixDefs)
        let finalTypePairs = map (B.second removeRecTypesFromSig) (trace ("FIXTYPES: " ++ show fixTypes) fixTypes)
        let finalTypesMap = M.fromList finalTypePairs
        let finalTypes = map snd (trace ("FINAL TYPES MAP:" ++ show finalTypesMap) finalTypePairs)
        let fixPointType = typesToFunType finalTypes
        let indParamTypes = map (getRecParamType . snd) (trace ("FIXTYPEEEE: " ++ show fixPointType) fixTypes)
        mapM_ (\t -> unify (fixPointType, t)) indParamTypes
        mapM_ unifyMutableTVars (zipWith (\a b -> (a, snd b)) mutTVars finalTypePairs)
        finalResolvedTypesMap <- mapM resolveType finalTypesMap
        closeScopeInTypes
        openScopeInNames
        mapM_ (\d -> insertNameDef (d, finalResolvedTypesMap M.! ide d)) (trace ("FINAAAALL: " ++ show finalResolvedTypesMap) defs)
        -- SO FAR IT MAKES SENSE
        s <- trace "SO FAR IT MAKES SENSE" getSymbols
        preSemsMono <- mapM analyzeDef defs
        -- Generalize all functions
        preSems <- mapM genFunTypes preSemsMono
        putSemPosn p
        let keyEntryPairs = map snd preSems
        -- Update all definitions
        mapM_ (uncurry updateName) keyEntryPairs
        let semDefs = map fst preSems
        return $ Let semDefs (cpPosn p)
        -- semDefs <- trace (pretty s) $ mapM analyzeDef defs -- <-- this should be analyxeDef
        -- return $ LetRec semDefs (cpPosn p)

genFunTypes :: (Def SemanticTag, (Identifier, TableEntry)) -> Parser (Def SemanticTag, (Identifier, TableEntry))
genFunTypes (FunDef i ps e tg, (k, FunEntry (MonoType t) params)) = do
    scheme <- gen t
    return (FunDef i ps e tg{typeInfo = DefType scheme}, (k, FunEntry scheme params))
genFunTypes pair = return pair

insertMutTVarsPlaceholders :: Def AlexPosn -> Parser (Maybe SymbolType)
insertMutTVarsPlaceholders (VarDef {}) = do
    tv <- freshTVar
    let varType = SymType . RefType $ tv
    return (Just varType)
insertMutTVarsPlaceholders (ArrayDef _ es _) = do
    let dims = length es
    tv <- freshTVar
    let arrayType = SymType . ArrayType dims $ tv
    return (Just arrayType)
insertMutTVarsPlaceholders _ = return Nothing

unifyMutableTVars :: (Maybe SymbolType, SymbolType) -> Parser ()
unifyMutableTVars (Just t1, t2) = unify (t1, t2)
unifyMutableTVars (Nothing, _) = return ()

typeOfDef :: Def b -> Maybe (Type b)
typeOfDef (VarDefTyped _ t _)      = Just (Type (RefType t) (tag t))
typeOfDef (ArrayDefTyped _ es t _) = Just (Type (ArrayType (length es) t) (tag t))
typeOfDef (FunDefTyped _ _ t _ _)  = Just t
typeOfDef _                        = Nothing

toFixPoint :: [Def AlexPosn] -> [Def AlexPosn]
toFixPoint defs =
    let funToParam d = case typeOfDef d of
            Just t  -> TypedParam (ide d) t (tag d)
            Nothing -> Param (ide d) (tag d)
        renameDuplicate param (acc, m) =
            let i = ide param
                t = tag param
                m' = M.insertWith (+) i (1 :: Int) m
            in case M.lookup i m of
                Just c  -> (Param (i ++ "@Fix" ++ show c) t:acc, m')
                Nothing -> (param:acc, m')
        -- We need to ensure that the following type checks:
        -- let rec f x f = f -- <- This (f) is the second param
        --         f x = f x -- <- this (f) is the same function defined
        indParams = map funToParam defs
        fixParams ps = fst $ foldr renameDuplicate ([], M.empty) (indParams ++ ps)
        fixFunction (VarDef i p)             = FunDef i (fixParams []) (Expr (ConstExpr i) p) p
        fixFunction (VarDefTyped i _ p)      = FunDef i (fixParams []) (Expr (ConstExpr i) p) p
        fixFunction (ArrayDef i d p)         = FunDef i (fixParams []) (Expr (LetIn (Let (map (\e -> FunDefTyped "dim@Fix" [] (Type IntType (tag e)) e (tag e)) d) p) (Expr (ConstExpr i) p )) p) p
        fixFunction (ArrayDefTyped i d _ p)  = FunDef i (fixParams []) (Expr (LetIn (Let (map (\e -> FunDefTyped "dim@Fix" [] (Type IntType (tag e)) e (tag e)) d) p) (Expr (ConstExpr i) p )) p) p
        fixFunction (FunDef i ps e p)        = FunDef i (fixParams ps) e p
        fixFunction (FunDefTyped i ps _ e p) = FunDef i (fixParams ps) e p
    in map fixFunction defs

{-
    This pre-analyzes the definition
    That means it does not create any
    name in the symbol table but it can
    create fresh variables in typespace.
    It will add type vars for mutables and functions
    in the current typespace and will infer
    monotypes for functions.
    It checks:
        - sem analysis of explicit types
        - array dimensions are of type int
        - sem analysis of function params
        - sem analysis of function bodies
        - infer (mono)type of function and unify tvar with it
-}
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
    semT <- analyzeType t
    putSemPosn p
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
    semT <- analyzeType t
    semEs <- mapM analyzeExpr es
    typesEs <- mapM getNodeType semEs
    putSemPosn p
    mapM_ (unify . (,) (SymType IntType)) typesEs
    rSemEs <- mapM resolveNodeType semEs
    let dims = length es
    let arrayType = SymType . ArrayType dims $ typeToSymbolType semT
    let entry = (i, ArrayEntry arrayType dims)
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType arrayType}
    let semDef = ArrayDef i rSemEs tg
    return (semDef, entry)
preAnalyzeDef (FunDef i ps e p) = do
    -- First we make sure that there are no duplicates in params
    let paramNames = map ide ps
    when (hasDuplicates paramNames) $
        throwSemAtPosn ("Fun " ++ i ++ " cannot have duplicate params") p
    -- Fresh tv is the overall type of the function
    tv <- freshTVar
    -- Open scope for params names and their types
    openScopeInTable
    -- Analyze params and body in the current scope
    semPs <- mapM (analyzeParam i) ps
    semE <- analyzeExpr e
    -- Collect the results: the new param types, the expr type and unify tv with the result fun type
    putSemPosn p
    eT <- getNodeType semE
    paramTypes <- mapM getNodeType semPs
    let fType = paramsToFunType paramTypes eT
    unify (tv, fType)
    rSemPs <- mapM resolveNodeType semPs
    rSemE <- resolveNodeType semE
    rtv <- resolveType tv
    -- Close scope
    closeScopeInTable
    let fScheme = MonoType rtv
    let entry = (i, FunEntry fScheme paramNames)
    let tg = SemTag{posn = p, typeInfo = DefType fScheme}
    let semDef = FunDef i rSemPs rSemE tg
    return (semDef, entry)
preAnalyzeDef (FunDefTyped i ps t e p) = do
    let paramNames = map ide ps
    -- First we make sure that there are no duplicates in params
    when (hasDuplicates paramNames) $
        throwSemAtPosn ("Fun " ++ i ++ " cannot have duplicate params") p
    -- We analyze the overall type of the function
    semT <- analyzeType t
    -- Open scope for params names and their types
    openScopeInTable
    -- Analyze params and body in the current scope
    semPs <- mapM (analyzeParam i) ps
    semE <- analyzeExpr e
    -- Collect the results: the new param types, the expr type and unify tv with the result fun type
    putSemPosn p
    eT <- getNodeType semE
    paramTypes <- mapM getNodeType semPs
    let fType = paramsToFunType paramTypes eT
    unify (typeToSymbolType semT, fType)
    rSemPs <- mapM resolveNodeType semPs
    rSemE <- resolveNodeType semE
    -- Close scope
    closeScopeInTable
    let fScheme = MonoType $ typeToSymbolType t
    let entry = (i, FunEntry fScheme paramNames)
    let tg = SemTag{posn = p, typeInfo = DefType fScheme}
    let semDef = FunDefTyped i rSemPs semT rSemE tg
    return (semDef, entry)

{-
    Analyzes params by writting them to
    the symbol table
    - Checks the explicit types of params
-}
analyzeParam :: Identifier -> Param AlexPosn -> Parser (Param SemanticTag)
analyzeParam fun (TypedParam param t p) = do
    semT <- analyzeType t
    putSemPosn p
    let st = typeToSymbolType t
    insertName param (ParamEntry st fun)
    return $ TypedParam param semT SemTag{posn = p, typeInfo = NodeType st}
analyzeParam fun (Param param p) = do
    putSemPosn p
    vt <- freshTVar
    insertName param (ParamEntry vt fun)
    return $ Param param SemTag{posn = p, typeInfo = NodeType vt}

insertNameDef :: (Def AlexPosn, SymbolType) -> Parser ()
insertNameDef (VarDef x _, fixt) = insertName x (MutableEntry fixt)
insertNameDef (VarDefTyped x t _, fixt) | fixt == SymType (RefType $ typeToSymbolType t) = insertName x (MutableEntry fixt)
insertNameDef (ArrayDef i es p, fixt) = do
    putSemPosn p
    let dims = length es
    when (null es) $ throwSem ("Definition of array " ++ i ++ " should contain at least one dimension")
    insertName i (ArrayEntry fixt dims)
insertNameDef (ArrayDefTyped i es t p, fixt) | fixt == SymType (ArrayType (length es) $ typeToSymbolType t) = do
    putSemPosn p
    when (null es) $ throwSem ("Definition of array " ++ i ++ " should contain at least one dimension")
    let dims = length es
    insertName i (ArrayEntry fixt dims)
insertNameDef (FunDef i ps _ p, fixt) = do
    putSemPosn p
    let paramNames = map ide ps
    scheme <- gen fixt
    insertName i (FunEntry scheme paramNames)
insertNameDef (FunDefTyped i ps t _ p, fixt) | fixt == typeToSymbolType t = do
    putSemPosn p
    let paramNames = map ide ps
    insertName i (FunEntry (MonoType fixt) paramNames)
insertNameDef (d, t)  = do
    putSemPosn (tag d)
    throwSem $ "Inserting mono type: " ++ pretty t ++ " for def " ++ show d ++ "  in let rec failed"

analyzeDef :: Def AlexPosn -> Parser (Def SemanticTag, (Identifier, TableEntry))
analyzeDef (VarDef x p) = do
    entry <- findName x
    case entry of
        MutableEntry t -> do
            let tg = SemTag{posn = p, typeInfo = DefType $ MonoType t}
            return (VarDef x tg, (x, entry))
        _ -> throwSem ""
analyzeDef (VarDefTyped x t p) = do
    semT <- analyzeType t
    entry <- findName x
    case entry of
        MutableEntry t' | t' == typeToSymbolType semT-> do
            let tg = SemTag{posn = p, typeInfo = DefType $ MonoType t'}
            return (VarDefTyped x semT tg, (x, entry))
        _ -> throwSem ""
analyzeDef (ArrayDef i es p) = do
    entry <- findName i
    case entry of
        ArrayEntry t dims | length es == dims -> do
            semEs <- mapM analyzeExpr es
            typesEs <- mapM getNodeType semEs
            putSemPosn p
            mapM_ (unify . (,) (SymType IntType)) typesEs
            rSemEs <- mapM resolveNodeType semEs
            let dims = length es
            let arrayType = SymType $ ArrayType dims t
            let entry = (i, ArrayEntry arrayType dims)
            let tg = SemTag{posn = p, typeInfo = DefType $ MonoType arrayType}
            let semDef = ArrayDef i rSemEs tg
            return (semDef, entry)
        _ -> throwSem ""
analyzeDef (ArrayDefTyped i es t p) = do
    semT <- analyzeType t
    semEs <- mapM analyzeExpr es
    typesEs <- mapM getNodeType semEs
    putSemPosn p
    mapM_ (unify . (,) (SymType IntType)) typesEs
    rSemEs <- mapM resolveNodeType semEs
    let dims = length es
    let arrayType = SymType . ArrayType dims $ typeToSymbolType semT
    let entry = (i, ArrayEntry arrayType dims)
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType arrayType}
    let semDef = ArrayDef i rSemEs tg
    return (semDef, entry)
analyzeDef d@(FunDef {}) = preAnalyzeDef d
analyzeDef d@(FunDefTyped {}) = preAnalyzeDef d

-- Semantic analysis of expressions

analyzeExpr :: Expr AlexPosn -> Parser (Expr SemanticTag)
analyzeExpr = recSemExpr indSemExpr

recSemExpr :: (ExprF SemanticTag (Expr SemanticTag) -> Parser (Expr SemanticTag))
         -> Expr AlexPosn
         -> Parser (Expr SemanticTag)
recSemExpr g (Expr ef p) = do
    semEf <- mapMExprF analyzeLet analyzeType analyzeClause (recSemExpr g) ef
    putSemPosn p
    g semEf

retE :: ExprF SemanticTag (Expr SemanticTag) -> SymbolType -> Parser (Expr SemanticTag)
retE ef t = do
    p <- getSemPosn
    return $ Expr ef SemTag{posn = p, typeInfo = NodeType t}

indSemExpr :: ExprF SemanticTag (Expr SemanticTag) -> Parser (Expr SemanticTag)
indSemExpr (IntCExpr c)    = retE (IntCExpr c) (SymType IntType)
indSemExpr (FloatCExpr c)  = retE (FloatCExpr c) (SymType FloatType)
indSemExpr (CharCExpr c)   = retE (CharCExpr c) (SymType CharType)
indSemExpr (StringCExpr c) = retE (StringCExpr c) (SymType (ArrayType 1 (SymType CharType)))
indSemExpr TrueCExpr       = retE TrueCExpr (SymType BoolType)
indSemExpr FalseCExpr      = retE FalseCExpr (SymType BoolType)
indSemExpr UnitCExpr       = retE UnitCExpr (SymType UnitType)
indSemExpr (ConstExpr i) = do
    entry <- findName i
    case (trace ("EVER HERE? " ++ i) entry) of
        FunEntry ft _ -> do
            t <- inst ft
            s <- getSymbols
            trace ("ALL OK AGAIN WITH " ++ i ++ "\n" ++ pretty s) retE (ConstExpr i) t
        ParamEntry t _ -> do
            retE (ConstExpr i) t
        MutableEntry t -> do
            retE (ConstExpr i) t
        ArrayEntry t _ -> do
            retE (ConstExpr i) t
        ConstrEntry {} -> undefined
indSemExpr (ConstConstrExpr i) = do
    entry <- findName i
    case entry of
        FunEntry _ _ -> undefined
        ParamEntry _ _ -> undefined
        MutableEntry _ -> undefined
        ArrayEntry _ _ -> undefined
        ConstrEntry t _ _ -> retE (ConstExpr i) (constTypeToSymbolType t)
indSemExpr (FunAppExpr i es) = do
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
indSemExpr e@(ArrayDim i dim) = findName i >>= run where
    run (ArrayEntry _ dims)
      | dim < 1 = throwSem $ "Cannot compute the " ++ show dim ++ " dimension of array " ++ i
      | dims < dim = throwSem $ "Cannot compute the " ++ show dim ++ " dimension of " ++ show dims ++ "-dim array " ++ i
      | otherwise = retE e (SymType IntType)
    run _ = throwSem $ "No array " ++ i ++ " in scope"
indSemExpr ef@(LetIn _ e) = do
    trace "LETINNNN" closeScopeInNames
    t <- getNodeType e
    retE ef t
-- TODO: Define
indSemExpr _ = undefined

-- Semantic analysis of clauses

analyzeClause :: Clause AlexPosn -> Parser (Clause SemanticTag)
analyzeClause = undefined

-- Semantic analysis of patterns

analyzePattern :: Pattern AlexPosn -> Parser (Pattern SemanticTag)
analyzePattern = undefined
