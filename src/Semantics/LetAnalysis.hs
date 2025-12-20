module Semantics.LetAnalysis(analyzeLet) where

import Control.Monad (when, zipWithM)
import qualified Data.Bifunctor as B

import Common.Token (Identifier)
import Common.AST
import Common.PrintAST
import Common.SymbolTable
import Common.SymbolType (TypeScheme(..), SymbolType(..), paramsToFun)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser, stackTrace)
import Parser.SymbolTableUtils (openScopeInNames, closeScopeInNames)
import Semantics.TypeConstraints (TypeConstraint(..))
import Semantics.Utils
import Semantics.Unifier (gen, checkConstraint, unify)
import Semantics.TypeAnalysis (analyzeType)
import Semantics.ExprAnalysis (analyzeExpr)

-- Semantic analysis of definitions

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

entryPair :: SigAnalysisRes -> KeyEntryPair
entryPair (SigAnalysisRes _ _ _ pair) = pair

addMutVarToFree :: SigAnalysisRes -> Parser ()
addMutVarToFree (SigAnalysisRes (Untyped t) Mut _ _) = addFreeTVars t
addMutVarToFree (SigAnalysisRes (Untyped t) (Arr _) _ _) = addFreeTVars t
addMutVarToFree _ = return ()

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
