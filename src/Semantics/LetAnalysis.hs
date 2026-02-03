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
type DefinitionType = SymbolType
type KeyEntryPair = (Identifier, TableEntry)

data SigAnalyzedAST =
   Mut (Maybe (Type SemanticTag))
 | Arr [Expr AlexPosn] (Maybe (Type SemanticTag))
 | Fun [Param SemanticTag] (Maybe (Type SemanticTag)) (Expr AlexPosn)

data SigAnalysisRes = SigAnalysisRes SigAnalyzedAST DefinitionType AlexPosn KeyEntryPair

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
genResult (FunDef i ps outT e tg, FunEntry (MonoType t) params) = do
    -- Update all the free variables from the outer scope
    resolveFreeVars
    -- Resolve the type to generalize
    rt <- resolveType t
    -- Generalize type
    scheme <- gen rt
    return (FunDef i ps outT e tg{typeInfo = DefType scheme}, FunEntry scheme params)
genResult pair = return pair

entryPair :: SigAnalysisRes -> KeyEntryPair
entryPair (SigAnalysisRes _ _ _ pair) = pair

addMutVarToFree :: SigAnalysisRes -> Parser ()
addMutVarToFree (SigAnalysisRes (Mut Nothing) t _ _) = addFreeTVars t
addMutVarToFree (SigAnalysisRes (Arr _ Nothing) t _ _) = addFreeTVars t
addMutVarToFree _ = return ()

{-
    Analysis of the signature of a definition
-}
analyzeDefSig :: Def AlexPosn -> Parser SigAnalysisRes
analyzeDefSig (VarDef x Nothing p) = do
    tv <- freshTVar
    checkConstraint tv (NotPolymorphicVar $ "Cannot abstract on mutable var " ++ pretty tv)
    let varType = SymType . RefType $ tv
    return $ SigAnalysisRes (Mut Nothing) varType p (x, MutableEntry varType)
analyzeDefSig (VarDef x (Just t) p) = do
    semT <- stackTrace ("while analyzing mut var " ++ x) $ analyzeType t
    varType <- SymType . RefType <$> typeToSymbolType semT
    return $ SigAnalysisRes (Mut (Just semT)) varType p (x, MutableEntry varType)
analyzeDefSig (ArrayDef i es outT p) = do
    let dims = length es
    (semOutT, outDefT) <- case outT of
        Just t  -> do
            semT <- stackTrace ("while analyzing array " ++ i) $ analyzeType t
            outDefT <- typeToSymbolType semT
            return (Just semT, outDefT)
        Nothing -> do
            tv <- freshTVar
            checkConstraint tv (NotPolymorphicVar $ "Cannot abstract on array var " ++ pretty tv)
            return (Nothing, tv)
    let arrayType = SymType . ArrayType dims $ outDefT
    return $ SigAnalysisRes (Arr es semOutT) arrayType p (i, ArrayEntry arrayType dims)
analyzeDefSig (FunDef i ps outT e p) = do
    let paramNames = map ide ps
    when (hasDuplicates paramNames) $
        throwSemAtPosn ("Fun " ++ i ++ " cannot have duplicate params") p
    -- Analyze params in the current scope
    semPs <- mapM (stackTrace ("while analyzing the params of " ++ i) . analyzeParam) ps
    paramTypes <- mapM getNodeType semPs
    (semOutT, outDefT) <- case outT of
        Just t  -> do
            -- We analyze the out type of the function
            semT <- stackTrace ("while analyzing fun " ++ i) $ analyzeType t
            outDefT <- typeToSymbolType semT
            return (Just semT, outDefT)
        Nothing -> do
            -- Fresh outV is the output type of the function
            outV <- freshTVar
            return (Nothing, outV)
    checkConstraint outDefT (NotAllowedFunType $ "Function " ++ i ++ " cannot return function type: " ++ pretty outDefT)
    let fType = paramsToFun paramTypes outDefT
    return $ SigAnalysisRes (Fun semPs semOutT e) fType p (i, FunEntry (MonoType fType) paramNames)

{-
    Analyzes params by writting them to
    the symbol table
    - Checks the explicit types of params
-}
analyzeParam :: Param AlexPosn -> Parser (Param SemanticTag)
analyzeParam (TypedParam param t p) = do
    semT <- stackTrace ("while analyzing param " ++ param) $ analyzeType t
    st <- typeToSymbolType semT
    return $ TypedParam param semT SemTag{posn = p, typeInfo = NodeType st}
analyzeParam (Param param p) = do
    vt <- freshTVar
    return $ Param param SemTag{posn = p, typeInfo = NodeType vt}

{-
    Analysis of the body of a definition
-}
analyzeDefBody :: SigAnalysisRes -> Parser (Def SemanticTag, TableEntry)
analyzeDefBody (SigAnalysisRes (Mut t) st p (i, entry)) =
    let tg = SemTag{posn = p, typeInfo = DefType . MonoType $ st}
    in return (VarDef i t tg, entry)
analyzeDefBody (SigAnalysisRes (Arr es t) st p (i, entry)) = do
    semEs <- mapM (stackTrace ("while analyzing the dimensions of array " ++ i) . analyzeExpr) es
    typesEs <- mapM getNodeType semEs
    putSemPosn p
    mapM_ (unify . (,) (SymType IntType)) typesEs
    let tg = SemTag{posn = p, typeInfo = DefType $ MonoType st}
    return (ArrayDef i semEs t tg, entry)
analyzeDefBody (SigAnalysisRes (Fun semPs outT e) st p (i, _)) = do
    -- Hold the free vars outside the body
    outerScopeVars <- getFreeTVars
    -- Open scope for params names and their types
    addFreeTVars st
    openScopeInNames
    -- Insert params in the current scope
    let insertParam param = do
          t <- getNodeType param
          insertName (ide param) (ParamEntry t i)
          return t
    paramTypes <- mapM insertParam semPs
    semE <- stackTrace ("while analyzing the body of " ++ i) (analyzeExpr e)
    -- Collect the results: the new param types, the expr type and unify infered type with the expected fun type
    eT <- getNodeType semE
    let fType = paramsToFun paramTypes eT
    putSemPosn p
    unify (st, fType)
    -- Close scope
    closeScopeInNames
    putFreeTVars outerScopeVars
    let fScheme = MonoType st
    let tg = SemTag{posn = p, typeInfo = DefType fScheme}
    return (FunDef i semPs outT semE tg, FunEntry fScheme (map ide semPs))
