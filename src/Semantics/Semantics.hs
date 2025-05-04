module Semantics.Semantics (SemanticState(..), SemanticTag(..),
                            Analyzable, sem, semTag,
                            TypeAble, infer, typeCheck,
                            analyzeAST) where

import qualified Data.Set as Set
import Control.Monad (when)

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST
import Common.SymbolTable (SymbolType(..), TableEntry (..), typeToSymbolType)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser)
import Parser.ParserState (SemanticState(varTypeC, posnOfSem))
import Semantics.Utils

-- This module contains the semantic analysis of the nodes
-- and decorates them with the semantic tag
data SemanticTag = SemTag {
                    posn :: AlexPosn,
                    exprType :: Maybe SymbolType
                    }
    deriving Show

-- Organize the semantic analyzable nodes in a class
class Node f => Analyzable f where
    sem :: f AlexPosn -> Parser (f SemanticTag)
    semTag :: f AlexPosn -> Parser SemanticTag
    semTag f = tag <$> sem f

class Analyzable f => TypeAble f where
    infer :: f AlexPosn -> Parser SymbolType
    infer f = do
        tg <- semTag f
        case exprType tg of
            Nothing -> throwSemAtPosn "Could not infer type of expr" (posn tg)
            Just t  -> return t
    typeCheck :: f AlexPosn -> SymbolType -> Parser Bool
    typeCheck f t = (t ==) <$> infer f

-- Functions for analyzing nodes
analyzeAST :: AST AlexPosn -> Parser (AST SemanticTag)
analyzeAST []                 = return []
analyzeAST (Left def : ast)   = (:) . Left <$> sem def <*> analyzeAST ast
analyzeAST (Right tDef : ast) = (:) . Right <$> sem tDef <*> analyzeAST ast

-- Semantic analysis of type definitions
instance Analyzable TypeDef where
    sem (TypeDef tDefs p) =
        let typeName (TDef name _ _) = name
            typesInDef = foldl (\acc tDef -> typeName tDef : acc) [] tDefs
        in do
            openScopeInTable
            mapM_ (insertTypeDef typesInDef) tDefs
            semTDefs <- mapM semTDef tDefs
            return $ TypeDef semTDefs SemTag{posn = p, exprType = Nothing}

insertTypeDef :: [Identifier] -> TDef AlexPosn -> Parser ()
insertTypeDef typesInDef (TDef tId cs p) =
    let typesInDefSet :: Set.Set Identifier
        typesInDefSet = Set.fromList typesInDef
        constrNames :: [ConstrIdentifier]
        constrNames = map (\(Constr c _ _) -> c) cs
        checkDuplicateConstrs :: Parser ()
        checkDuplicateConstrs = when (hasDuplicates constrNames) $
            throwSemAtPosn ("Type " ++ tId ++ " cannot have duplicate constructors")  p
        checkTypesInCtx :: [Type AlexPosn] -> Parser [SymbolType]
        checkTypesInCtx [] = return []
        checkTypesInCtx (t@(Type (UserDefinedType tName) tp):ts) =
            if Set.member tName typesInDefSet
            then (typeToSymbolType t:) <$> checkTypesInCtx ts
            else do
                checkTypeInScope tp tName
                (typeToSymbolType t:) <$> checkTypesInCtx ts
        checkTypesInCtx (t:ts) = (typeToSymbolType t:) <$> checkTypesInCtx ts
        checkConstrParams :: Constr AlexPosn -> Parser (ConstrIdentifier, [SymbolType])
        checkConstrParams (Constr c ts _) = do
            checkedTs <- checkTypesInCtx ts
            return (c, checkedTs)
    in do
        checkDuplicateConstrs
        constrs <- mapM checkConstrParams cs
        insertSymbols tId (TypeEntry constrs)

semTDef :: TDef AlexPosn -> Parser (TDef SemanticTag)
semTDef (TDef tId cs p) = do
    semCs <- mapM (semConstr tId) cs
    return $ TDef tId semCs SemTag{posn = p, exprType = Nothing}

semConstr :: Identifier -> Constr AlexPosn -> Parser (Constr SemanticTag)
semConstr tId (Constr cId params p) = do
    semParams <- mapM sem params
    let paramTypes = map typeToSymbolType semParams
    let typeOfConstr = paramsToFunType paramTypes outputType
    insertSymbols cId (ConstrEntry typeOfConstr paramTypes outputType)
    return $ Constr cId semParams SemTag{posn = p, exprType = Nothing} where
        outputType = SymType $ UserDefinedType tId

instance Analyzable Type where
    sem (Type UnitType p)  = return $ Type UnitType SemTag{posn = p, exprType = Nothing}
    sem (Type IntType p)   = return $ Type IntType SemTag{posn = p, exprType = Nothing}
    sem (Type CharType p)  = return $ Type CharType SemTag{posn = p, exprType = Nothing}
    sem (Type BoolType p)  = return $ Type BoolType SemTag{posn = p, exprType = Nothing}
    sem (Type FloatType p) = return $ Type FloatType SemTag{posn = p, exprType = Nothing}
    sem (Type (FunType s t) p) = do
        semS <- sem s
        semT <- sem t
        return $ Type (FunType semS semT) SemTag{posn = p, exprType = Nothing}
    sem (Type (RefType t) p) = do
        semT <- sem t
        return $ Type (RefType semT) SemTag{posn = p, exprType = Nothing}
    sem (Type (ArrayType dim t) p) = do
        if dim < 1 then throwSemAtPosn "Dimension of array type can't be less than 1" p
        else do
            semT <- sem t
            return $ Type (ArrayType dim semT) SemTag{posn = p, exprType = Nothing}
    sem (Type (UserDefinedType t) p) = do
        checkTypeInScope p t
        return $ Type (UserDefinedType t) SemTag{posn = p, exprType = Nothing}

-- Semantic analysis of definitions
instance Analyzable LetDef where
    -- TODO: Define
    sem _ = undefined

getExprType :: Expr SemanticTag -> Parser SymbolType
getExprType e = case exprType (tag e) of
    Nothing -> do
        let p = posn $ tag e
        throwSemAtPosn "Unable to compute type of expression" p
    Just t  -> return t

retE :: ExprF SemanticTag (Expr SemanticTag) -> SymbolType -> Parser (Expr SemanticTag)
retE ef t = do
    p <- getSemPosn
    return $ Expr ef SemTag{posn = p, exprType = Just t}

semE :: ExprF SemanticTag (Expr SemanticTag) -> Parser (Expr SemanticTag)
semE (IntCExpr c)    = retE (IntCExpr c) (SymType IntType)
semE (FloatCExpr c)  = retE (FloatCExpr c) (SymType FloatType)
semE (CharCExpr c)   = retE (CharCExpr c) (SymType CharType)
semE (StringCExpr c) = retE (StringCExpr c) (SymType (ArrayType 1 (SymType CharType)))
semE TrueCExpr       = retE TrueCExpr (SymType BoolType)
semE FalseCExpr      = retE FalseCExpr (SymType BoolType)
semE UnitCExpr       = retE UnitCExpr (SymType UnitType)
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
