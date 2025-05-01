module Semantics.Semantics (SemanticTag(..), Analyzable, sem, semTag,
                            TypeAble, infer, typeCheck,
                            analyzeAST, semConstr) where

import qualified Data.Set as Set
import Control.Monad (when)

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST
import Common.SymbolTable (SymbolType(..), TableEntry (..),
    query, insert, openScope, closeScope, typeToSymbolType)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser, throwSemAtPosn, getSymbols, putSymbols)

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

-- Parser symbol table utiles
insertSymbols :: String -> TableEntry -> Parser ()
insertSymbols k entry = do
    symbols <- getSymbols
    putSymbols $ insert k entry symbols

queryAndRun :: AlexPosn -> String -> (TableEntry -> Parser a) -> Parser a
queryAndRun p k run = do
    symbols <- getSymbols
    case query k symbols of
        Just entry -> run entry
        _ -> throwSemAtPosn ("Symbol " ++ k ++ " is not in scope") p

checkTypeInScope :: AlexPosn -> String -> Parser ()
checkTypeInScope p k = do
    symbols <- getSymbols
    case query k symbols of
        Just (TypeEntry _) -> return ()
        _ -> throwSemAtPosn ("Symbol " ++ k ++ " is not in scope") p

openScopeInTable :: Parser ()
openScopeInTable = do
    symbols <- getSymbols
    putSymbols $ openScope symbols

closeScopeInTable :: Parser ()
closeScopeInTable = do
    symbols <- getSymbols
    putSymbols $ closeScope symbols

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

paramsToFunType :: [SymbolType] -> SymbolType -> SymbolType
paramsToFunType [] out = out
paramsToFunType (t:ts) out = SymType (FunType t (paramsToFunType ts out))

-- Functions for analyzing nodes
analyzeAST :: AST AlexPosn -> Parser (AST SemanticTag)
analyzeAST []                 = return []
analyzeAST (Left def : ast)   = (:) . Left <$> sem def <*> analyzeAST ast
analyzeAST (Right tDef : ast) = (:) . Right <$> sem tDef <*> analyzeAST ast

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

instance Analyzable LetDef where
    sem _ = undefined