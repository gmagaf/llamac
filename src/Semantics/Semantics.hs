module Semantics.Semantics (SemanticTag(..), Analyzable, sem, semTag,
                            TypeAble, infer, typeCheck,
                            analyzeAST, semConstr) where

import qualified Data.Set as Set
import Control.Monad (when)

import Common.Token (Identifier, ConstrIdentifier)
import Common.PrintAST (pretty)
import Common.AST
import Common.SymbolTable (SymbolType(..), TableEntry (..),
    query, update, insert, openScope, closeScope, typeToSymbolType)
import Lexer.Lexer (AlexPosn, printPosn)
import Parser.ParserM (Parser, throwSemanticError, getSymbols, putSymbols, getAndIncrVarTypeC)

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
            Nothing -> throwSemanticError $ "Could not compute type of expr at: " ++ printPosn (posn tg)
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
        _          -> throwSemanticError $
                        "Symbol " ++ k ++ " at " ++ printPosn p ++ " is not in scope"

updateSymbolType :: AlexPosn -> String -> TableEntry -> Parser ()
updateSymbolType p k entry = queryAndRun p k run where
    run (TypeEntry _) = do
        symbols <- getSymbols
        putSymbols $ update k entry symbols
    run _ = throwSemanticError $
                "The type " ++ k ++ " at " ++ printPosn p ++ " is not in scope"

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

-- Functions for analyzing nodes
analyzeAST :: AST AlexPosn -> Parser (AST SemanticTag)
analyzeAST []                 = return []
analyzeAST (Left def : ast)   = (:) . Left <$> sem def <*> analyzeAST ast
analyzeAST (Right tDef : ast) = (:) . Right <$> sem tDef <*> analyzeAST ast

instance Analyzable TypeDef where
    sem (TypeDef tDefs p) = do
        openScopeInTable
        let insertTypeName (TDef typeName _ _) = insertSymbols typeName (TypeEntry [])
        mapM_ insertTypeName tDefs
        semTDefs <- mapM sem tDefs
        return $ TypeDef semTDefs SemTag{posn = p, exprType = Nothing}

instance Analyzable TDef where
    sem (TDef tName cs p) =
        let checkDuplicateConstrs :: Parser ()
            checkDuplicateConstrs = when (hasDuplicates (map (\(Constr c _ _) -> c) cs)) $
                throwSemanticError $ "Type " ++ tName ++ " cannot have duplicate constructors at " ++ printPosn p
            csParams :: [Constr AlexPosn] -> Parser [(ConstrIdentifier, [SymbolType])]
            csParams [] = return []
            csParams (Constr c _ cp:constrs) = queryAndRun cp c run where
                run (ConstrEntry _ params _) = ((c, params) :) <$> csParams constrs
                run _ = throwSemanticError $
                    "The constructor " ++ c ++ " at " ++ printPosn cp ++ " is not in scope"
        in do
            checkDuplicateConstrs
            semCs <- mapM (semConstr tName) cs
            constrs <- csParams cs
            updateSymbolType p tName (TypeEntry constrs)
            return $ TDef tName semCs SemTag{posn = p, exprType = Nothing}

semConstr :: Identifier -> Constr AlexPosn -> Parser (Constr SemanticTag)
semConstr tName (Constr cName params p) = do
    semParams <- mapM sem params
    let paramTypes = map typeToSymbolType semParams
    let typeOfConstr = paramsToType paramTypes
    insertSymbols cName (ConstrEntry typeOfConstr paramTypes outputType)
    return $ Constr cName semParams SemTag{posn = p, exprType = Nothing} where
        outputType = SymType $ UserDefinedType tName
        paramsToType [] = outputType
        paramsToType (t:ts) = SymType (FunType t (paramsToType ts))

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
        if dim < 1 then throwSemanticError $ "Dimension of array type can't be less than 1 at " ++ printPosn p
        else do
            semT <- sem t
            return $ Type (ArrayType dim semT) SemTag{posn = p, exprType = Nothing}
    sem (Type (UserDefinedType t) p) =
        queryAndRun p t (\_ -> return $ Type (UserDefinedType t) SemTag{posn = p, exprType = Nothing})

instance Analyzable LetDef where
    sem _ = undefined