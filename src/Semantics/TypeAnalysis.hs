module Semantics.TypeAnalysis (analyzeTypeDef, analyzeType) where

import qualified Data.Set as Set
import Control.Monad (when)

import Common.Token (Identifier, ConstrIdentifier)
import Common.AST
     (TypeF(..),
      Type(..),
      Constr(..),
      TDef(..),
      TypeDef(..),
      NameDef(ide))
import Common.SymbolTable (TypeTableEntry(..), TableEntry(..))
import Common.SymbolType (ConstType(..), typeToConstType)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser)
import Semantics.Utils

-- Semantic analysis of type definitions

analyzeTypeDef :: TypeDef AlexPosn -> Parser (TypeDef SemanticTag)
analyzeTypeDef (TypeDef tDefs p) =
    let typeNames = Set.fromList $ map ide tDefs
    in do
        openScopeInTable
        mapM_ (insertTypeDef typeNames) tDefs
        semTDefs <- mapM analyzeTDef tDefs
        return $ TypeDef semTDefs (cpPosn p)

insertTypeDef :: Set.Set Identifier -> TDef AlexPosn -> Parser ()
insertTypeDef typesInDef (TDef tId cs p) =
    let constrNames :: [ConstrIdentifier]
        constrNames = map ide cs
        checkDuplicateConstrs :: Parser ()
        checkDuplicateConstrs = when (hasDuplicates constrNames) $
            throwSem $ "Type " ++ tId ++ " cannot have duplicate constructors"
        checkTypeInCtx :: Type AlexPosn -> Parser ConstType
        checkTypeInCtx t@(Type (UserDefinedType tName) tp) =
            if Set.member tName typesInDef
            then return (typeToConstType t)
            else do
                putSemPosn tp
                checkTypeInScope tName
                return (typeToConstType t)
        checkTypeInCtx t = return (typeToConstType t)
        checkConstrParams :: Constr AlexPosn -> Parser (ConstrIdentifier, [ConstType])
        checkConstrParams (Constr c ts _) = do
            checkedTs <- mapM checkTypeInCtx ts
            return (c, checkedTs)
    in do
        putSemPosn p
        checkDuplicateConstrs
        constrs <- mapM checkConstrParams cs
        insertType tId (TypeEntry constrs)

analyzeTDef :: TDef AlexPosn -> Parser (TDef SemanticTag)
analyzeTDef (TDef tId cs p) = do
    semCs <- mapM (analyzeConstr tId) cs
    return $ TDef tId semCs (cpPosn p)

analyzeConstr :: Identifier -> Constr AlexPosn -> Parser (Constr SemanticTag)
analyzeConstr tId (Constr cId params p) = do
    semParams <- mapM analyzeType params
    let paramTypes = map typeToConstType semParams
    let outputType = ConstType $ UserDefinedType tId
    let typeOfConstr = paramsToConstFunType paramTypes outputType
    insertName cId (ConstrEntry typeOfConstr paramTypes outputType)
    return $ Constr cId semParams (cpPosn p)

recSemType :: (TypeF (Type SemanticTag) -> Parser (Type SemanticTag))
    -> Type AlexPosn
    -> Parser (Type SemanticTag)
recSemType f (Type tf p) = do
    semTf <- mapM (recSemType f) tf
    putSemPosn p
    f semTf

analyzeType :: Type AlexPosn -> Parser (Type SemanticTag)
analyzeType = recSemType aType where
    aType :: TypeF (Type SemanticTag) -> Parser (Type SemanticTag)
    aType (ArrayType dim _) | dim < 1 = throwSem "Dimension of array type can't be less than 1"
    aType (UserDefinedType t) = do
        checkTypeInScope t
        Type (UserDefinedType t) . cpPosn <$> getSemPosn
    aType tf = Type tf . cpPosn <$> getSemPosn
