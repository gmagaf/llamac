module Semantics.TypeAnalysis (analyzeTypeDef, analyzeType) where

import qualified Data.Set as S
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
import Common.SymbolType (ConstType(..), typeTo, paramsToFun)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser, stackTrace)
import Semantics.Utils
import Common.PrintAST (Pretty(pretty))

-- Semantic analysis of type definitions

analyzeTypeDef :: TypeDef AlexPosn -> Parser (TypeDef SemanticTag)
analyzeTypeDef (TypeDef tDefs p) =
    let typeNames = S.fromList $ map ide tDefs
    in do
        openScopeInTypes
        openScopeInNames
        mapM_ (insertTypeDef typeNames) tDefs
        semTDefs <- mapM analyzeTDef tDefs
        return $ TypeDef semTDefs (cpPosn p)

insertTypeDef :: S.Set Identifier -> TDef AlexPosn -> Parser ()
insertTypeDef typesInDef (TDef tId cs p) =
    let constrNames :: [ConstrIdentifier]
        constrNames = map ide cs
        checkDuplicateConstrs :: Parser ()
        checkDuplicateConstrs = when (hasDuplicates constrNames) $
            throwSem $ "Type " ++ tId ++ " cannot have duplicate constructors"
        checkTypeInCtx :: Type AlexPosn -> Parser ConstType
        checkTypeInCtx t@(Type (UserDefinedType tName) tp) =
            if S.member tName typesInDef
            then return (typeTo ConstType t)
            else do
                putSemPosn tp
                checkTypeInScope tName
                return (typeTo ConstType t)
        checkTypeInCtx t = return (typeTo ConstType t)
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
    semParams <- mapM (stackTrace ("while analyzing constr " ++ cId) . analyzeType) params
    let paramTypes = map (typeTo ConstType) semParams
    let outputType = ConstType $ UserDefinedType tId
    let typeOfConstr = paramsToFun ConstType paramTypes outputType
    insertName cId (ConstrEntry typeOfConstr paramTypes outputType)
    return $ Constr cId semParams (cpPosn p)

recSemType :: (TypeF (Type SemanticTag) -> Parser (Type SemanticTag))
    -> Type AlexPosn
    -> Parser (Type SemanticTag)
recSemType f t@(Type tf p) = do
    let aType = stackTrace ("while analyzing type " ++ pretty t) . recSemType f
    semTf <- mapM aType tf
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
