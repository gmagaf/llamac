module Semantics.TypeAnalysis (analyzeTypeDef, analyzeType) where

import Control.Monad (when, void, zipWithM)

import Common.Token (ConstrIdentifier)
import Common.AST
     (TypeF(..),
      Type(..),
      Constr(..),
      TDef(..),
      TypeDef(..),
      NameDef(ide),
      tag)
import Common.SymbolTable (TypeTableEntry(..), TableEntry(..))
import Common.SymbolType (ConstType(..), PosnId (..), TypeId, typeTo, paramsToFun, typeTo2)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser, stackTrace, getSource)
import Parser.SymbolTableUtils (openScopeInTypes, openScopeInNames)
import Semantics.Utils hiding (posn)
import Common.PrintAST (Pretty(pretty))
import Data.List (foldl')

-- Semantic analysis of type definitions

analyzeTypeDef :: TypeDef AlexPosn -> Parser (TypeDef SemanticTag)
analyzeTypeDef (TypeDef tDefs p) = do
        src <- getSource
        let typeNames = map (\t -> PosnId (ide t) src (tag t)) tDefs
        openScopeInTypes
        openScopeInNames
        mapM_ (insertTypeDef typeNames) tDefs
        semTDefs <- zipWithM analyzeTDef typeNames tDefs
        return $ TypeDef semTDefs (cpPosn p)

insertTypeDef :: [PosnId] -> TDef AlexPosn -> Parser ()
insertTypeDef typesInDef (TDef tId cs p) =
    let constrNames :: [ConstrIdentifier]
        constrNames = map ide cs
        checkDuplicateConstrs :: Parser ()
        checkDuplicateConstrs = when (hasDuplicates constrNames) $
            throwSem $ "Type " ++ tId ++ " cannot have duplicate constructors"
        checkTypeInCtx :: Type AlexPosn -> Parser ConstType
        checkTypeInCtx = typeTo2 aux where
            aux (Type _ tp) tName =
                let maybeId = foldl' (\acc pId -> if identifier pId == tName then Just pId else acc) Nothing typesInDef
                in do
                    putSemPosn tp
                    maybe (findTypeId tName) return maybeId
        checkConstrParams :: Constr AlexPosn -> Parser (ConstrIdentifier, [ConstType])
        checkConstrParams (Constr c ts _) = do
            checkedTs <- mapM checkTypeInCtx ts
            return (c, checkedTs)
    in do
        putSemPosn p
        checkDuplicateConstrs
        constrs <- mapM checkConstrParams cs
        src <- getSource
        let typeId = PosnId {identifier = tId, source = src, posn = p}
        insertType tId (TypeEntry typeId constrs)

analyzeTDef :: PosnId -> TDef AlexPosn -> Parser (TDef SemanticTag)
analyzeTDef pId (TDef tId cs p) = do
    semCs <- mapM (analyzeConstr pId) cs
    return $ TDef tId semCs (cpPosn p)

analyzeConstr :: PosnId -> Constr AlexPosn -> Parser (Constr SemanticTag)
analyzeConstr tId (Constr cId params p) = do
    semParams <- mapM (stackTrace ("while analyzing constr " ++ cId) . analyzeType) params
    paramTypes <- mapM (typeTo findTypeId) semParams
    let outputType = ConstType $ UserDefinedType tId
    let typeOfConstr = paramsToFun paramTypes outputType
    insertName cId (ConstrEntry typeOfConstr paramTypes outputType)
    return $ Constr cId semParams (cpPosn p)

recSemType :: (TypeF (TypeId (Type SemanticTag)) (Type SemanticTag) -> Parser (Type SemanticTag))
    -> Type AlexPosn
    -> Parser (Type SemanticTag)
recSemType f t@(Type tf p) = do
    let aType = stackTrace ("while analyzing type " ++ pretty t) . recSemType f
    semTf <- mapM aType tf
    putSemPosn p
    f semTf

analyzeType :: Type AlexPosn -> Parser (Type SemanticTag)
analyzeType = recSemType aType where
    aType :: TypeF (TypeId (Type SemanticTag)) (Type SemanticTag) -> Parser (Type SemanticTag)
    aType (ArrayType dim _) | dim < 1 = throwSem "Dimension of array type can't be less than 1"
    aType (UserDefinedType t) = do
        void (findTypeId t)
        Type (UserDefinedType t) . cpPosn <$> getSemPosn
    aType tf = Type tf . cpPosn <$> getSemPosn
