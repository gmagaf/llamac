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
    let typesInDef = foldl (\acc tDef -> ide tDef : acc) [] tDefs
    in do
        openScopeInTypes
        mapM_ (insertTypeDef typesInDef) tDefs
        semTDefs <- mapM semTDef tDefs
        return $ TypeDef semTDefs SemTag{posn = p, symType = Nothing}

insertTypeDef :: [Identifier] -> TDef AlexPosn -> Parser ()
insertTypeDef typesInDef (TDef tId cs p) =
    let typesInDefSet :: Set.Set Identifier
        typesInDefSet = Set.fromList typesInDef
        constrNames :: [ConstrIdentifier]
        constrNames = map ide cs
        checkDuplicateConstrs :: Parser ()
        checkDuplicateConstrs = when (hasDuplicates constrNames) $
            throwSemAtPosn ("Type " ++ tId ++ " cannot have duplicate constructors")  p
        checkTypesInCtx :: [Type AlexPosn] -> Parser [ConstType]
        checkTypesInCtx [] = return []
        checkTypesInCtx (t@(Type (UserDefinedType tName) tp):ts) =
            if Set.member tName typesInDefSet
            then (typeToConstType t:) <$> checkTypesInCtx ts
            else do
                putSemPosn tp
                checkTypeInScope tName
                (typeToConstType t:) <$> checkTypesInCtx ts
        checkTypesInCtx (t:ts) = (typeToConstType t:) <$> checkTypesInCtx ts
        checkConstrParams :: Constr AlexPosn -> Parser (ConstrIdentifier, [ConstType])
        checkConstrParams (Constr c ts _) = do
            checkedTs <- checkTypesInCtx ts
            return (c, checkedTs)
    in do
        checkDuplicateConstrs
        constrs <- mapM checkConstrParams cs
        insertType tId (TypeEntry constrs)

semTDef :: TDef AlexPosn -> Parser (TDef SemanticTag)
semTDef (TDef tId cs p) = do
    semCs <- mapM (semConstr tId) cs
    return $ TDef tId semCs SemTag{posn = p, symType = Nothing}

semConstr :: Identifier -> Constr AlexPosn -> Parser (Constr SemanticTag)
semConstr tId (Constr cId params p) = do
    semParams <- mapM analyzeType params
    let paramTypes = map typeToConstType semParams
    let typeOfConstr = paramsToConstFunType paramTypes outputType
    insertName cId (ConstrEntry typeOfConstr paramTypes outputType)
    return $ Constr cId semParams SemTag{posn = p, symType = Nothing} where
        outputType = ConstType $ UserDefinedType tId

analyzeType :: Type AlexPosn -> Parser (Type SemanticTag)
analyzeType (Type UnitType p)  = return $ Type UnitType SemTag{posn = p, symType = Nothing}
analyzeType (Type IntType p)   = return $ Type IntType SemTag{posn = p, symType = Nothing}
analyzeType (Type CharType p)  = return $ Type CharType SemTag{posn = p, symType = Nothing}
analyzeType (Type BoolType p)  = return $ Type BoolType SemTag{posn = p, symType = Nothing}
analyzeType (Type FloatType p) = return $ Type FloatType SemTag{posn = p, symType = Nothing}
analyzeType (Type (FunType s t) p) = do
    semS <- analyzeType s
    semT <- analyzeType t
    return $ Type (FunType semS semT) SemTag{posn = p, symType = Nothing}
analyzeType (Type (RefType t) p) = do
    semT <- analyzeType t
    return $ Type (RefType semT) SemTag{posn = p, symType = Nothing}
analyzeType (Type (ArrayType dim t) p) = do
    if dim < 1 then throwSemAtPosn "Dimension of array type can't be less than 1" p
    else do
        semT <- analyzeType t
        return $ Type (ArrayType dim semT) SemTag{posn = p, symType = Nothing}
analyzeType (Type (UserDefinedType t) p) = do
    putSemPosn p
    checkTypeInScope t
    return $ Type (UserDefinedType t) SemTag{posn = p, symType = Nothing}