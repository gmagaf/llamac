module Property.Semantics.SemanticAST (semanticTypesAST) where

import qualified Data.Map as M
import qualified Data.Set as S

import Test.QuickCheck

import Common.Token
import Common.AST
import Common.SymbolType
import Semantics.Utils (hasDuplicates, SemanticTag (..), TypeInfo (..))

import Property.Utils
import Property.Lexer.ArbitraryTokens
    ( arbIdWithLength,
      arbConstrIdWithLength )

type Scope = M.Map String ConstType
type TypeScope = S.Set PosnId

arbTypeId :: Gen PosnId
arbTypeId = addPosn . ("ty_" ++) <$> arbIdWithLength 7

arbConstrId :: Gen ConstrIdentifier
arbConstrId = ("Co_" ++) <$> arbConstrIdWithLength 7

typeInScope :: TypeScope -> Gen Identifier
typeInScope s = identifier <$> elements (S.toList s)

-- Generator for semantically correct programs
-- that define types.

semanticTypesAST :: Gen (AST SemanticTag)
semanticTypesAST = sized $ \n -> do
  k <- choose (0 :: Int, logSize n)
  AST <$> f n S.empty k <*> arbTag NotTypable where
    f :: Int -> TypeScope -> Int -> Gen [Either (LetDef SemanticTag) (TypeDef SemanticTag)]
    f _ _  0 = return []
    f n ts k = do
      (tdef, _, ts') <- arbTypeDef ts
      (Right tdef :) <$> f n ts' (k - 1)

arbTypeDef :: TypeScope -> Gen (TypeDef SemanticTag, Scope, TypeScope)
arbTypeDef s = sized $ \n -> do
  typesToDef <- boundedListOf (1, logSize n) arbTypeId
  let s' = S.union s (S.fromList typesToDef)
  arbTDefs <- mapM (arbTDef s' . identifier) typesToDef
  let f outT (Constr i ts _) = (i, paramsToFun (map (typeTo addPosn :: Type b -> ConstType) ts) outT)
  let getConstrs (TDef i cs p) = map (f (typeTo addPosn $ Type (UserDefinedType i) p)) cs
  let constrs = foldl (\acc td -> getConstrs td ++ acc) [] arbTDefs
  td <- TypeDef arbTDefs <$> arbTag NotTypable
  return (td, M.fromList constrs, s')

arbTDef :: TypeScope -> Identifier -> Gen (TDef SemanticTag)
arbTDef s t = sized $ \n -> do
  cIds <- suchThat (boundedListOf (1, logSize n) arbConstrId) (not . hasDuplicates)
  TDef t <$> mapM (arbConstr s) cIds <*> arbTag NotTypable

arbConstr :: TypeScope -> ConstrIdentifier -> Gen (Constr SemanticTag)
arbConstr s i = sized $ \n -> Constr i <$> boundedListOf (0, logSize n) (arbType s) <*> arbTag NotTypable

arbType :: TypeScope -> Gen (Type SemanticTag)
arbType s = sized g where
  g n = Type <$> arbTypeF s (resize (div n 2) (arbType s)) <*> arbTag NotTypable

arbTypeF :: TypeScope -> Gen t -> Gen (TypeF Identifier t)
arbTypeF s r = sized gen where
  gen 0 = do
    let baseTypes = [UnitType, IntType, CharType, BoolType, FloatType]
    if null s then elements baseTypes
    else do
      i <- typeInScope s
      elements $ UserDefinedType i : baseTypes
  gen n = do
    i <- choose (1, 3) :: Gen Int
    oneof [gen (div n 2), RefType <$> r, ArrayType i <$> r, FunType <$> r <*> r]
