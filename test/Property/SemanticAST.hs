module Property.SemanticAST (semanticAST) where

import qualified Data.Map as M
import qualified Data.Set as S

import Test.QuickCheck
import Common.Token
import Common.AST

import Common.SymbolType
import Semantics.Utils (hasDuplicates)

import Property.ArbitraryAST (boundedListOf)

type Scope = M.Map String TypeScheme
type TypeScope = S.Set String

arbitraryIdentifier :: Gen Identifier
arbitraryIdentifier = ("id_" ++) <$> boundedListOf (7, 7) g where
  g = elements ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

arbitraryTypeIdentifier :: Gen Identifier
arbitraryTypeIdentifier = ("ty_" ++) <$> boundedListOf (7, 7) g where
  g = elements ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

arbitraryConstrIdentifier :: Gen ConstrIdentifier
arbitraryConstrIdentifier = ("Co_" ++) <$> boundedListOf (7, 7) g where
  g = elements ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

typeInScope :: TypeScope -> Gen Identifier
typeInScope s = elements (S.toList s)

semanticAST :: Arbitrary b => Gen (AST b)
semanticAST = do
  k <- choose (0 :: Int, 6 :: Int)
  f M.empty S.empty k where
    f :: Arbitrary b => Scope -> TypeScope -> Int -> Gen (AST b)
    f _ _ 0 = return []
    f s ts n = do
      (d, s', ts') <- arbTypeDef ts
      (Right d:) <$> f s' ts' (n - 1)
  -- g = frequency [(2, Left <$> arbLetDef), (1, Right <$> arbTypeDef)]

arbTypeDef :: Arbitrary b => TypeScope -> Gen (TypeDef b, Scope, TypeScope)
arbTypeDef s = do
  typesToDef <- boundedListOf (1, 3) arbitraryTypeIdentifier
  let s' = S.union s (S.fromList typesToDef)
  arbTDefs <- mapM (arbTDef s') typesToDef
  let f outT (Constr i ts _) = (i, MonoType $ paramsToFunType (map typeToSymbolType ts) outT)
  let getConstrs (TDef i cs p) = map (f (typeToSymbolType $ Type (UserDefinedType i) p)) cs
  let constrs = foldl (\acc td -> getConstrs td ++ acc) [] arbTDefs
  td <- TypeDef arbTDefs <$> arbitrary
  return (td, M.fromList constrs, s')

arbTDef :: Arbitrary b => TypeScope -> Identifier -> Gen (TDef b)
arbTDef s t = do
  cIds <- suchThat (boundedListOf (1, 2) arbitraryConstrIdentifier) (not . hasDuplicates)
  TDef t <$> mapM (arbConstr s) cIds <*> arbitrary

arbConstr :: Arbitrary b => TypeScope -> ConstrIdentifier -> Gen (Constr b)
arbConstr s i = Constr i <$> boundedListOf (0, 2) (arbType s) <*> arbitrary

arbType :: Arbitrary b => TypeScope -> Gen (Type b)
arbType s = sized g where
  g n = Type <$> arbTypeF s (resize (div n 2) (arbType s)) <*> arbitrary

arbTypeF :: TypeScope -> Gen t -> Gen (TypeF t)
arbTypeF s r = sized gen where
  gen 0 = do
    i <- typeInScope s
    elements [UnitType, IntType, CharType, BoolType, FloatType, UserDefinedType i]
  gen n = do
    i <- choose (1, 3) :: Gen Int
    oneof [gen (div n 2), RefType <$> r, ArrayType i <$> r, FunType <$> r <*> r]
