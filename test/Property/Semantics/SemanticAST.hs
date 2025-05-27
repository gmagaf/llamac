module Property.Semantics.SemanticAST (semanticTypesAST, semanticScopesAST) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isUpper)
import Test.QuickCheck

import Common.Token
import Common.AST
import Common.SymbolType
import Semantics.Utils (hasDuplicates)
import Property.Utils
import Property.Lexer.ArbitraryTokens

type Scope = M.Map String ConstType
type TypeScope = S.Set String

arbId :: Gen Identifier
arbId = ("id_" ++) <$> arbIdWithLength 7

arbTypeId :: Gen Identifier
arbTypeId = ("ty_" ++) <$> arbIdWithLength 7

arbConstrId :: Gen ConstrIdentifier
arbConstrId = ("Co_" ++) <$> arbConstrIdWithLength 7

typeInScope :: TypeScope -> Gen Identifier
typeInScope s = elements (S.toList s)

-- Generator for semantically correct programs
-- that define types.
-- The sizes work exponentially

semanticTypesAST :: Arbitrary b => Gen (AST b)
semanticTypesAST = sized $ \n -> do
  k <- choose (0 :: Int, n :: Int)
  f S.empty k where
    f :: Arbitrary b => TypeScope -> Int -> Gen (AST b)
    f _  0 = return []
    f ts k = do
      (tdef, _, ts') <- arbTypeDef ts
      (Right tdef :) <$> f ts' (k - 1)

arbTypeDef :: Arbitrary b => TypeScope -> Gen (TypeDef b, Scope, TypeScope)
arbTypeDef s = sized $ \n -> do
  typesToDef <- boundedListOf (1, n) arbTypeId
  let s' = S.union s (S.fromList typesToDef)
  arbTDefs <- mapM (arbTDef s') typesToDef
  let f outT (Constr i ts _) = (i, paramsToFun ConstType (map (typeTo ConstType) ts) outT)
  let getConstrs (TDef i cs p) = map (f (typeTo ConstType $ Type (UserDefinedType i) p)) cs
  let constrs = foldl (\acc td -> getConstrs td ++ acc) [] arbTDefs
  td <- TypeDef arbTDefs <$> arbitrary
  return (td, M.fromList constrs, s')

arbTDef :: Arbitrary b => TypeScope -> Identifier -> Gen (TDef b)
arbTDef s t = sized $ \n -> do
  cIds <- suchThat (boundedListOf (1, n) arbConstrId) (not . hasDuplicates)
  TDef t <$> mapM (arbConstr s) cIds <*> arbitrary

arbConstr :: Arbitrary b => TypeScope -> ConstrIdentifier -> Gen (Constr b)
arbConstr s i = sized $ \n -> Constr i <$> boundedListOf (0, n) (arbType s) <*> arbitrary

arbType :: Arbitrary b => TypeScope -> Gen (Type b)
arbType s = sized g where
  g n = Type <$> arbTypeF s (resize (div n 2) (arbType s)) <*> arbitrary

arbTypeF :: TypeScope -> Gen t -> Gen (TypeF t)
arbTypeF s r = sized gen where
  gen 0 = do
    let baseTypes = [UnitType, IntType, CharType, BoolType, FloatType]
    if S.null s then elements baseTypes
    else do
      i <- typeInScope s
      elements $ UserDefinedType i : baseTypes
  gen n = do
    i <- choose (1, 3) :: Gen Int
    oneof [gen (div n 2), RefType <$> r, ArrayType i <$> r, FunType <$> r <*> r]

-- Generator for semantically correct programs
-- that define functions.
-- The sizes work logarithmically

semanticScopesAST :: Arbitrary b => Gen (AST b)
semanticScopesAST = do
  k <- choose (0 :: Int, 6 :: Int)
  f M.empty S.empty k where
    f :: Arbitrary b => Scope -> TypeScope -> Int -> Gen (AST b)
    f _ _ 0 = return []
    f s ts k = do
      (ldef, ls) <- arbLetDef s ts
      (tdef, s', ts') <- arbSimpleTypeDef
      (d, s'', ts'') <- frequency
        [(10, return (Left ldef, ls, ts)),
         (1, return (Right tdef, s', ts'))]
      (d:) <$> f s'' ts'' (k - 1)

arbSimpleTypeDef :: Arbitrary b => Gen (TypeDef b, Scope, TypeScope)
arbSimpleTypeDef = do
  b <- arbitrary
  elements
    [
      (TypeDef [TDef "t1" [Constr "C1" [] b, Constr "C2" [] b, Constr "C3" [] b] b] b,
        M.fromList [("C1", ConstType (UserDefinedType "t1")),
                    ("C2", ConstType (UserDefinedType "t1")),
                    ("C3", ConstType (UserDefinedType "t1"))],
        S.fromList ["t1"]),
      (TypeDef [TDef "t2" [Constr "D1" [] b, Constr "D2" [] b, Constr "D3" [] b] b] b,
        M.fromList [("D1", ConstType (UserDefinedType "t2")),
                    ("D2", ConstType (UserDefinedType "t2")),
                    ("D3", ConstType (UserDefinedType "t2"))],
        S.fromList ["t2"]),
        (TypeDef [TDef "t3" [Constr "E1" [] b, Constr "E2" [] b, Constr "E3" [] b] b] b,
        M.fromList [("E1", ConstType (UserDefinedType "t3")),
                    ("E2", ConstType (UserDefinedType "t3")),
                    ("E3", ConstType (UserDefinedType "t3"))],
        S.fromList ["t3"]),
        (TypeDef [TDef "t4" [Constr "F1" [] b, Constr "F2" [] b, Constr "F3" [] b] b] b,
        M.fromList [("F1", ConstType (UserDefinedType "t4")),
                    ("F2", ConstType (UserDefinedType "t4")),
                    ("F3", ConstType (UserDefinedType "t4"))],
        S.fromList ["t4"])
    ]

arbSimpleType :: Arbitrary b => TypeScope -> Gen (Type b)
arbSimpleType ts = sized gen where
  user = map UserDefinedType (S.toList ts)
  basicTf = [UnitType, IntType, CharType, BoolType, FloatType] ++ user
  gen 0 = do
    b <- arbitrary
    elements $ map (`Type` b) basicTf
  gen n = do
    let r = gen (div n 2)
    i <- choose (1, 3) :: Gen Int
    oneof [r, (Type . RefType <$> r) <*> arbitrary,
            (Type . ArrayType i <$> r) <*> arbitrary,
            Type <$> (FunType <$> gen 0 <*> gen 0) <*> arbitrary] -- TODO: Add support for fun types

arbLetDef :: Arbitrary b => Scope -> TypeScope -> Gen (LetDef b, Scope)
arbLetDef s ts = do
  ids <- boundedListOf (1, 6) arbId
  let idc = length ids
  types <- boundedListOf (idc, idc) (arbSimpleType ts)
  let pairs = zip ids types
  let constTypes = map (typeTo ConstType) types
  let entries = zip ids constTypes
  let s' = M.union s (M.fromList entries)
  isRec <- elements [True, False]
  b <- arbitrary
  if isRec
    then do
      -- Let rec -> run with s'
      defs <- mapM (arbDef s') pairs
      return (LetRec defs b, s')
    else do
      -- Let case -> run with s
      defs <- mapM (arbDef s) pairs
      return (Let defs b, s')

arbDef :: Arbitrary b => Scope -> (Identifier, Type b) -> Gen (Def b)
arbDef s (i, t@(Type tf _)) = do
  case tf of
    RefType t'       -> frequency [(2, VarDef i <$> arbitrary),
                             (1, VarDefTyped i t' <$> arbitrary)]
    ArrayType dim t' -> frequency [(2, ArrayDef i <$> es dim s (ConstType IntType) <*> arbitrary),
                                   (1, ArrayDefTyped i <$> es dim s (ConstType IntType) <*> return t' <*> arbitrary)]
    FunType _ _      -> do
      let types = funToTypes (\(Type tf' _) -> Left tf') t
      let pc = length types - 1
      let argTypes = take pc types
      let outT = typeTo ConstType (last types)
      pids <- suchThat (boundedListOf (pc, pc) arbId) (not . hasDuplicates)
      ps <- mapM arbParam (zip pids argTypes)
      let s' = M.union s (M.fromList $ zipWith pEntry ps argTypes)
      frequency [(2, FunDef i ps <$> arbExpr s' outT <*> arbitrary),
                 (3, FunDefTyped i ps t <$> arbExpr s' outT <*> arbitrary)]
    _ -> frequency [(2, FunDef i [] <$> arbExpr s ct <*> arbitrary),
                    (1, FunDefTyped i [] t <$> arbExpr s ct <*> arbitrary)]
  where
    ct = typeTo ConstType t
    es dim s' t' = boundedListOf (1, dim) (arbExpr s' t')
    pEntry :: Param b -> Type b -> (Identifier, ConstType)
    pEntry p t' = (ide p, typeTo ConstType t')

arbParam :: Arbitrary b => (Identifier, Type b) -> Gen (Param b)
arbParam (i, t) = oneof [Param i <$> arbitrary,
                         TypedParam i t <$> arbitrary]

arbExpr :: Arbitrary b => Scope -> ConstType -> Gen (Expr b)
arbExpr s t = Expr <$> arbExprF s t (arbExpr s) <*> arbitrary

arbExprF :: Scope -> ConstType -> (ConstType -> Gen e) -> Gen (ExprF e)
arbExprF s t@(ConstType tf) r = sized gen where
  gen 0 = case tf of
    IntType -> IntCExpr <$> arbitraryIntConstant
    FloatType -> FloatCExpr <$> arbitraryFloatConstant
    CharType -> CharCExpr <$> arbitraryCharConstant
    BoolType -> elements [TrueCExpr, FalseCExpr]
    UnitType -> return UnitCExpr
    UserDefinedType _ -> do
      i <- fst <$> suchThat (elements (M.toList s)) (\(_, ct) -> t == ct)
      if isUpper (head i)
        then return (ConstConstrExpr i)
        else return (ConstExpr i)
    -- TODO: Add support for more types
  gen n = do
    let funs = filter (\(_, ct) -> outFunType ctCoAlg ct == t) (M.toList s)
    let r' = resize (div n 2) . r
    if null funs
      then gen 0
      else do
        (fun, ct) <- elements funs
        let argTypes = funToArgs ctCoAlg ct
        args <- mapM r' argTypes
        if isUpper (head fun)
        then return (ConstrAppExpr fun args)
        else return (FunAppExpr fun args)

-- sample (arbExpr (Data.Map.fromList [("f", ConstType (FunType (ConstType CharType) (ConstType IntType )))]) (ConstType IntType))

-- m = Data.Map.fromList [("f", ConstType (FunType (ConstType CharType) (ConstType IntType )))]
-- filter (\(_, ct) -> outFunConstType ct == (ConstType IntType )) (M.toList m)
-- (f, ct) = head $ filter (\(_, ct) -> outFunConstType ct == (ConstType IntType )) (M.toList m)
-- funTypeToArgConstTypes ct