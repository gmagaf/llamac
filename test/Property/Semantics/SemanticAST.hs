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
-- The sizes work exponentially

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
      (TypeDef [TDef "t1" [Constr "C1" [] b, Constr "C2" [Type IntType b] b, Constr "C3" [Type (UserDefinedType "t1") b] b] b] b,
        M.fromList [("C1", ConstType (UserDefinedType "t1")),
                    ("C2", ConstType (FunType (ConstType IntType) (ConstType (UserDefinedType "t1")))),
                    ("C3", ConstType (FunType (ConstType (UserDefinedType "t1")) (ConstType (UserDefinedType "t1"))))],
        S.fromList ["t1"]),
      (TypeDef [TDef "t2" [Constr "D1" [] b, Constr "D2" [Type IntType b] b, Constr "D3" [Type (UserDefinedType "t2") b] b] b] b,
        M.fromList [("D1", ConstType (UserDefinedType "t2")),
                    ("D2", ConstType (FunType (ConstType IntType) (ConstType (UserDefinedType "t2")))),
                    ("D3", ConstType (FunType (ConstType (UserDefinedType "t2")) (ConstType (UserDefinedType "t2"))))],
        S.fromList ["t2"]),
        (TypeDef [TDef "t3" [Constr "E1" [] b, Constr "E2" [Type IntType b] b, Constr "E3" [Type (UserDefinedType "t3") b] b] b] b,
        M.fromList [("E1", ConstType (UserDefinedType "t3")),
                    ("E2", ConstType (FunType (ConstType IntType) (ConstType (UserDefinedType "t3")))),
                    ("E3", ConstType (FunType (ConstType (UserDefinedType "t3")) (ConstType (UserDefinedType "t3"))))],
        S.fromList ["t3"]),
        (TypeDef [TDef "t4" [Constr "F1" [] b, Constr "F2" [Type IntType b] b, Constr "F3" [Type (UserDefinedType "t4") b] b] b] b,
        M.fromList [("F1", ConstType (UserDefinedType "t4")),
                    ("F2", ConstType (FunType (ConstType IntType) (ConstType (UserDefinedType "t4")))),
                    ("F3", ConstType (FunType (ConstType (UserDefinedType "t4")) (ConstType (UserDefinedType "t4"))))],
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
    let r = gen (n - 1)
    i <- choose (1, 3) :: Gen Int
    oneof [r,
           Type . RefType <$> r <*> arbitrary,
           Type . ArrayType i <$> r <*> arbitrary,
           Type <$> (FunType <$> r <*> r) <*> arbitrary]

arbLetDef :: Arbitrary b => Scope -> TypeScope -> Gen (LetDef b, Scope)
arbLetDef s ts = sized $ \n -> do
  ids <- boundedListOf (1, n) arbId
  let idc = length ids
  types <- boundedListOf (idc, idc) (resize n $ arbSimpleType ts)
  let pairs = zip ids types
  let constTypes = map (typeTo ConstType) types
  let entries = zip ids constTypes
  let s' = M.union s (M.fromList entries)
  isRec <- elements [True, False]
  b <- arbitrary
  if isRec
    then do
      -- Let rec -> run with s'
      defs <- mapM (resize n . arbDef s') pairs
      return (LetRec defs b, s')
    else do
      -- Let case -> run with s
      defs <- mapM (resize n . arbDef s) pairs
      return (Let defs b, s')

arbDef :: Arbitrary b => Scope -> (Identifier, Type b) -> Gen (Def b)
arbDef s (i, t@(Type tf _)) = sized $ \n -> case tf of
  RefType t'       -> frequency [(2, VarDef i <$> arbitrary),
                                 (1, VarDefTyped i t' <$> arbitrary)]
  ArrayType dim t' -> do
    b <- arbitrary
    let intType = Type IntType b
    frequency [(2, ArrayDef i <$> es intType <*> arbitrary),
               (1, ArrayDefTyped i <$> es intType <*> return t' <*> arbitrary)]
      where es et = boundedListOf (dim, dim) (resize n $ arbExpr s et)
  FunType _ _      -> do
    let types = funToTypes (\(Type tf' _) -> Left tf') t
    let pc = length types - 1
    let argTypes = take pc types
    let outT = last types
    pids <- suchThat (boundedListOf (pc, pc) arbId) (not . hasDuplicates)
    ps <- mapM arbParam (zip pids argTypes)
    let s' = M.union s (M.fromList $ zipWith pEntry ps argTypes)
    frequency [(2, FunDef i ps <$> resize n (arbExpr s' outT) <*> arbitrary),
               (3, FunDefTyped i ps t <$> resize n (arbExpr s' outT) <*> arbitrary)]
  _ -> frequency [(2, FunDef i [] <$> resize n (arbExpr s t) <*> arbitrary),
                  (1, FunDefTyped i [] t <$> resize n (arbExpr s t) <*> arbitrary)]
  where
    pEntry :: Param b -> Type b -> (Identifier, ConstType)
    pEntry p t' = (ide p, typeTo ConstType t')

arbParam :: Arbitrary b => (Identifier, Type b) -> Gen (Param b)
arbParam (i, t) = oneof [Param i <$> arbitrary,
                         TypedParam i t <$> arbitrary]

arbExpr :: Arbitrary b => Scope -> Type b -> Gen (Expr b)
arbExpr s t@(Type tf _) = sized gen where
  gen 0 = do
    case tf of
      IntType -> Expr . IntCExpr <$> arbitraryIntConstant <*> arbitrary
      FloatType -> (Expr . FloatCExpr <$> arbitraryFloatConstant) <*> arbitrary
      CharType -> (Expr . CharCExpr <$> arbitraryCharConstant) <*> arbitrary
      BoolType -> Expr <$> elements [TrueCExpr, FalseCExpr] <*> arbitrary
      UnitType -> Expr UnitCExpr <$> arbitrary
      RefType (Type (ArrayType _ _) _) -> do
        b <- arbitrary
        def <- resize 0 $ arbDef s ("id_ref_arr", t)
        return (LetIn (Let [def] b) (Expr (ConstExpr "id_ref_arr") b) b)
      RefType t' -> NewType t' <$> arbitrary
      UserDefinedType _ -> do
        i <- fst <$> suchThat (elements (M.toList s)) (\(_, ct) -> typeTo ConstType t == ct)
        if isUpper (head i)
          then Expr (ConstConstrExpr i) <$> arbitrary
          else Expr (ConstExpr i) <$> arbitrary
      ArrayType _ _ -> do
        b <- arbitrary
        def <- resize 0 $ arbDef s ("id_arr", t)
        return (LetIn (Let [def] b) (Expr (ConstExpr "id_arr") b) b)
      FunType _ _ -> do
        b <- arbitrary
        def <- resize 0 $ arbDef s ("id_fun", t)
        return (LetIn (Let [def] b) (Expr (ConstExpr "id_fun") b) b)
  gen n = do
    let r = resize (n - 1) . arbExpr s
    oneof [funApp r, ifgen r, loopgen r] where
    loopgen r = case tf of
      UnitType -> do
        b <- arbitrary
        condExpr <- r (Type BoolType b)
        lExpr <- r (Type IntType b)
        uExpr <- r (Type IntType b)
        e <- r t
        i <- arbId
        fe <- resize (n - 1) $ arbExpr (M.insert i (ConstType IntType) s) t
        elements [Expr (WhileExpr condExpr e) b,
                  Expr (ForExpr i lExpr uExpr fe) b,
                  Expr (ForDownExpr i uExpr lExpr fe) b]
      _ -> do
        b <- arbitrary
        e1 <- r t
        e2 <- r (Type UnitType b)
        return (Expr (BinOpExpr SemicolonOp e2 e1) b)
    ifgen r = case tf of
      UnitType -> do
        b <- arbitrary
        condExpr <- r (Type BoolType b)
        e <- r t
        return (Expr (IfThenExpr condExpr e) b)
      _ -> do
        b <- arbitrary
        condExpr <- r (Type BoolType b)
        e1 <- r t
        e2 <- r t
        return (Expr (IfThenElseExpr condExpr e1 e2) b)
    funApp r =
      let funs = filter (\(_, ct) -> outFunType ctCoAlg ct == typeTo ConstType t) (M.toList s)
          ctToType :: Arbitrary b' => ConstType -> Gen (Type b')
          ctToType (ConstType ctf) = do
            b <- arbitrary
            tf' <- mapM ctToType ctf
            return (Type tf' b)
      in if null funs
        then gen 0
        else do
          (fun, ct) <- elements funs
          let argctTypes = funToArgs ctCoAlg ct
          argTypes <- mapM ctToType argctTypes
          args <- mapM r argTypes
          if isUpper (head fun)
          then if null args then Expr (ConstConstrExpr fun) <$> arbitrary
                            else Expr (ConstrAppExpr fun args) <$> arbitrary
          else if null args then Expr (ConstExpr fun) <$> arbitrary
                            else Expr (FunAppExpr fun args) <$> arbitrary
