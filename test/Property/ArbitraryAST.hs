module Property.ArbitraryAST (arbitraryAST) where

import Test.QuickCheck
import Common.Token
import Common.AST

arbitraryIdentifier :: Gen Identifier
arbitraryIdentifier = elements ["a", "b", "c", "d", "e", "f", "g", "foo", "bar", "main"]

arbitraryConstrIdentifier :: Gen ConstrIdentifier
arbitraryConstrIdentifier = elements ["A", "B", "C", "D", "Nil", "Cons", "Empty", "Tree"]

arbitraryIntConstant :: Gen IntConstant
arbitraryIntConstant = elements [0..42]

arbitraryFloatConstant :: Gen FloatConstant
arbitraryFloatConstant = elements [0.0, 2.56, 3.14, 0.420e+2, 42000.0e-3]

arbitraryCharConstant :: Gen CharConstant
arbitraryCharConstant = elements ["a", "7", "\\n", "\\\"", "\\xE9"]

arbitraryStringConstant :: Gen StringConstant
arbitraryStringConstant = elements ["foo", "bar", "Route66", "Name:\\t\\\"DouglasAdams\\\"\\nValue:\\t42\\n"]

boundedListOf :: (Int, Int) -> Gen a -> Gen [a]
boundedListOf (l, u) gen = do
  k <- choose (l, u)
  vectorOf k gen

arbitraryAST :: Arbitrary b => Gen (AST b)
arbitraryAST = boundedListOf (0, 6) g where
  g = frequency [(2, Left <$> arbLetDef), (1, Right <$> arbTypeDef)]

arbTypeDef :: Arbitrary b => Gen (TypeDef b)
arbTypeDef = TypeDef <$> boundedListOf (1, 2) arbTDef <*> arbitrary

arbTDef :: Arbitrary b => Gen (TDef b)
arbTDef = TDef <$> arbitraryIdentifier <*> boundedListOf (1, 2) arbConstr <*> arbitrary

arbConstr :: Arbitrary b => Gen (Constr b)
arbConstr = Constr <$> arbitraryConstrIdentifier <*> boundedListOf (0, 2) arbType <*> arbitrary

arbType :: Arbitrary b => Gen (Type b)
arbType = sized g where
  g n = Type <$> arbTypeF (resize (div n 2) arbType) <*> arbitrary

arbTypeF :: Gen t -> Gen (TypeF t)
arbTypeF r = sized gen where
  gen 0 = do
    i <- arbitraryIdentifier
    elements [UnitType, IntType, CharType, BoolType, FloatType, UserDefinedType i]
  gen n = do
    i <- choose (1, 3) :: Gen Int
    oneof [gen (div n 2), RefType <$> r, ArrayType i <$> r, FunType <$> r <*> r]

arbLetDef :: Arbitrary b => Gen (LetDef b)
arbLetDef = sized $ \n -> frequency [(3, Let <$> g n <*> arbitrary),
                                     (1, LetRec <$> g n <*> arbitrary)] where
  g n = boundedListOf (1, 2) $ resize n arbDef

arbDef :: Arbitrary b => Gen (Def b)
arbDef = sized $ \n -> frequency [(3, FunDef <$> i <*> ps <*> g n <*> arbitrary),
    (3, FunDefTyped <$> i <*> ps <*> arbType <*> g n <*> arbitrary),
    (1, VarDef <$> i <*> arbitrary),
    (1, VarDefTyped <$> i <*> arbType <*> arbitrary),
    (1, ArrayDef <$> i <*> es n <*> arbitrary),
    (1, ArrayDefTyped <$> i <*> es n <*> arbType <*> arbitrary)] where
  i = arbitraryIdentifier
  g n = resize n arbExpr
  es n = boundedListOf (1, 3) (g n)
  ps = boundedListOf (0, 3) arbParam

arbParam :: Arbitrary b => Gen (Param b)
arbParam = oneof [Param <$> arbitraryIdentifier <*> arbitrary,
              TypedParam <$> arbitraryIdentifier <*> arbType <*> arbitrary]

arbExpr :: Arbitrary b => Gen (Expr b)
arbExpr = sized g where
    g n = Expr <$> arbExprF (resize (div n 2) arbExpr) <*> arbitrary

arbExprF :: Arbitrary b => Gen e -> Gen (ExprF b e)
arbExprF r = sized gen where
  baseGens = [IntCExpr <$> arbitraryIntConstant, FloatCExpr <$> arbitraryFloatConstant,
    CharCExpr <$> arbitraryCharConstant, StringCExpr <$> arbitraryStringConstant,
    return TrueCExpr, return FalseCExpr, return UnitCExpr]
  gen 0 = oneof baseGens
  gen n = do
    unOp <- arbUnOp
    binOp <- arbBinOp
    let clauses = boundedListOf (1, 3) $ resize (div n 2) arbClause
    let letdef = resize (div n 2) arbLetDef
    oneof [gen (div n 2),
      UnOpExpr unOp <$> r,
      BinOpExpr binOp <$> r <*> r,
      ConstExpr <$> arbitraryIdentifier,
      ConstConstrExpr <$> arbitraryConstrIdentifier,
      FunAppExpr <$> arbitraryIdentifier <*> boundedListOf (1, 3) r,
      ConstrAppExpr <$> arbitraryConstrIdentifier <*> boundedListOf (1, 3) r,
      ArrayAccess <$> arbitraryIdentifier <*> boundedListOf (1, 3) r,
      ArrayDim <$> arbitraryIdentifier <*> arbitraryIntConstant,
      NewType <$> arbType,
      DeleteExpr <$> r,
      LetIn <$> letdef <*> r,
      BeginExpr <$> r,
      IfThenExpr <$> r <*> r,
      IfThenElseExpr <$> r <*> r <*> r,
      WhileExpr <$> r <*> r,
      ForExpr <$> arbitraryIdentifier <*> r <*> r <*> r,
      ForDownExpr <$> arbitraryIdentifier <*> r <*> r <*> r,
      MatchExpr <$> r <*> clauses
      ]

arbUnOp :: Gen UnOp
arbUnOp = elements [PlusUnOp, MinusUnOp, PlusFloatUnOp, MinusFloatUnOp,
                    BangOp, NotOp]

arbBinOp :: Gen BinOp
arbBinOp = elements [PlusOp, MinusOp, TimesOp, DivOp, PlusFloatOp,
                      MinusFloatOp, TimesFloatOp, DivFloatOp, ModOp, ExpOp,
                      EqOp, NotEqOp, LTOp, GTOp, LEqOp, GEqOp,
                      NatEqOp, NotNatEqOp, AndOp, OrOp, SemicolonOp, AssignMutableOp]

arbClause :: Arbitrary b => Gen (Clause b)
arbClause = sized $ \n -> do
  p <- arbPattern
  e <- resize n arbExpr
  Match p e <$> arbitrary

arbPattern :: Arbitrary b => Gen (Pattern b)
arbPattern = sized g where
  g n = Pattern <$> arbPatternF (resize (div n 2) arbPattern) <*> arbitrary

arbPatternF :: Gen p -> Gen (PatternF p)
arbPatternF genP = sized gen where
  gen n = do
    sign <- elements [NoSign, Plus, Minus]
    let baseGens = [IntConstPattern sign <$> arbitraryIntConstant,
           FloatConstPattern sign <$> arbitraryFloatConstant,
           CharConstPattern <$> arbitraryCharConstant,
           return TruePattern, return FalsePattern,
           IdPattern <$> arbitraryIdentifier]
    let ps = boundedListOf (0, 3) genP
    if n == 0 then
      oneof baseGens
    else frequency [(1, ConstrPattern <$> arbitraryConstrIdentifier <*> ps), (1, oneof baseGens)]
