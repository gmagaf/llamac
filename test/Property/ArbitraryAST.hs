module Property.ArbitraryAST (arbitraryProgram) where

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

arbitraryProgram :: Gen Program
arbitraryProgram = boundedListOf (0, 6) g where
  g = frequency [(2, Left <$> arbitrary), (1, Right <$> arbitrary)]

instance Arbitrary TypeDef where
  arbitrary = Type <$> (boundedListOf (1, 2) arbitrary)

instance Arbitrary TDef where
  arbitrary = TDef <$> arbitraryIdentifier <*> (boundedListOf (1, 2) arbitrary)

instance Arbitrary Constr where
  arbitrary = Constr <$> arbitraryConstrIdentifier <*> (boundedListOf (0, 2) arbitrary)

instance Arbitrary Type where
  arbitrary = sized gen where
    gen 0 = do
      i <- arbitraryIdentifier
      elements [UnitType, IntType, CharType, BoolType, FloatType, UserDefinedType i]
    gen n = do
      i <- choose (1, 3) :: Gen Int
      let r = gen (div n 2)
      oneof [r, RefType <$> r, ArrayType i <$> r, FunType <$> r <*> r]

instance Arbitrary LetDef where
  arbitrary = frequency [(3, Let <$> g), (1, LetRec <$> g)] where
    g = boundedListOf (1, 2) arbitrary

instance Arbitrary Def where
  arbitrary = frequency [(3, FunDef <$> i <*> ps <*> arbitrary),
      (3, FunDefTyped <$> i <*> ps <*> arbitrary <*> arbitrary),
      (1, VarDef <$> i), (1, VarDefTyped <$> i <*> arbitrary),
      (1, ArrayDef <$> i <*> es), (1, ArrayDefTyped <$> i <*> es <*> arbitrary)] where
    i = arbitraryIdentifier
    ps = boundedListOf (0, 3) arbitrary
    es = boundedListOf (1, 3) arbitrary

instance Arbitrary Param where
  arbitrary = oneof [Param <$> arbitraryIdentifier,
                TypedParam <$> arbitraryIdentifier <*> arbitrary]

-- TODO
instance Arbitrary Expr where
  arbitrary = sized gen where
    baseGens = [IntCExpr <$> arbitraryIntConstant, FloatCExpr <$> arbitraryFloatConstant,
      CharCExpr <$> arbitraryCharConstant, StringCExpr <$> arbitraryStringConstant,
      return TrueCExpr, return FalseCExpr, return UnitCExpr]
    gen 0 = oneof baseGens
    gen n = do
      let r = gen (div n 2)
      -- unOp <- arbitrary
      -- binOp <- arbitrary
      oneof [r,
              -- UnOpExpr unOp <$> r,
              -- BinOpExpr binOp expr expr, FunAppExpr identifier exprs,
              -- ConstrAppExpr constr exprs, ArrayAccess identifier exprs,
              -- ArrayDim identifier, ArrayDimMult identifier int,
              -- NewType t, DeleteExpr expr, LetIn LetDef expr, BeginExpr expr,
              IfThenExpr <$> r <*> r, IfThenElseExpr <$> r <*> r <*> r
              -- WhileExpr <$> r <*> r,
              -- ForExpr <$> arbitraryIdentifier <*> r <*> r <*> r,
              -- ForDownExpr <$> arbitraryIdentifier <*> r <*> r <*> r,
              -- MatchExpr <$> r <*> arbitrary
            ]

instance Arbitrary UnOp where
  arbitrary = elements [PlusUnOp, MinusUnOp, PlusFloatUnOp, MinusFloatUnOp,
                        BangOp, NotOp]

instance Arbitrary BinOp where
  arbitrary = elements [PlusOp, MinusOp, TimesOp, DivOp, PlusFloatOp,
                        MinusFloatOp, TimesFloatOp, DivFloatOp, ModOp, ExpOp,
                        AssignOp, NotStructEqOp, LTOp, GTOp, LEqOp, GEqOp,
                        NatEqOp, NotNatEqOp, AndOp, OrOp, SemicolonOp, AssignMutableOp]

instance Arbitrary Clause where
  arbitrary = do
    p <- arbitrary
    e <- arbitrary
    return (Match p e)

instance Arbitrary Pattern where
  arbitrary = sized gen where
    gen n = do
      sign <- elements [NoSign, Plus, Minus]
      let baseGens = [IntConstPattern sign <$> arbitraryIntConstant,
             FloatConstPattern sign <$> arbitraryFloatConstant,
             CharConstPattern <$> arbitraryCharConstant,
             return TruePattern, return FalsePattern,
             IdPattern <$> arbitraryIdentifier]
      let r = gen (div n 2)
      let ps = boundedListOf (0, 3) r
      if n == 0 then
        oneof baseGens
      else frequency [(1, ConstrPattern <$> arbitraryConstrIdentifier <*> ps), (1, oneof baseGens)]
