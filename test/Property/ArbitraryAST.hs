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
arbitraryCharConstant = elements ["a", "7", "\n", "\"", "\xE9"]

arbitraryStringConstant :: Gen StringConstant
arbitraryStringConstant = elements ["foo", "bar", "Route66", "Name:\t\"DouglasAdams\"\nValue:\t42\n"]

listOf :: Gen a -> Gen [a]
listOf gen = sized $ \n ->
  do k <- choose (0, n)
     vectorOf k gen

boundedListOf :: (Int, Int) -> Gen a -> Gen [a]
boundedListOf (l, u) gen = do
  k <- choose (l, u)
  vectorOf k gen

instance Arbitrary Type where
  arbitrary = sized gen where
    gen 0 = do
      i <- arbitraryIdentifier
      oneof $ map return [UnitType, IntType, CharType, BoolType, FloatType,
        UserDefinedType i]
    gen n = do
      i <- choose (1, 3) :: Gen Int
      let r = gen (div n 2)
      oneof [r, RefType <$> r, ArrayType i <$> r, FunType <$> r <*> r]

instance Arbitrary Constr where
  arbitrary = Constr <$> arbitraryConstrIdentifier <*> (boundedListOf (1, 1) arbitrary)

instance Arbitrary TDef where
  arbitrary = TDef <$> arbitraryIdentifier <*> (boundedListOf (1,1) arbitrary)

instance Arbitrary TypeDef where
  arbitrary = Type <$> (boundedListOf (1, 3) arbitrary)

arbitraryProgram :: Gen Program
arbitraryProgram = do
    l <- boundedListOf (1, 4) (arbitrary :: Gen TypeDef)
    let r = map Right l
    return r


-- instance Arbitrary LetDef where
--   arbitrary = do
--     a <- arbitrary
--     b <- arbitrary
--     elements [Let a, LetRec b]
--
-- instance Arbitrary Def where
--   definitions

-- instance Arbitrary TypeDef where
--   arbitrary = undefined
--
--
-- instance Arbitrary Expr where
--   arbitrary = do
--     int <- arbitraryIntConstant
--     float <- arbitraryFloatConstant
--     char <- arbitraryCharConstant
--     string <- arbitraryStringConstant
--     expr <- arbitrary
--     unOp <- arbitrary
--     binOp <- arbitrary
--     identifier <- arbitraryIdentifier
--     exprs <- arbitraryList
--     constr <- arbitraryConstrIdentifier
--     -- t <- arbitrary
--     clauses <- arbitraryList
--     elements [IntCExpr int, FloatCExpr float, CharCExpr char, StringCExpr string,
--               TrueCExpr, FalseCExpr, UnitCExpr, NestedExpr expr, UnOpExpr unOp expr,
--               BinOpExpr binOp expr expr, FunAppExpr identifier exprs,
--               ConstrAppExpr constr exprs, ArrayAccess identifier exprs,
--               ArrayDim identifier, ArrayDimMult identifier int,
--               -- NewType t, DeleteExpr expr, LetIn LetDef expr, BeginExpr expr,
--               IfThenExpr expr expr, IfThenElseExpr expr expr expr,
--               WhileExpr expr expr, ForExpr identifier expr expr expr,
--               ForDownExpr identifier expr expr expr, MatchExpr expr clauses]
--
-- instance Arbitrary UnOp where
--   arbitrary = elements [PlusUnOp, MinusUnOp, PlusFloatUnOp, MinusFloatUnOp,
--                         BangOp, NotOp]
--
-- instance Arbitrary BinOp where
--   arbitrary = elements [PlusOp, MinusOp, TimesOp, DivOp, PlusFloatOp,
--                         MinusFloatOp, TimesFloatOp, DivFloatOp, ModOp, ExpOp,
--                         AssignOp, NotStructEqOp, LTOp, GTOp, LEqOp, GEqOp,
--                         NatEqOp, NotNatEqOp, AndOp, OrOp, SemicolonOp, AssignMutableOp]
--
-- instance Arbitrary Clause where
--   arbitrary = do
--     p <- arbitrary
--     e <- arbitrary
--     return (Match p e)
--
-- instance Arbitrary Pattern where
--   arbitrary = do
--     int <- arbitraryIntConstant
--     float <- arbitraryFloatConstant
--     char <- arbitraryCharConstant
--     i <- arbitraryIdentifier
--     p <- arbitrary
--     constr <- arbitraryConstrIdentifier
--     ps <- arbitraryList
--     elements [IntConstPattern int, PlusIntConstPattern int, MinusIntConstPattern int,
--       FloatConstPattern float, PlusFloatConstPattern float, MinusFloatConstPattern float,
--       CharConstPattern char, TruePattern, FalsePattern, IdPattern i,
--       ConstrPattern constr ps]
