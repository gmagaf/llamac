-- module Property.ArbitraryAST (arbitraryProgram) where
module Property.ArbitraryAST where

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
  g = frequency [(2, Left <$> arbLetDef), (1, Right <$> arbTypeDef)]

arbTypeDef :: Gen TypeDef
arbTypeDef = Type <$> (boundedListOf (1, 2) arbTDef)

arbTDef :: Gen TDef
arbTDef = TDef <$> arbitraryIdentifier <*> (boundedListOf (1, 2) arbConstr)

arbConstr :: Gen Constr
arbConstr = Constr <$> arbitraryConstrIdentifier <*> (boundedListOf (0, 2) arbType)

arbType :: Gen Type
arbType = sized gen where
  gen 0 = do
    i <- arbitraryIdentifier
    elements [UnitType, IntType, CharType, BoolType, FloatType, UserDefinedType i]
  gen n = do
    i <- choose (1, 3) :: Gen Int
    let r = gen (div n 2)
    oneof [r, RefType <$> r, ArrayType i <$> r, FunType <$> r <*> r]

arbLetDef :: Gen LetDef
arbLetDef = sized $ \n -> frequency [(3, Let <$> g n), (1, LetRec <$> g n)] where
  g n = boundedListOf (1, 2) $ resize n arbDef

arbDef :: Gen Def
arbDef = sized $ \n -> frequency [(3, FunDef <$> i <*> ps <*> (g n)),
    (3, FunDefTyped <$> i <*> ps <*> arbType <*> (g n)),
    (1, VarDef <$> i), (1, VarDefTyped <$> i <*> arbType),
    (1, ArrayDef <$> i <*> (es n)), (1, ArrayDefTyped <$> i <*> (es n) <*> arbType)] where
  i = arbitraryIdentifier
  g n = resize n arbExpr
  es n = boundedListOf (1, 3) (g n)
  ps = boundedListOf (0, 3) arbParam

arbParam :: Gen Param
arbParam = oneof [Param <$> arbitraryIdentifier,
              TypedParam <$> arbitraryIdentifier <*> arbType]
-- TODO
arbExpr :: Gen Expr
arbExpr = sized gen where
  baseGens = [IntCExpr <$> arbitraryIntConstant, FloatCExpr <$> arbitraryFloatConstant,
    CharCExpr <$> arbitraryCharConstant, StringCExpr <$> arbitraryStringConstant,
    return TrueCExpr, return FalseCExpr, return UnitCExpr]
  gen 0 = oneof baseGens
  gen n = do
    let r = gen (div n 2)
    unOp <- arbUnOp
    binOp <- arbBinOp
    let clauses = boundedListOf (1, 3) $ resize (div n 2) arbClause
    let letdef = resize (div n 2) arbLetDef
    oneof [r,
      UnOpExpr unOp <$> r,
      BinOpExpr binOp <$> r <*> r,
      FunAppExpr <$> arbitraryIdentifier <*> boundedListOf (0, 3) r,
      ConstrAppExpr <$> arbitraryConstrIdentifier <*> boundedListOf (0, 3) r,
      -- ArrayAccess <$> arbitraryIdentifier <*> boundedListOf (1, 3) r,
      -- ArrayDim <$> arbitraryIdentifier <*> arbitraryIntConstant,
      -- NewType <$> arbType,
      DeleteExpr <$> r,
      LetIn <$> letdef <*> r,
      -- BeginExpr <$> r,
      IfThenExpr <$> r <*> r,
      IfThenElseExpr <$> r <*> r <*> r]
      -- WhileExpr <$> r <*> r,
      -- ForExpr <$> arbitraryIdentifier <*> r <*> r <*> r,
      -- ForDownExpr <$> arbitraryIdentifier <*> r <*> r <*> r,
      -- MatchExpr <$> r <*> clauses]

arbUnOp :: Gen UnOp
arbUnOp = elements [PlusUnOp, MinusUnOp, PlusFloatUnOp, MinusFloatUnOp,
                        BangOp, NotOp]

arbBinOp :: Gen BinOp
arbBinOp = elements [PlusOp, MinusOp, TimesOp, DivOp, PlusFloatOp,
                      MinusFloatOp, TimesFloatOp, DivFloatOp, ModOp, ExpOp,
                      AssignOp, NotStructEqOp, LTOp, GTOp, LEqOp, GEqOp,
                      NatEqOp, NotNatEqOp, AndOp, OrOp, SemicolonOp, AssignMutableOp]

arbClause :: Gen Clause
arbClause = sized $ \n -> do
  p <- arbPattern
  e <- resize n arbExpr
  return (Match p e)

arbPattern :: Gen Pattern
arbPattern = sized gen where
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
