module Property.Parser.ArbitraryAST (arbitraryAST, ArbPosn(..)) where

import Common.AST
import Lexer.Lexer (AlexPosn(..))
import Test.QuickCheck
import Property.Utils
import Property.Lexer.ArbitraryTokens

-- This module defines a generator for syntactically correct
-- programs.

newtype ArbPosn = ArbPosn {arb_posn :: AlexPosn}
  deriving Show

instance Arbitrary ArbPosn where
   arbitrary = ArbPosn <$> (AlexPn <$> arbitrary <*> arbitrary <*> arbitrary)

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
arbType = sized $ \n -> Type <$> arbTypeF (resize (div n 2) arbType) <*> arbitrary

arbTypeF :: Gen t -> Gen (TypeF t)
arbTypeF r = sized gen where
  gen 0 = do
    i <- arbitraryIdentifier
    elements [UnitType, IntType, CharType, BoolType, FloatType, UserDefinedType i]
  gen n = do
    i <- choose (1, 3) :: Gen Int
    oneof [gen (div n 2), RefType <$> r, ArrayType i <$> r, FunType <$> r <*> r]

arbLetDef :: Arbitrary b => Gen (LetDef b)
arbLetDef = frequency [(3, Let <$> defs <*> arbitrary),
                       (1, LetRec <$> defs <*> arbitrary)] where
  defs = boundedListOf (1, 2) arbDef

arbDef :: Arbitrary b => Gen (Def b)
arbDef = frequency [(3, FunDef <$> i <*> ps <*> arbExpr <*> arbitrary),
    (3, FunDefTyped <$> i <*> ps <*> arbType <*> arbExpr <*> arbitrary),
    (1, VarDef <$> i <*> arbitrary),
    (1, VarDefTyped <$> i <*> arbType <*> arbitrary),
    (1, ArrayDef <$> i <*> es <*> arbitrary),
    (1, ArrayDefTyped <$> i <*> es <*> arbType <*> arbitrary)] where
  i = arbitraryIdentifier
  es = boundedListOf (1, 3) arbExpr
  ps = boundedListOf (0, 3) arbParam

arbParam :: Arbitrary b => Gen (Param b)
arbParam = oneof [Param <$> arbitraryIdentifier <*> arbitrary,
              TypedParam <$> arbitraryIdentifier <*> arbType <*> arbitrary]

arbExpr :: Arbitrary b => Gen (Expr b)
arbExpr = sized g where
    g 0 = Expr <$> arbExprF (resize 0 arbExpr) <*> arbitrary
    g n =
      let r = resize (div n 2) arbExpr
          clauses = boundedListOf (1, 3) $ resize (div n 2) arbClause
          letdef = resize (div n 2) arbLetDef
      in frequency [(3, Expr <$> arbExprF r <*> arbitrary),
                    (1, NewType <$> arbType <*> arbitrary),
                    (1, LetIn <$> letdef <*> r <*> arbitrary),
                    (1, MatchExpr <$> r <*> clauses <*> arbitrary)
                  ]

arbExprF :: Gen e -> Gen (ExprF e)
arbExprF r = sized gen where
  baseGens = [IntCExpr <$> arbitraryIntConstant, FloatCExpr <$> arbitraryFloatConstant,
    CharCExpr <$> arbitraryCharConstant, StringCExpr <$> arbitraryStringConstant,
    return TrueCExpr, return FalseCExpr, return UnitCExpr]
  gen 0 = oneof baseGens
  gen n = do
    unOp <- arbUnOp
    binOp <- arbBinOp
    oneof [gen (div n 2),
      UnOpExpr unOp <$> r,
      BinOpExpr binOp <$> r <*> r,
      ConstExpr <$> arbitraryIdentifier,
      ConstConstrExpr <$> arbitraryConstrIdentifier,
      FunAppExpr <$> arbitraryIdentifier <*> boundedListOf (1, 3) r,
      ConstrAppExpr <$> arbitraryConstrIdentifier <*> boundedListOf (1, 3) r,
      ArrayAccess <$> arbitraryIdentifier <*> boundedListOf (1, 3) r,
      ArrayDim <$> arbitraryIdentifier <*> arbitraryIntConstant,
      DeleteExpr <$> r,
      BeginExpr <$> r,
      IfThenExpr <$> r <*> r,
      IfThenElseExpr <$> r <*> r <*> r,
      WhileExpr <$> r <*> r,
      ForExpr <$> arbitraryIdentifier <*> r <*> r <*> r,
      ForDownExpr <$> arbitraryIdentifier <*> r <*> r <*> r
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
arbClause = do
  p <- arbPattern
  e <- arbExpr
  Match p e <$> arbitrary

arbPattern :: Arbitrary b => Gen (Pattern b)
arbPattern = sized $ \n -> Pattern <$> arbPatternF (resize (div n 2) arbPattern) <*> arbitrary

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
