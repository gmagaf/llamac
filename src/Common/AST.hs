{-# LANGUAGE DeriveFunctor #-}
module Common.AST (module Common.AST) where

import Common.Token (Identifier,
                     ConstrIdentifier,
                     IntConstant,
                     FloatConstant,
                     CharConstant,
                     StringConstant)

-- Definitions of all the ASTs of Llama

type AST t = [Either (LetDefTag t) (TypeDefTag t)]

mapAST :: (a -> b) -> AST a -> AST b
mapAST f = map g where
  g (Left (l, a))  = Left (fmap f l, f a)
  g (Right (t, a)) = Right (fmap f t, f a)
-- Definitions
type LetDefTag t = (LetDef t, t)
data LetDef t = Let [DefTag t]
              | LetRec [DefTag t]
  deriving (Eq, Show, Functor)

type DefTag t = (Def t, t)
data Def t = FunDef Identifier [ParamTag t] (ExprTag t)
           | FunDefTyped Identifier [ParamTag t] (TypeTag t) (ExprTag t)
           | VarDef Identifier
           | VarDefTyped Identifier (TypeTag t)
           | ArrayDef Identifier [ExprTag t]
           | ArrayDefTyped Identifier [ExprTag t] (TypeTag t)
  deriving (Eq, Show, Functor)

type ParamTag t = (Param t, t)
data Param t = Param Identifier
             | TypedParam Identifier (TypeTag t)
  deriving (Eq, Show, Functor)

-- Types
type TypeDefTag t = (TypeDef t, t)
data TypeDef t = TypeDef [TDefTag t]
  deriving (Eq, Show, Functor)

type TDefTag t = (TDef t, t)
data TDef t = TDef Identifier [ConstrTag t]
  deriving (Eq, Show, Functor)

type ConstrTag t = (Constr t, t)
data Constr t = Constr ConstrIdentifier [TypeTag t]
  deriving (Eq, Show, Functor)

type TypeTag t = (Type t, t)
data Type t = UnitType | IntType | CharType | BoolType | FloatType
            | FunType (TypeTag t) (TypeTag t)
            | RefType (TypeTag t)
            | ArrayType Int (TypeTag t)
            | UserDefinedType Identifier
  deriving (Eq, Show, Functor)

-- Expressions
type ExprTag t = (Expr t, t)
data Expr t = IntCExpr IntConstant
          | FloatCExpr FloatConstant
          | CharCExpr CharConstant
          | StringCExpr StringConstant
          | TrueCExpr
          | FalseCExpr
          | UnitCExpr
          | UnOpExpr UnOp (ExprTag t)
          | BinOpExpr BinOp (ExprTag t) (ExprTag t)
          | FunAppExpr Identifier [ExprTag t]
          | ConstrAppExpr ConstrIdentifier [ExprTag t]
          | ArrayAccess Identifier [ExprTag t]
          | ArrayDim Identifier Int
          | NewType (TypeTag t)
          | DeleteExpr (ExprTag t)
          | LetIn (LetDefTag t) (ExprTag t)
          | BeginExpr (ExprTag t)
          | IfThenExpr (ExprTag t) (ExprTag t)
          | IfThenElseExpr (ExprTag t) (ExprTag t) (ExprTag t)
          | WhileExpr (ExprTag t) (ExprTag t)
          | ForExpr Identifier (ExprTag t) (ExprTag t) (ExprTag t)
          | ForDownExpr Identifier (ExprTag t) (ExprTag t) (ExprTag t)
          | MatchExpr (ExprTag t) [ClauseTag t]
  deriving (Eq, Show, Functor)

data UnOp = PlusUnOp | MinusUnOp
          | PlusFloatUnOp | MinusFloatUnOp
          | BangOp | NotOp
  deriving (Eq, Show)

data BinOp = PlusOp | MinusOp | TimesOp | DivOp
           | PlusFloatOp | MinusFloatOp | TimesFloatOp | DivFloatOp
           | ModOp | ExpOp
           | EqOp | NotEqOp
           | LTOp | GTOp | LEqOp | GEqOp
           | NatEqOp | NotNatEqOp
           | AndOp | OrOp
           | SemicolonOp | AssignMutableOp
  deriving (Eq, Show)

type ClauseTag t = (Clause t, t)
data Clause t = Match (PatternTag t) (ExprTag t)
  deriving (Eq, Show, Functor)

data PatternSign = NoSign | Plus | Minus
  deriving (Eq, Show)

type PatternTag t = (Pattern t, t)
data Pattern t = IntConstPattern PatternSign IntConstant
               | FloatConstPattern PatternSign FloatConstant
               | CharConstPattern CharConstant
               | TruePattern  | FalsePattern
               | IdPattern Identifier
               | ConstrPattern ConstrIdentifier [PatternTag t]
  deriving (Eq, Show, Functor)
