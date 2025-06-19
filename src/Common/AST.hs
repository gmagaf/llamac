{-# LANGUAGE DeriveTraversable #-}
module Common.AST (module Common.AST) where

import Common.Token (Identifier,
                     ConstrIdentifier,
                     IntConstant,
                     FloatConstant,
                     CharConstant,
                     StringConstant)

-- Utils for nodes
class Traversable n => Node n where
  tag :: n b -> b
  mapTag :: (a -> a) -> n a -> n a

class Node n => NameDef n where
  ide :: n b -> String

-- Definitions of all the ASTs of Llama

type AST t = [Either (LetDef t) (TypeDef t)]

-- This is useful for repl
data ProgramOrExpr t = Program (AST t)
                     | Expression (Expr t)
  deriving (Eq, Show)

-- Definitions
data LetDef b = Let [Def b] b
              | LetRec [Def b] b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Def b = FunDef Identifier [Param b] (Expr b) b
           | FunDefTyped Identifier [Param b] (Type b) (Expr b) b
           | VarDef Identifier b
           | VarDefTyped Identifier (Type b) b
           | ArrayDef Identifier [Expr b] b
           | ArrayDefTyped Identifier [Expr b] (Type b) b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Param b = Param Identifier b
             | TypedParam Identifier (Type b) b
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Types
data TypeDef b = TypeDef [TDef b] b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TDef b = TDef Identifier [Constr b] b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Constr b = Constr ConstrIdentifier [Type b] b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Type b = Type (TypeF (Type b)) b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TypeF t = UnitType | IntType | CharType | BoolType | FloatType
             | FunType t t
             | RefType t
             | ArrayType Int t
             | UserDefinedType Identifier
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Expressions
data Expr b = Expr (ExprF (Expr b)) b
            | NewType (Type b) b
            | LetIn (LetDef b) (Expr b) b
            | MatchExpr (Expr b) [Clause b] b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data ExprF e = IntCExpr IntConstant
          | FloatCExpr FloatConstant
          | CharCExpr CharConstant
          | StringCExpr StringConstant
          | TrueCExpr
          | FalseCExpr
          | UnitCExpr
          | ConstExpr Identifier
          | ConstConstrExpr ConstrIdentifier
          | UnOpExpr UnOp e
          | BinOpExpr BinOp e e
          | FunAppExpr Identifier [e]
          | ConstrAppExpr ConstrIdentifier [e]
          | ArrayAccess Identifier [e]
          | ArrayDim Identifier Int
          | DeleteExpr e
          | BeginExpr e
          | IfThenExpr e e
          | IfThenElseExpr e e e
          | WhileExpr e e
          | ForExpr Identifier e e e
          | ForDownExpr Identifier e e e
  deriving (Eq, Show, Functor, Foldable, Traversable)

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

data Clause b = Match (Pattern b) (Expr b) b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data PatternSign = NoSign | Plus | Minus
  deriving (Eq, Show)

data Pattern b = Pattern (PatternF (Pattern b)) b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data PatternF p = IntConstPattern PatternSign IntConstant
             | FloatConstPattern PatternSign FloatConstant
             | CharConstPattern CharConstant
             | TruePattern | FalsePattern
             | IdPattern Identifier
             | ConstrPattern ConstrIdentifier [p]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Utils for fmapping all tags

mapAST :: (a -> b) -> AST a -> AST b
mapAST f = map g where
  g (Left l)  = Left (fmap f l)
  g (Right t) = Right (fmap f t)

-- Instantiations of the node classes
instance Node LetDef where
  tag (Let _ b)    = b
  tag (LetRec _ b) = b
  mapTag f (Let defs b)    = Let defs (f b)
  mapTag f (LetRec defs b) = LetRec defs (f b)
instance Node Def where
  tag (FunDef _ _ _ b)        = b
  tag (FunDefTyped _ _ _ _ b) = b
  tag (VarDef _ b)            = b
  tag (VarDefTyped _ _ b)     = b
  tag (ArrayDef _ _ b)        = b
  tag (ArrayDefTyped _ _ _ b) = b
  mapTag f (FunDef i p e b)        = FunDef i p e (f b)
  mapTag f (FunDefTyped i p e t b) = FunDefTyped i p e t (f b)
  mapTag f (VarDef i b)            = VarDef i (f b)
  mapTag f (VarDefTyped i t b)     = VarDefTyped i t (f b)
  mapTag f (ArrayDef i e b)        = ArrayDef i e (f b)
  mapTag f (ArrayDefTyped i e t b) = ArrayDefTyped i e t (f b)
instance Node Param where
  tag (Param _ b)        = b
  tag (TypedParam _ _ b) = b
  mapTag f (Param i b)        = Param i (f b)
  mapTag f (TypedParam i t b) = TypedParam i t (f b)
instance Node TypeDef where
  tag (TypeDef _ b) = b
  mapTag f (TypeDef tDef b) = TypeDef tDef (f b)
instance Node TDef where
  tag (TDef _ _ b) = b
  mapTag f (TDef i cs b) = TDef i cs (f b)
instance Node Constr where
  tag (Constr _ _ b) = b
  mapTag f (Constr i ts b) = Constr i ts (f b)
instance Node Type where
  tag (Type _ b) = b
  mapTag f (Type tf b) = Type tf (f b)
instance Node Expr where
  tag (Expr _ b) = b
  tag (NewType _ b) = b
  tag (LetIn _ _ b) = b
  tag (MatchExpr _ _ b) = b
  mapTag f (Expr ef b) = Expr ef (f b)
  mapTag f (NewType e b) = NewType e (f b)
  mapTag f (LetIn l e b) = LetIn l e (f b)
  mapTag f (MatchExpr e cs b) = MatchExpr e cs (f b)

instance Node Clause where
  tag (Match _ _ b) = b
  mapTag f (Match p e b) = Match p e (f b)
instance Node Pattern where
  tag (Pattern _ b) = b
  mapTag f (Pattern pf b) = Pattern pf (f b)

instance NameDef Def where
  ide (FunDef i _ _ _)        = i
  ide (FunDefTyped i _ _ _ _) = i
  ide (VarDef i _)            = i
  ide (VarDefTyped i _ _)     = i
  ide (ArrayDef i _ _)        = i
  ide (ArrayDefTyped i _ _ _) = i
instance NameDef Param where
  ide (Param i _)        = i
  ide (TypedParam i _ _) = i
instance NameDef TDef where
  ide (TDef i _ _) = i
instance NameDef Constr where
  ide (Constr i _ _) = i
