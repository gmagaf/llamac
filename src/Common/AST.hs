{-# LANGUAGE DeriveTraversable #-}
module Common.AST (module Common.AST) where

import Data.Bifunctor (Bifunctor (..))
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bitraversable (Bitraversable (bitraverse))

import Common.DebugPrint (Debug)
import Common.Token (Identifier,
                     ConstrIdentifier,
                     IntConstant,
                     FloatConstant,
                     CharConstant,
                     StringConstant)

-- Utils for nodes
class Traversable n => Node n where
  tag :: n b -> b

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

data Def b = FunDef Identifier [Param b] (Maybe (Type b)) (Expr b) b
           | VarDef Identifier (Maybe (Type b)) b
           | ArrayDef Identifier [Expr b] (Maybe (Type b)) b
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

data Type b = Type (TypeF Identifier (Type b)) b
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TypeF i t = UnitType | IntType | CharType | BoolType | FloatType
               | FunType t t
               | RefType t
               | ArrayType Int t
               | UserDefinedType i
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor TypeF where
  first _ UnitType            = UnitType
  first _ IntType             = IntType
  first _ CharType            = CharType
  first _ BoolType            = BoolType
  first _ FloatType           = FloatType
  first _ (FunType s t)       = FunType s t
  first _ (RefType t)         = RefType t
  first _ (ArrayType d t)     = ArrayType d t
  first f (UserDefinedType i) = UserDefinedType (f i)
  second = fmap

instance Bifoldable TypeF where
  bifoldMap f _ (UserDefinedType i) = f i
  bifoldMap _ g t                   = foldMap g t

instance Bitraversable TypeF where
  bitraverse f _ (UserDefinedType i) = UserDefinedType <$> f i
  bitraverse _ _ UnitType            = pure UnitType
  bitraverse _ _ IntType             = pure IntType
  bitraverse _ _ CharType            = pure CharType
  bitraverse _ _ BoolType            = pure BoolType
  bitraverse _ _ FloatType           = pure FloatType
  bitraverse _ g (FunType s t)       = FunType <$> g s <*> g t
  bitraverse _ g (RefType t)         = RefType <$> g t
  bitraverse _ g (ArrayType d t)     = ArrayType d <$> g t

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

instance Node Def where
  tag (FunDef _ _ _ _ b)  = b
  tag (VarDef _ _ b)      = b
  tag (ArrayDef _ _ _ b)  = b

instance Node Param where
  tag (Param _ b)        = b
  tag (TypedParam _ _ b) = b

instance Node TypeDef where
  tag (TypeDef _ b) = b

instance Node TDef where
  tag (TDef _ _ b) = b

instance Node Constr where
  tag (Constr _ _ b) = b

instance Node Type where
  tag (Type _ b) = b

instance Node Expr where
  tag (Expr _ b) = b
  tag (NewType _ b) = b
  tag (LetIn _ _ b) = b
  tag (MatchExpr _ _ b) = b

instance Node Clause where
  tag (Match _ _ b) = b

instance Node Pattern where
  tag (Pattern _ b) = b

instance NameDef Def where
  ide (FunDef i _ _ _ _) = i
  ide (VarDef i _ _)     = i
  ide (ArrayDef i _ _ _) = i

instance NameDef Param where
  ide (Param i _)        = i
  ide (TypedParam i _ _) = i

instance NameDef TDef where
  ide (TDef i _ _) = i

instance NameDef Constr where
  ide (Constr i _ _) = i

-- Debugging utils
instance (Show b) => Debug (LetDef b)

instance (Show b) => Debug (TypeDef b)
