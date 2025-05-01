{-# LANGUAGE DeriveFunctor #-}
module Common.AST (module Common.AST) where

import Common.Token (Identifier,
                     ConstrIdentifier,
                     IntConstant,
                     FloatConstant,
                     CharConstant,
                     StringConstant)

-- Utils for nodes
class Node f where
  tag :: f b -> b

-- Definitions of all the ASTs of Llama

type AST t = [Either (LetDef t) (TypeDef t)]

-- Definitions
data LetDef b = Let [Def b] b
              | LetRec [Def b] b
  deriving (Eq, Show, Functor)
instance Node LetDef where
  tag (Let _ b)    = b
  tag (LetRec _ b) = b

data Def b = FunDef Identifier [Param b] (Expr b) b
           | FunDefTyped Identifier [Param b] (Type b) (Expr b) b
           | VarDef Identifier b
           | VarDefTyped Identifier (Type b) b
           | ArrayDef Identifier [Expr b] b
           | ArrayDefTyped Identifier [Expr b] (Type b) b
  deriving (Eq, Show, Functor)
instance Node Def where
  tag (FunDef _ _ _ b)        = b
  tag (FunDefTyped _ _ _ _ b) = b
  tag (VarDef _ b)            = b
  tag (VarDefTyped _ _ b)     = b
  tag (ArrayDef _ _ b)        = b
  tag (ArrayDefTyped _ _ _ b) = b

data Param b = Param Identifier b
             | TypedParam Identifier (Type b) b
  deriving (Eq, Show, Functor)
instance Node Param where
  tag (Param _ b)        = b
  tag (TypedParam _ _ b) = b

-- Types
data TypeDef b = TypeDef [TDef b] b
  deriving (Eq, Show, Functor)
instance Node TypeDef where
  tag (TypeDef _ b) = b

data TDef b = TDef Identifier [Constr b] b
  deriving (Eq, Show, Functor)
instance Node TDef where
  tag (TDef _ _ b) = b

data Constr b = Constr ConstrIdentifier [Type b] b
  deriving (Eq, Show, Functor)
instance Node Constr where
  tag (Constr _ _ b) = b

data Type b = Type (TypeF (Type b)) b
  deriving (Eq, Show, Functor)
instance Node Type where
  tag (Type _ b) = b

data TypeF t = UnitType | IntType | CharType | BoolType | FloatType
             | FunType t t
             | RefType t
             | ArrayType Int t
             | UserDefinedType Identifier
  deriving (Eq, Show, Functor)

-- Expressions
data Expr b = Expr (ExprF b (Expr b)) b
  deriving (Eq, Show)
instance Node Expr where
  tag (Expr _ b) = b
data ExprF b e = IntCExpr IntConstant
          | FloatCExpr FloatConstant
          | CharCExpr CharConstant
          | StringCExpr StringConstant
          | TrueCExpr
          | FalseCExpr
          | UnitCExpr
          | UnOpExpr UnOp e
          | BinOpExpr BinOp e e
          | FunAppExpr Identifier [e]
          | ConstrAppExpr ConstrIdentifier [e]
          | ArrayAccess Identifier [e]
          | ArrayDim Identifier Int
          | NewType (Type b)
          | DeleteExpr e
          | LetIn (LetDef b) e
          | BeginExpr e
          | IfThenExpr e e
          | IfThenElseExpr e e e
          | WhileExpr e e
          | ForExpr Identifier e e e
          | ForDownExpr Identifier e e e
          | MatchExpr e [Clause b]
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

data Clause b = Match (Pattern b) (Expr b) b
  deriving (Eq, Show, Functor)
instance Node Clause where
  tag (Match _ _ b) = b

data PatternSign = NoSign | Plus | Minus
  deriving (Eq, Show)

data Pattern b = Pattern (PatternF (Pattern b)) b
  deriving (Eq, Show, Functor)
instance Node Pattern where
  tag (Pattern _ b) = b

data PatternF p = IntConstPattern PatternSign IntConstant
             | FloatConstPattern PatternSign FloatConstant
             | CharConstPattern CharConstant
             | TruePattern | FalsePattern
             | IdPattern Identifier
             | ConstrPattern ConstrIdentifier [p]
  deriving (Eq, Show, Functor)

-- Utils for fmapping all tags

mapAST :: (a -> b) -> AST a -> AST b
mapAST f = map g where
  g (Left l)  = Left (fmap f l)
  g (Right t) = Right (fmap f t)

instance Functor Expr where
  fmap f (Expr ef b) =
    let mf = fmap f
        mapEf = case ef of
          IntCExpr c           -> IntCExpr c
          FloatCExpr c         -> FloatCExpr c
          CharCExpr c          -> CharCExpr c
          StringCExpr c        -> StringCExpr c
          TrueCExpr            -> TrueCExpr
          FalseCExpr           -> FalseCExpr
          UnitCExpr            -> UnitCExpr
          ArrayDim i c         -> ArrayDim i c
          UnOpExpr op e        -> UnOpExpr op (mf e)
          BinOpExpr op u v     -> BinOpExpr op (mf u) (mf v)
          FunAppExpr i le      -> FunAppExpr i (fmap mf le)
          ConstrAppExpr i le   -> ConstrAppExpr i (fmap mf le)
          ArrayAccess i le     -> ArrayAccess i (fmap mf le)
          NewType t            -> NewType (fmap f t)
          DeleteExpr e         -> DeleteExpr (mf e)
          LetIn l e            -> LetIn (fmap f l) (mf e)
          BeginExpr e          -> BeginExpr (mf e)
          IfThenExpr u v       -> IfThenExpr (mf u) (mf v)
          IfThenElseExpr u v w -> IfThenElseExpr (mf u) (mf v) (mf w)
          WhileExpr u v        -> WhileExpr (mf u) (mf v)
          ForExpr i u v w      -> ForExpr i (mf u) (mf v) (mf w)
          ForDownExpr i u v w  -> ForDownExpr i (mf u) (mf v) (mf w)
          MatchExpr e lc       -> MatchExpr (mf e) (fmap (fmap f) lc)
    in Expr mapEf (f b)

bottomUp :: (Expr b -> Expr b) -> Expr b -> Expr b
bottomUp f (Expr ef b) = f $ Expr (fmap f ef) b

unPackBegin :: Expr b -> Expr b
unPackBegin (Expr (BeginExpr e) _) = e
unPackBegin e = e