{-# LANGUAGE DeriveTraversable #-}
module Common.AST (module Common.AST) where

import Common.Token (Identifier,
                     ConstrIdentifier,
                     IntConstant,
                     FloatConstant,
                     CharConstant,
                     StringConstant)

-- Utils for nodes
class Node n where
  tag :: n b -> b

class Node n => NameDef n where
  ide :: n b -> String

-- Definitions of all the ASTs of Llama

type AST t = [Either (LetDef t) (TypeDef t)]

-- Definitions
data LetDef b = Let [Def b] b
              | LetRec [Def b] b
  deriving (Eq, Show, Functor)

data Def b = FunDef Identifier [Param b] (Expr b) b
           | FunDefTyped Identifier [Param b] (Type b) (Expr b) b
           | VarDef Identifier b
           | VarDefTyped Identifier (Type b) b
           | ArrayDef Identifier [Expr b] b
           | ArrayDefTyped Identifier [Expr b] (Type b) b
  deriving (Eq, Show, Functor)

data Param b = Param Identifier b
             | TypedParam Identifier (Type b) b
  deriving (Eq, Show, Functor)

-- Types
data TypeDef b = TypeDef [TDef b] b
  deriving (Eq, Show, Functor)

data TDef b = TDef Identifier [Constr b] b
  deriving (Eq, Show, Functor)

data Constr b = Constr ConstrIdentifier [Type b] b
  deriving (Eq, Show, Functor)

data Type b = Type (TypeF (Type b)) b
  deriving (Eq, Show, Functor)

data TypeF t = UnitType | IntType | CharType | BoolType | FloatType
             | FunType t t
             | RefType t
             | ArrayType Int t
             | UserDefinedType Identifier
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Expressions
data Expr b = Expr (ExprF b (Expr b)) b
  deriving (Eq, Show)

instance Functor Expr where
  fmap f (Expr ef b) = Expr (fmapExprF (fmap f) (fmap f) (fmap f) (fmap f) ef) (f b)

data ExprF b e = IntCExpr IntConstant
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
  deriving (Eq, Show)

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

data PatternSign = NoSign | Plus | Minus
  deriving (Eq, Show)

data Pattern b = Pattern (PatternF (Pattern b)) b
  deriving (Eq, Show, Functor)

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

-- Implementations of fmap, foldMap, traverse, mapM for expression functor
fmapExprF :: (LetDef a -> LetDef b)
          -> (Type a -> Type b)
          -> (Clause a -> Clause b)
          -> (e1 -> e2)
          -> ExprF a e1 -> ExprF b e2
fmapExprF lf tf cf f ef = case ef of
  IntCExpr c           -> IntCExpr c
  FloatCExpr c         -> FloatCExpr c
  CharCExpr c          -> CharCExpr c
  StringCExpr c        -> StringCExpr c
  TrueCExpr            -> TrueCExpr
  FalseCExpr           -> FalseCExpr
  UnitCExpr            -> UnitCExpr
  ConstExpr i          -> ConstExpr i
  ConstConstrExpr i    -> ConstConstrExpr i
  UnOpExpr op e        -> UnOpExpr op (f e)
  BinOpExpr op u v     -> BinOpExpr op (f u) (f v)
  FunAppExpr i es      -> FunAppExpr i (fmap f es)
  ConstrAppExpr i es   -> ConstrAppExpr i (fmap f es)
  ArrayAccess i es     -> ArrayAccess i (fmap f es)
  ArrayDim i dim       -> ArrayDim i dim
  NewType t            -> NewType (tf t)
  DeleteExpr e         -> DeleteExpr (f e)
  LetIn l e            -> LetIn (lf l) (f e)
  BeginExpr e          -> BeginExpr (f e)
  IfThenExpr u v       -> IfThenExpr (f u) (f v)
  IfThenElseExpr u v w -> IfThenElseExpr (f u) (f v) (f w)
  WhileExpr u v        -> WhileExpr (f u) (f v)
  ForExpr i u v w      -> ForExpr i (f u) (f v) (f w)
  ForDownExpr i u v w  -> ForDownExpr i (f u) (f v) (f w)
  MatchExpr e cs       -> MatchExpr (f e) (fmap cf cs)

foldMapExprF :: Monoid m =>
                (LetDef b -> m)
             -> (Type b -> m)
             -> (Clause b -> m)
             -> (e -> m)
             -> ExprF b e -> m
foldMapExprF lf tf cf f ef = case ef of
  IntCExpr _           -> mempty
  FloatCExpr _         -> mempty
  CharCExpr _          -> mempty
  StringCExpr _        -> mempty
  TrueCExpr            -> mempty
  FalseCExpr           -> mempty
  UnitCExpr            -> mempty
  ConstExpr _          -> mempty
  ConstConstrExpr _    -> mempty
  UnOpExpr _ e         -> f e
  BinOpExpr _ u v      -> mappend (f u) (f v)
  FunAppExpr _ es      -> mconcat (fmap f es)
  ConstrAppExpr _ es   -> mconcat (fmap f es)
  ArrayAccess _ es     -> mconcat (fmap f es)
  ArrayDim _ _         -> mempty
  NewType t            -> tf t
  DeleteExpr e         -> f e
  LetIn l e            -> mappend (lf l) (f e)
  BeginExpr e          -> f e
  IfThenExpr u v       -> mappend (f u) (f v)
  IfThenElseExpr u v w -> mconcat (fmap f [u, v, w])
  WhileExpr u v        -> mappend (f u) (f v)
  ForExpr _ u v w      -> mconcat (fmap f [u, v, w])
  ForDownExpr _ u v w  -> mconcat (fmap f [u, v, w])
  MatchExpr e cs       -> mappend (f e) (mconcat $ fmap cf cs)

traverseExprF :: Applicative f =>
                 (LetDef b -> f (LetDef a))
              -> (Type b -> f (Type a))
              -> (Clause b -> f (Clause a))
              -> (e -> f e')
              -> ExprF b e -> f (ExprF a e')
traverseExprF lf tf cf f ef = case ef of
  IntCExpr c           -> pure $ IntCExpr c
  FloatCExpr c         -> pure $ FloatCExpr c
  CharCExpr c          -> pure $ CharCExpr c
  StringCExpr c        -> pure $ StringCExpr c
  TrueCExpr            -> pure TrueCExpr
  FalseCExpr           -> pure FalseCExpr
  UnitCExpr            -> pure UnitCExpr
  ConstExpr i          -> pure $ ConstExpr i
  ConstConstrExpr i    -> pure $ ConstConstrExpr i
  UnOpExpr op e        -> UnOpExpr op <$> f e
  BinOpExpr op u v     -> BinOpExpr op <$> f u <*> f v
  FunAppExpr i es      -> FunAppExpr i <$> traverse f es
  ConstrAppExpr i es   -> ConstrAppExpr i <$> traverse f es
  ArrayAccess i es     -> ArrayAccess i <$> traverse f es
  ArrayDim i dim       -> pure $ ArrayDim i dim
  NewType t            -> NewType <$> tf t
  DeleteExpr e         -> DeleteExpr <$> f e
  LetIn l e            -> LetIn <$> lf l <*> f e
  BeginExpr e          -> BeginExpr <$> f e
  IfThenExpr u v       -> IfThenExpr <$> f u <*> f v
  IfThenElseExpr u v w -> IfThenElseExpr <$> f u <*> f v <*> f w
  WhileExpr u v        -> WhileExpr <$> f u <*> f v
  ForExpr i u v w      -> ForExpr i <$> f u <*> f v <*> f w
  ForDownExpr i u v w  -> ForDownExpr i <$> f u <*> f v <*> f w
  MatchExpr e cs       -> MatchExpr <$> f e <*> traverse cf cs

mapMExprF :: Monad m =>
            (LetDef b -> m (LetDef a))
         -> (Type b -> m (Type a))
         -> (Clause b -> m (Clause a))
         -> (e -> m e')
         -> ExprF b e -> m (ExprF a e')
mapMExprF = traverseExprF


-- Instantiations of the node classes
instance Node LetDef where
  tag (Let _ b)    = b
  tag (LetRec _ b) = b
instance Node Def where
  tag (FunDef _ _ _ b)        = b
  tag (FunDefTyped _ _ _ _ b) = b
  tag (VarDef _ b)            = b
  tag (VarDefTyped _ _ b)     = b
  tag (ArrayDef _ _ b)        = b
  tag (ArrayDefTyped _ _ _ b) = b
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
instance Node Clause where
  tag (Match _ _ b) = b
instance Node Pattern where
  tag (Pattern _ b) = b

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
instance NameDef Constr where
  ide (Constr i _ _) = i
