{-# LANGUAGE DeriveGeneric #-}

module Interpreter.Interpreter (module Interpreter.Interpreter) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData, deepseq)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Map
import Common.Token
import Common.AST
import Common.PrintAST

data Value =
    IntVal IntConstant
  | FloatVal FloatConstant
  | CharVal CharConstant
  | StringVal StringConstant
  | TrueVal | FalseVal | UnitVal
  | IdVal Identifier
  | ConstrVal ConstrIdentifier
  deriving (Eq, Show, Generic)

instance NFData Value

instance Ord Value where
  compare (IntVal u) (IntVal v)     = compare u v
  compare (FloatVal u) (FloatVal v) = compare u v
  compare (CharVal u) (CharVal v)   = compare u v
  compare _ _                       = error "Non-comparable values"

boolToVal :: Bool -> Value
boolToVal True  = TrueVal
boolToVal False = FalseVal

data Computable e a = Computable { eval :: e -> a }

instance Functor (Computable e) where
  fmap = liftM

instance Applicative (Computable e) where
  pure a = Computable $ const a
  (<*>) = ap

instance Monad (Computable e) where
  return = pure
  a >>= f  = Computable $ \e -> eval (f (eval a e)) e

type FunContext    = Map (Identifier, [Identifier]) Expr
type VarContext    = Map Identifier Expr
type ArrayContext  = Map (Identifier, Int) Expr

data Context = Context {
    ids :: FunContext,
    constrs :: FunContext,
    vars :: VarContext,
    arrays :: ArrayContext
  }

emptyCtx :: Context
emptyCtx = Context { ids = empty, constrs = empty, vars = empty, arrays = empty }

getContext :: State Context Context
getContext = state $ \s -> (s, s)

putContext :: Context -> State Context ()
putContext c = state $ \_ -> ((), c)

hoist :: Monad m => Maybe b -> MaybeT m b
hoist = hoistMaybe

interpreter :: Expr -> MaybeT (State Context) Value
interpreter e = case e of
  IntCExpr c    -> return $ IntVal c
  FloatCExpr c  -> return $ FloatVal c
  CharCExpr c   -> return $ CharVal c
  StringCExpr c -> return $ StringVal c
  TrueCExpr     -> return TrueVal
  FalseCExpr    -> return FalseVal
  UnitCExpr     -> return UnitVal
  UnOpExpr op u -> evalUnOpExpr op u
  _             -> hoist Nothing

evalUnOpExpr :: UnOp -> Expr -> MaybeT (State Context) Value
evalUnOpExpr op u = do
  r <- interpreter u
  case (op, r) of
    (PlusUnOp, IntVal v)         -> return $ IntVal v
    (MinusUnOp, IntVal v)        -> return $ IntVal $ -(v)
    (PlusFloatUnOp, FloatVal v)  -> return $ FloatVal v
    (MinusFloatUnOp, FloatVal v) -> return $ FloatVal $ -(v)
    (BangOp, IdVal x)            -> do
      ctx <- lift getContext :: MaybeT (State Context) Context
      e <- hoist $ Data.Map.lookup x (vars ctx)
      interpreter e
    (NotOp, TrueVal)             -> return FalseVal
    (NotOp, FalseVal)            -> return TrueVal
    _                            -> hoist Nothing

evalBinOpExpr :: BinOp -> Expr -> Expr -> MaybeT (State Context) Value
evalBinOpExpr op u v = do
  s <- interpreter u
  t <- interpreter v
  case (op, s, t) of
    (PlusOp, IntVal v1, IntVal v2) -> return $ IntVal (v1 + v2)
    (MinusOp, IntVal v1, IntVal v2) -> return $ IntVal (v1 - v2)
    (TimesOp, IntVal v1, IntVal v2) -> return $ IntVal (v1 * v2)
    (DivOp, IntVal v1, IntVal v2) -> return $ IntVal (div v1 v2)
    (PlusFloatOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 + v2)
    (MinusFloatOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 - v2)
    (TimesFloatOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 * v2)
    (DivFloatOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 / v2)
    (ModOp, IntVal v1, IntVal v2) -> return $ IntVal (mod v1 v2)
    (ExpOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 ** v2)
    (AndOp, v1, v2) -> return $ andValues v1 v2 where
      andValues :: Value -> Value -> Value
      andValues FalseVal _ = FalseVal
      andValues TrueVal TrueVal = TrueVal
      andValues TrueVal FalseVal = FalseVal
      andValues _ _ = error "???"
    (OrOp, v1, v2) -> return $ orValues v1 v2 where
      orValues :: Value -> Value -> Value
      orValues TrueVal _ = TrueVal
      orValues FalseVal TrueVal = TrueVal
      orValues FalseVal FalseVal = FalseVal
      orValues _ _ = error "???"
    -- (EqOp, IdVal x, _) ->
    -- (NotEqOp, ???, ???)
    (LTOp, v1, v2) -> return $ boolToVal (v1 < v2)
    (GTOp, v1, v2) -> return $ boolToVal (v1 > v2)
    (LEqOp, v1, v2) -> return $ boolToVal (v1 <= v2)
    (GEqOp, v1, v2) -> return $ boolToVal (v1 >= v2)
    -- (NatEqOp, ???, ???)
    -- (NotNatEqOp, ???, ???)
    (SemicolonOp, v1, v2) -> return (deepseq v1 v2) -- compute evalExpr (BinOpExpr SemicolonOp (BinOpExpr DivOp (IntCExpr 42) (IntCExpr 0)) (IntCExpr 17))
    (AssignMutableOp, IdVal x, _) -> do
      ctx <- lift getContext :: MaybeT (State Context) Context
      lift . putContext $ ctx{vars = insert x v (vars ctx)}
      return UnitVal
    _ -> hoist Nothing

{-
compute :: (Computable e a) -> e -> a
compute = eval

getComputation :: (Computable e a) -> e -> e
getComputation _ e = e

get :: Computable e e
get = Computable id

put :: Computable e a -> e -> Computable e a
put c e = do
  return $ compute c e

evalExpr :: Computable Expr Value
evalExpr =
  let comp :: Expr -> Computable Expr Value
      comp = put evalExpr
  in do
    e <- get
    case e of
      IntCExpr c    -> return $ IntVal c
      FloatCExpr c  -> return $ FloatVal c
      CharCExpr c   -> return $ CharVal c
      StringCExpr c -> return $ StringVal c
      TrueCExpr     -> return TrueVal
      FalseCExpr    -> return FalseVal
      UnitCExpr     -> return UnitVal
      UnOpExpr op u -> evalUnOpExpr op u
      BinOpExpr op u v
                    -> evalBinOpExpr op u v
      -- FunAppExpr f ps -> do
      --   inp <- forM comp ps
      --   context ???
      --   return expression
      IfThenExpr u v -> do
        cond <- comp u
        if cond == TrueVal
        then comp v
        else return UnitVal

evalUnOpExpr :: UnOp -> Expr -> Computable Expr Value
evalUnOpExpr op u = do
  r <- put evalExpr u
  let unOpErr unOp expr = error $ "Cannot apply unary operator '" ++ (show unOp) ++ "' to value: " ++ (pretty expr)
  case (op, r) of
    (PlusUnOp, IntVal v)         -> return $ IntVal v
    (MinusUnOp, IntVal v)        -> return $ IntVal $ -(v)
    (PlusFloatUnOp, FloatVal v)  -> return $ FloatVal v
    (MinusFloatUnOp, FloatVal v) -> return $ FloatVal $ -(v)
    -- (BangOp, ???) -> return $ IntVal v -- TODO: implement this
    (NotOp, TrueVal)             -> return FalseVal
    (NotOp, FalseVal)            -> return TrueVal
    _                            -> unOpErr op u

evalBinOpExpr :: BinOp -> Expr -> Expr -> Computable Expr Value
evalBinOpExpr op u v = do
  s <- put evalExpr u
  t <- put evalExpr v
  case (op, s, t) of
    (PlusOp, IntVal v1, IntVal v2) -> return $ IntVal (v1 + v2)
    (MinusOp, IntVal v1, IntVal v2) -> return $ IntVal (v1 - v2)
    (TimesOp, IntVal v1, IntVal v2) -> return $ IntVal (v1 * v2)
    (DivOp, IntVal v1, IntVal v2) -> return $ IntVal (div v1 v2)
    (PlusFloatOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 + v2)
    (MinusFloatOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 - v2)
    (TimesFloatOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 * v2)
    (DivFloatOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 / v2)
    (ModOp, IntVal v1, IntVal v2) -> return $ IntVal (mod v1 v2)
    (ExpOp, FloatVal v1, FloatVal v2) -> return $ FloatVal (v1 ** v2)
    (AndOp, v1, v2) -> return $ andValues v1 v2 where
      andValues :: Value -> Value -> Value
      andValues FalseVal _ = FalseVal
      andValues TrueVal TrueVal = TrueVal
      andValues TrueVal FalseVal = FalseVal
      andValues _ _ = error "???"
    (OrOp, v1, v2) -> return $ orValues v1 v2 where
      orValues :: Value -> Value -> Value
      orValues TrueVal _ = TrueVal
      orValues FalseVal TrueVal = TrueVal
      orValues FalseVal FalseVal = FalseVal
      orValues _ _ = error "???"
    -- (EqOp, ???, ???)
    -- (NotEqOp, ???, ???)
    (LTOp, v1, v2) -> return $ boolToVal (v1 < v2)
    (GTOp, v1, v2) -> return $ boolToVal (v1 > v2)
    (LEqOp, v1, v2) -> return $ boolToVal (v1 <= v2)
    (GEqOp, v1, v2) -> return $ boolToVal (v1 >= v2)
    -- (NatEqOp, ???, ???)
    -- (NotNatEqOp, ???, ???)
    (SemicolonOp, v1, v2) -> return (deepseq v1 v2) -- compute evalExpr (BinOpExpr SemicolonOp (BinOpExpr DivOp (IntCExpr 42) (IntCExpr 0)) (IntCExpr 17))
    -- (AssignMutableOp, ???, ???)
-}
