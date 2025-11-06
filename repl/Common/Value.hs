{-# OPTIONS_GHC -Wno-orphans #-}
module Common.Value (module Common.Value) where

import GHC.IORef
import qualified Data.Map as M

import Common.Token (ConstrIdentifier,
                    Identifier,
                    IntConstant,
                    FloatConstant,
                    CharConstant,
                    Token(..))
import Common.AST (Expr)
import Common.PrintAST (prettyPrecSepList, Pretty(prettyPrec))
import Semantics.Utils (SemanticTag)

import Common.Interpreter

-- This module contains the definitions
-- for the values of all data types

type RunTimeLibComputation = [Value] -> Interpreter Value

data FunBody = LlamaFun (Expr SemanticTag)
             | RunTimeFun RunTimeLibComputation

instance Show FunBody where
    show (LlamaFun expr) = showParen True (showString "LlamaFun " . showParen True (shows expr)) ""
    show (RunTimeFun _)  = "(RunTimeFun ...)"

data Value = UnitVal
           | IntVal IntConstant
           | FloatVal FloatConstant
           | CharVal CharConstant
           | BoolVal Bool
           | FunVal Identifier [Identifier] FunBody
           | ConstrVal ConstrIdentifier Int [Value]
           | RefVal Int (IORef Value)
           | Undefined
           | ArrayVal [Int] Int (M.Map Int (IORef Value))
    deriving Show

instance Show a => Show (IORef a) where
    show _ = "IORef"

instance Pretty Value where
    prettyPrec d UnitVal             = prettyPrec d LParenT . prettyPrec d RParenT
    prettyPrec d (IntVal n)          = prettyPrec d (ConstIntT n)
    prettyPrec d (FloatVal f)        = prettyPrec d (ConstFloatT f)
    prettyPrec d (CharVal c)         = prettyPrec d (ConstCharT c)
    prettyPrec d (BoolVal True)      = prettyPrec d TrueT
    prettyPrec d (BoolVal False)     = prettyPrec d FalseT
    prettyPrec d (FunVal f _ _)      = prettyPrec d (IdT f)
    prettyPrec d (ConstrVal i _ as)  = showParen (d > app_prec && not (null as)) $
        prettyPrec d (IdConstrT i) .
        showString sep . prettyPrecSepList (app_prec + 1) " " as
        where app_prec = 5
              sep = if null as then "" else " "
    prettyPrec d (RefVal ha r)       = prettyPrec d (ConstIntT ha) . prettyPrec d (IdT "@") . showsPrec d r
    prettyPrec d Undefined           = prettyPrec d (IdT "Undefined")
    prettyPrec d (ArrayVal dims a _) = prettyPrec d (ConstIntT a) . prettyPrec d (IdT "@") .
        prettyPrec d (IdT "Array") . prettyDims
        where prettyDims = prettyPrec d LBracketT .
                prettyPrecSepList d ", " (map ConstIntT dims) . prettyPrec d RBracketT
