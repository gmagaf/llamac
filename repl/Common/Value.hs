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
import Common.DebugPrint (Debug (debugMode), PrintConfig (..))

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
    prettyPrec p d UnitVal             = prettyPrec p d LParenT . prettyPrec p d RParenT
    prettyPrec p d (IntVal n)          = prettyPrec p d (ConstIntT n)
    prettyPrec p d (FloatVal f)        = prettyPrec p d (ConstFloatT f)
    prettyPrec p d (CharVal c)         = prettyPrec p d (ConstCharT c)
    prettyPrec p d (BoolVal True)      = prettyPrec p d TrueT
    prettyPrec p d (BoolVal False)     = prettyPrec p d FalseT
    prettyPrec p d (FunVal f _ _)      = prettyPrec p d (IdT f)
    prettyPrec p d (ConstrVal i _ as)  = showParen ((p || d > app_prec) && not (null as)) $
        prettyPrec p d (IdConstrT i) .
        showString sep . prettyPrecSepList p (app_prec + 1) " " as
        where app_prec = 5
              sep = if null as then "" else " "
    prettyPrec p d (RefVal ha r)       = prettyPrec p d (ConstIntT ha) . prettyPrec p d (IdT "@") . showsPrec d r
    prettyPrec p d Undefined           = prettyPrec p d (IdT "Undefined")
    prettyPrec p d (ArrayVal dims a _) = prettyPrec p d (ConstIntT a) . prettyPrec p d (IdT "@") .
        prettyPrec p d (IdT "Array") . prettyDims
        where prettyDims = prettyPrec p d LBracketT .
                prettyPrecSepList p d ", " (map ConstIntT dims) . prettyPrec p d RBracketT

instance Debug Value where
    debugMode _ = Left (PrintConfig { color = True, wrapParens = False })
