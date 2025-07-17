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

-- This module contains the definitions
-- for the values of all data types
-- It also defines the errors

data RunTimeError = RunTimeError { msg :: String }
                  | ParserError { msg :: String }

instance Show RunTimeError where
    show (RunTimeError err) = "RunTimeError: " ++ err
    show (ParserError err)  = "CompileTimeError: " ++ err

type RunTimeLibComputation = [Value] -> IO (Either RunTimeError Value)

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
           | ArrayVal [Int] (M.Map Int (IORef Value))
    deriving Show

instance Show a => Show (IORef a) where
    show _ = "IORef"

instance Pretty Value where
    prettyPrec d UnitVal             = prettyPrec d T_lparen . prettyPrec d T_rparen
    prettyPrec d (IntVal n)          = prettyPrec d (T_const_int n)
    prettyPrec d (FloatVal f)        = prettyPrec d (T_const_float f)
    prettyPrec d (CharVal c)         = prettyPrec d (T_const_char c)
    prettyPrec d (BoolVal True)      = prettyPrec d T_true
    prettyPrec d (BoolVal False)     = prettyPrec d T_false
    prettyPrec d (FunVal f _ _)      = prettyPrec d (T_id f)
    prettyPrec d (ConstrVal i _ as)  = showParen (d > app_prec && not (null as)) $
        prettyPrec d (T_id_constr i) .
        showString sep . prettyPrecSepList (app_prec + 1) " " as
        where app_prec = 5
              sep = if null as then "" else " "
    prettyPrec d (RefVal ha r)       = prettyPrec d (T_const_int ha) . prettyPrec d (T_id "@") . showsPrec d r
    prettyPrec d Undefined           = prettyPrec d (T_id "Undefined")
    prettyPrec d (ArrayVal dims _)   = prettyPrec d (T_id "Array") . prettyDims
        where prettyDims = prettyPrec d T_lbracket .
                prettyPrecSepList d ", " (map T_const_int dims) . prettyPrec d T_rbracket
