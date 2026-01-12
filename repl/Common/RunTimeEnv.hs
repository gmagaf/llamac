{-# LANGUAGE DeriveFunctor #-}
module Common.RunTimeEnv (module Common.RunTimeEnv) where

import qualified Data.Map as M

import Common.Token (Identifier)
import Common.PrintAST (Pretty, pretty)
import Common.DebugPrint

-- This module defines the basic
-- data types for the runtime environment
-- It also defines errors

data RunTimeError = RunTimeError { msg :: String }
                  | ParserError { msg :: String }

instance Show RunTimeError where
    show (RunTimeError err) = "RunTimeError: " ++ err
    show (ParserError err)  = "CompileTimeError: " ++ err

-- Activation records and RunTime are defined
-- polymorphically with respect to the values

data ActivationRecordF v =
    Activation
    { offset :: Int -- This is only for debugging purposes
    , locals :: M.Map Identifier v
    , control_link :: Maybe (ActivationRecordF v)
    , access_link :: Maybe (ActivationRecordF v)
    }
    | RecActivation
    { offset :: Int -- This is only for debugging purposes
    , locals :: M.Map Identifier v
    , control_link :: Maybe (ActivationRecordF v)
    , access_link :: Maybe (ActivationRecordF v)
    } deriving (Show, Functor)

data RunTimeEnvF v = RunTimeEnv { frame_pointer :: ActivationRecordF v
                                , heap_address :: Int
                                , user_mallocs :: M.Map Int Bool
                                , inputBuffer :: String
                                }
    deriving (Show, Functor)

instance (Show v, Pretty v) => DebugPrint (RunTimeEnvF v) where
    debugPrint rtenv =
        let prettyMaybe = maybe "null"
            prettyNestedAr nar = show (offset nar)
            prettyAr ar =
                show (offset ar) ++ " * "
                ++ "| locals: " ++ M.foldrWithKey (\i v a -> i ++ " = " ++ pretty v ++ if a == "" then "" else ", " ++ a) "" (locals ar)
                ++ " | control_link: " ++ prettyMaybe prettyNestedAr (control_link ar)
                ++ " | access_link: " ++ prettyMaybe prettyNestedAr (access_link ar)
                ++ " |"
            traverseStack ar = prettyAr ar ++ maybe "" (("\n" ++) . traverseStack) (control_link ar)
        in putStrLn $ traverseStack (frame_pointer rtenv)