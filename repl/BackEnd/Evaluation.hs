module BackEnd.Evaluation (runAST, evalExpr) where

import qualified Data.Map as M

import Common.Token (Identifier)
import Common.AST
import Semantics.Utils (SemanticTag (..))
import Control.Monad.IO.Class

import Common.Value
import Common.Interpreter

-- This module contains all the logic
-- for the interpretation of Llama into Haskell

{-
    Run time invariants: <- TODO: revise this
    1. Either locals or params is empty
    2. Each frame corresponds to an activation of a scope, the names are visible in this scope
    3. access_link contains the frame of the static scope containing current scope
-}


runAST :: AST SemanticTag -> Interpreter ()
runAST []              = return ()
runAST (Left ldef:ast) = runLet ldef >> runAST ast
runAST (Right _:ast)   = runAST ast

-- TODO: Define CORRECT implementation for each let
runLet :: LetDef SemanticTag -> Interpreter ()
runLet (Let _ _) = undefined
-- do
--     r <- getFramePointer
--     local_vals <- mapM runDef defs
--     let locals = M.fromList $ zipWith (\d v -> (ide d, v)) defs local_vals
--     let record = Activation { return_val = Nothing
--                             , params = M.empty
--                             , locals = locals
--                             , control_link = Just r
--                             , access_link = Just r }
--     putFramePointer record
runLet (LetRec defs _) = do
    r <- getFramePointer
    local_vals <- mapM runDef defs
    let local_defs = M.fromList $ zipWith (\d v -> (ide d, v)) defs local_vals
    let record = Activation { return_val = Nothing
                            , params = M.empty
                            , locals = local_defs
                            , control_link = Just r
                            , access_link = Just r }
    putFramePointer record

-- TODO: define all cases
runDef :: Def SemanticTag -> Interpreter Value
runDef (FunDef i ps e tg) = return (FunVal i (LlamaFun e))

evalRunTimeLib :: RunTimeLibComputation -> [Value] -> Interpreter Value
evalRunTimeLib c args = do
    res <- liftIO (c args)
    case res of
        Left err  -> throwRunTimeError err
        Right ret -> return ret

-- TODO: define all cases
evalExpr :: Expr SemanticTag -> Interpreter Value
evalExpr e@(Expr ef tg) =
    case ef of
        IntCExpr _              -> IntVal <$> evalIntExpr e
        BinOpExpr PlusOp e1 e2  -> IntVal <$> ((+) <$> evalIntExpr e1 <*> evalIntExpr e2)
        BinOpExpr MinusOp e1 e2 -> IntVal <$> ((-) <$> evalIntExpr e1 <*> evalIntExpr e2)
        BinOpExpr TimesOp e1 e2 -> IntVal <$> ((*) <$> evalIntExpr e1 <*> evalIntExpr e2)
        BinOpExpr DivOp e1 e2   -> IntVal <$> (div <$> evalIntExpr e1 <*> evalIntExpr e2)
        BinOpExpr ModOp e1 e2   -> IntVal <$> (mod <$> evalIntExpr e1 <*> evalIntExpr e2)
        ConstExpr i ->
            let cont v = return . const v
            in do
                fp <- getFramePointer
                findNameCont i fp cont
        -- FunAppExpr i es -> do
        --     vals <- mapM evalExpr es
        --     fp <- getFramePointer
        --     findNameCont i fp cont where
        --         cont f record = case f of
        --             FunVal _ (LlamaFun body) -> do
        --                 let record = Activation
        --                             { return_val = Nothing
        --                             , params = _ --M.fromList (zip ps vals)
        --                             , locals = M.empty
        --                             , control_link = Just fp
        --                             , access_link = access_link access_record
        --                             }
        --                 putFramePointer record
        --                 res <- evalExpr body
        --                 putFramePointer fp
        --                 return res
        --             FunVal _ (RunTimeFun run) -> run vals
        _ -> undefined
evalExpr _ = undefined
--         ConstExpr i -> do
--             fp <- getFramePointer
--             (access_record, f) <- findNameFrame i fp
--             case f of
--                 FunVal _ (LlamaFun body) -> do
--                     let record = Activation
--                                 { return_val = Nothing
--                                 , params = M.empty
--                                 , locals = M.empty
--                                 , control_link = Just fp
--                                 , access_link = Just access_record
--                                 }
--                     putFramePointer record
--                     res <- evalExpr body
--                     putFramePointer fp
--                     return res
--                 FunVal _ (RunTimeFun run) -> run []
--                 IntVal n -> return f

findNameCont :: Identifier
              -> ActivationRecord
              -> (Value -> ActivationRecord -> Interpreter a)
              -> Interpreter a
findNameCont i r f = searchMap (params r) f (searchMap (locals r) f (nextFrame r)) where
    searchMap m found nFound =
        case M.lookup i m of
            Just v -> found v r
            Nothing -> nFound
    nextFrame r' = case access_link r' of
                    Just al -> findNameCont i al f
                    Nothing -> throwRunTimeError (RunTimeError $ "No activation found for name: " ++ i)

evalIntExpr :: Expr SemanticTag -> Interpreter Int
evalIntExpr (Expr (IntCExpr n) _) = return n
evalIntExpr e = do
    v <- evalExpr e
    case v of
        IntVal n -> return n
        _ -> throwRunTimeError (RunTimeError { msg = "Expected Int value while evaluating expr: " ++ show e })
