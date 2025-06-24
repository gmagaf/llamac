module BackEnd.Evaluation (runAST, evalExpr) where

import qualified Data.Map as M
import Control.Monad.IO.Class (MonadIO(liftIO))

import Common.Token (Identifier, CharConstant)
import Common.AST
import Semantics.Utils (SemanticTag (..))

import Common.Value
import Common.Interpreter

-- This module contains all the logic
-- for the interpretation of Llama into Haskell

{-
    Run time invariants:
    1. Either locals or params is empty for each AR
    2. Each frame corresponds to an activation of a scope, the names are visible in subsequent scopes
       The names are also visible in the current scope if it is a RecActivation
    3. access_link contains the frame of the static scope containing current scope
    4. runLet pushes one frame
    5. evalExpr leaves the stack untouched
-}

runAST :: AST SemanticTag -> Interpreter ()
runAST []              = return ()
runAST (Left ldef:ast) = runLet ldef >> runAST ast
runAST (Right _:ast)   = runAST ast

runLet :: LetDef SemanticTag -> Interpreter ()
runLet (Let defs _) = do
    r <- getFramePointer
    local_vals <- mapM runDef defs
    let localDefs = M.fromList $ zipWith (\d v -> (ide d, v)) defs local_vals
    let record = Activation { offset = 1 + offset r
                            , return_val = Nothing
                            , params = M.empty
                            , locals = localDefs
                            , control_link = Just r
                            , access_link = Just r }
    putFramePointer record
runLet (LetRec defs _) = do
    r <- getFramePointer
    local_vals <- mapM runDef defs
    let local_defs = M.fromList $ zipWith (\d v -> (ide d, v)) defs local_vals
    let record = RecActivation { offset = 1 + offset r
                            , return_val = Nothing
                            , params = M.empty
                            , locals = local_defs
                            , control_link = Just r
                            , access_link = Just r }
    putFramePointer record

-- TODO: define all cases
runDef :: Def SemanticTag -> Interpreter Value
runDef (FunDef i ps e _)        = return (FunVal i (map ide ps) (LlamaFun e))
runDef (FunDefTyped i ps _ e _) = return (FunVal i (map ide ps) (LlamaFun e))

finallyStack :: Interpreter a -> Interpreter a
finallyStack run = do
    fp <- getFramePointer
    let finally err = putFramePointer fp >> throwRunTimeError err
    catchRunTimeError run finally

-- TODO: define all cases
evalExpr :: Expr SemanticTag -> Interpreter Value
evalExpr e@(Expr ef _) = finallyStack $ case ef of
    IntCExpr _           -> IntVal <$> evalIntExpr e
    FloatCExpr _         -> FloatVal <$> evalFloatExpr e
    CharCExpr _          -> CharVal <$> evalCharExpr e
    UnitCExpr            -> evalUnitExpr e >> return UnitVal
    TrueCExpr            -> BoolVal <$> evalBoolExpr e
    FalseCExpr           -> BoolVal <$> evalBoolExpr e
    ConstExpr i          -> evalConst i
    FunAppExpr i es      -> do
        vals <- mapM evalExpr es
        evalFunCall i vals
    ConstConstrExpr i    -> return (ConstrVal i [])
    ConstrAppExpr i args -> do
        vals <- mapM evalExpr args
        return (ConstrVal i vals)
    UnOpExpr op e1       -> evalUnOpExpr op e1
    BinOpExpr op e1 e2   -> evalBinOpExpr op e1 e2
    IfThenElseExpr cond e1 e2 -> do
        b <- evalBoolExpr cond
        if b then evalExpr e1 else evalExpr e2
    IfThenExpr cond e1   -> do
        b <- evalBoolExpr cond
        if b then evalExpr e1 else return UnitVal
    BeginExpr e'         -> evalExpr e'
    WhileExpr cond e1    -> loop where
        loop = do
            b <- evalBoolExpr cond
            if b then evalUnitExpr e1 >> loop else return UnitVal
    ForExpr i l u e1     -> do
        lv <- evalIntExpr l
        uv <- evalIntExpr u
        let body index = do
                fp <- getFramePointer
                let record = Activation { offset = 1 + offset fp
                                    , return_val = Nothing
                                    , params = M.empty
                                    , locals = M.fromList [(i, IntVal index)]
                                    , control_link = Just fp
                                    , access_link = Just fp }
                putFramePointer record
                evalUnitExpr e1
                putFramePointer fp
        forLoop lv uv body where
            forLoop index u body =
                if index <= u then body index >> forLoop (index + 1) u body
                else return UnitVal
    ForDownExpr i u l e1 -> do
        uv <- evalIntExpr u
        lv <- evalIntExpr l
        let body index = do
                fp <- getFramePointer
                let record = Activation { offset = 1 + offset fp
                                        , return_val = Nothing
                                        , params = M.empty
                                        , locals = M.fromList [(i, IntVal index)]
                                        , control_link = Just fp
                                        , access_link = Just fp }
                putFramePointer record
                evalUnitExpr e1
                putFramePointer fp
        forLoop uv lv body where
            forLoop index l body =
                if index >= l then body index >> forLoop (index - 1) l body
                else return UnitVal
evalExpr (LetIn l e _) = finallyStack $ do
    fp <- getFramePointer
    runLet l
    r <- evalExpr e
    putFramePointer fp
    return r
evalExpr (MatchExpr e cs _) = finallyStack $ do
        v <- evalExpr e
        matchPatterns v cs

matchPatterns :: Value -> [Clause SemanticTag] -> Interpreter Value
matchPatterns _ [] = throwRunTime "Exhausted all patterns and found none to match"
matchPatterns v (Match pat e _:cs) = do
    (m, binds) <- matchPattern v pat
    if m then do
        fp <- getFramePointer
        let record = Activation { offset = 1 + offset fp
                                , return_val = Nothing
                                , params = M.empty
                                , locals = M.fromList binds
                                , control_link = Just fp
                                , access_link = Just fp }
        putFramePointer record
        res <- evalExpr e
        putFramePointer fp
        return res
    else matchPatterns v cs

matchPattern :: Value -> Pattern SemanticTag -> Interpreter (Bool, [(Identifier, Value)])
matchPattern = auxMatch [] where
    auxMatch acc v (Pattern pat _) = case (v, pat) of
        (IntVal nv, IntConstPattern NoSign n)
            | nv == n -> return (True, acc)
        (IntVal nv, IntConstPattern Plus n)
            | nv == n -> return (True, acc)
        (IntVal nv, IntConstPattern Minus n)
            | nv == - n -> return (True, acc)
        (FloatVal fv, FloatConstPattern NoSign f)
            | fv == f -> return (True, acc)
        (FloatVal fv, FloatConstPattern Plus f)
            | fv == f -> return (True, acc)
        (FloatVal fv, FloatConstPattern Minus f)
            | fv == - f -> return (True, acc)
        (CharVal cv, CharConstPattern c)
            | cv == c -> return (True, acc)
        (BoolVal True, TruePattern) -> return (True, acc)
        (BoolVal False, FalsePattern) -> return (True, acc)
        (_, IdPattern x) -> return (True, (x, v) : acc)
        (ConstrVal i1 vs, ConstrPattern i2 ps) | i1 == i2 -> aux acc vs ps where
            aux acc' [] []         = return (True, acc')
            aux acc' (_:_) []      = return (False, acc')
            aux acc' [] (_:_)      = return (False, acc')
            aux acc' (val:vals) (p:pats) = do
                (m, binds) <- auxMatch acc' val p
                if not m then return (False, binds)
                else aux binds vals pats
        _ -> return (False, acc)


evalRunTimeLib :: RunTimeLibComputation -> [(String, Value)] -> Interpreter Value
evalRunTimeLib run args = do
    res <- liftIO (run (map snd args))
    case res of
        Left err  -> throwRunTimeError err
        Right ret -> return ret

findNameCont :: Identifier
              -> ActivationRecord
              -> (Value -> ActivationRecord -> Interpreter a)
              -> Interpreter a
findNameCont i r f = searchMap (params r) f searchLocals where
    searchLocals = searchMap (locals r) f (nextFrame r)
    searchMap m found nFound =
        case M.lookup i m of
            Just v -> found v r
            Nothing -> nFound
    nextFrame r' = case access_link r' of
                    Just al -> findNameCont i al f
                    Nothing -> throwRunTimeError (RunTimeError $ "No activation found for name: " ++ i)

getFunStaticContext :: ActivationRecord -> Maybe ActivationRecord
getFunStaticContext ar@(Activation {})    = access_link ar
getFunStaticContext ar@(RecActivation {}) = Just ar

evalConst :: Identifier -> Interpreter Value
evalConst i = do
    fp <- getFramePointer
    let cont c access_record = case c of
            FunVal _ [] (LlamaFun body) -> do
                let record = Activation
                            { offset = 1 + offset fp
                            , return_val = Nothing
                            , params = M.empty
                            , locals = M.empty
                            , control_link = Just fp
                            , access_link = getFunStaticContext access_record
                            }
                putFramePointer record
                res <- evalExpr body
                putFramePointer fp
                return res
            FunVal _ [] (RunTimeFun run) -> evalRunTimeLib run []
            v -> return v
    findNameCont i fp cont

evalFunCall :: Identifier -> [Value] -> Interpreter Value
evalFunCall i vals = do
    fp <- getFramePointer
    let cont f access_record = case f of
            FunVal _ ps (LlamaFun body) -> do
                let record = Activation
                            { offset = 1 + offset fp
                            , return_val = Nothing
                            , params = M.fromList (zip ps vals)
                            , locals = M.empty
                            , control_link = Just fp
                            , access_link = getFunStaticContext access_record
                            }
                putFramePointer record
                res <- evalExpr body
                putFramePointer fp
                return res
            FunVal _ ps (RunTimeFun run) -> evalRunTimeLib run (zip ps vals)
            v -> throwRunTime ("Value: " ++ show v ++ " cannot be applied to args")
    findNameCont i fp cont

evalUnOpExpr :: UnOp -> Expr SemanticTag -> Interpreter Value
evalUnOpExpr op e = case op of
    PlusUnOp -> IntVal <$> evalIntExpr e
    MinusUnOp -> IntVal . (0-) <$> evalIntExpr e
    PlusFloatUnOp -> FloatVal <$> evalFloatExpr e
    MinusFloatUnOp -> FloatVal . (0-) <$> evalFloatExpr e
    NotOp -> BoolVal . not <$> evalBoolExpr e
    -- TODO: Add BangOp

evalBinOpExpr :: BinOp -> Expr SemanticTag -> Expr SemanticTag -> Interpreter Value
evalBinOpExpr op e1 e2 = case op of
    PlusOp  -> IntVal <$> ((+) <$> evalIntExpr e1 <*> evalIntExpr e2)
    MinusOp -> IntVal <$> ((-) <$> evalIntExpr e1 <*> evalIntExpr e2)
    TimesOp -> IntVal <$> ((*) <$> evalIntExpr e1 <*> evalIntExpr e2)
    DivOp   -> do
        nom <- evalIntExpr e1
        denom <- evalIntExpr e2
        if denom == 0 then throwRunTime "Division by zero"
        else return (IntVal (div nom denom))
    ModOp   -> do
        nom <- evalIntExpr e1
        denom <- evalIntExpr e2
        if denom == 0 then throwRunTime "Modulo by zero"
        else return (IntVal (mod nom denom))
    PlusFloatOp  -> FloatVal <$> ((+) <$> evalFloatExpr e1 <*> evalFloatExpr e2)
    MinusFloatOp -> FloatVal <$> ((-) <$> evalFloatExpr e1 <*> evalFloatExpr e2)
    TimesFloatOp -> FloatVal <$> ((*) <$> evalFloatExpr e1 <*> evalFloatExpr e2)
    DivFloatOp   -> do
        nom <- evalFloatExpr e1
        denom <- evalFloatExpr e2
        if denom == 0 then throwRunTime "Float division by zero"
        else return (FloatVal ((/) nom denom))
    ExpOp   -> FloatVal <$> ((**) <$> evalFloatExpr e1 <*> evalFloatExpr e2)
    EqOp -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        BoolVal <$> structEq v1 v2
    NotEqOp -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        BoolVal . not <$> structEq v1 v2
    NatEqOp -> BoolVal <$> natEq e1 e2
    NotNatEqOp -> BoolVal . not <$> natEq e1 e2
    LTOp -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        BoolVal . (LT ==) <$> ordVal v1 v2
    GTOp -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        BoolVal . (GT ==) <$> ordVal v1 v2
    LEqOp -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        BoolVal . (GT /=) <$> ordVal v1 v2
    GEqOp -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        BoolVal . (LT /=) <$> ordVal v1 v2
    AndOp -> do
        v1 <- evalBoolExpr e1
        if v1 then BoolVal <$> evalBoolExpr e2 else return (BoolVal False)
    OrOp -> do
        v1 <- evalBoolExpr e1
        if v1 then return (BoolVal True) else BoolVal <$> evalBoolExpr e2
    SemicolonOp -> do
        evalUnitExpr e1
        evalExpr e2
    -- TODO: Add :=

structEq :: Value -> Value -> Interpreter Bool
structEq (IntVal v1) (IntVal v2) = return (v1 == v2)
structEq (FloatVal v1) (FloatVal v2) = return (v1 == v2)
structEq (CharVal v1) (CharVal v2) = return (v1 == v2)
structEq (BoolVal v1) (BoolVal v2) = return (v1 == v2)
structEq UnitVal UnitVal = return True
structEq (ConstrVal i1 args1) (ConstrVal i2 args2) = do
    (&& (i1 == i2)) <$> eqArgs args1 args2 where
        eqArgs [] [] = return True
        eqArgs (_:_) [] = return False
        eqArgs [] (_:_) = return False
        eqArgs (x:xs) (y:ys) = (&&) <$> structEq x y <*> eqArgs xs ys
structEq (FunVal i _ _) _ = throwRunTime ("Cannot perform structural equality on function: " ++ i)
structEq _ (FunVal i _ _) = throwRunTime ("Cannot perform structural equality on function: " ++ i)

natEq :: Expr SemanticTag -> Expr SemanticTag -> Interpreter Bool
natEq = undefined -- TODO: Define this

ordVal :: Value -> Value -> Interpreter Ordering
ordVal (IntVal v1) (IntVal v2) = return (compare v1 v2)
ordVal (FloatVal v1) (FloatVal v2) = return (compare v1 v2)
ordVal (CharVal v1) (CharVal v2) = return (compare v1 v2)
ordVal _ _ = throwRunTime "Can only compare terms of type int, float of char"

evalIntExpr :: Expr SemanticTag -> Interpreter Int
evalIntExpr (Expr (IntCExpr n) _) = return n
evalIntExpr e = do
    v <- evalExpr e
    case v of
        IntVal n -> return n
        _ -> throwRunTime ("Expected int value while evaluating expr: " ++ show e)

evalFloatExpr :: Expr SemanticTag -> Interpreter Float
evalFloatExpr (Expr (FloatCExpr f) _) = return f
evalFloatExpr e = do
    v <- evalExpr e
    case v of
        FloatVal f -> return f
        _ -> throwRunTime ("Expected float value while evaluating expr: " ++ show e)

evalBoolExpr :: Expr SemanticTag -> Interpreter Bool
evalBoolExpr (Expr TrueCExpr _) = return True
evalBoolExpr (Expr FalseCExpr _) = return False
evalBoolExpr e = do
    v <- evalExpr e
    case v of
        BoolVal b -> return b
        _ -> throwRunTime ("Expected bool value while evaluating expr: " ++ show e)

evalUnitExpr :: Expr SemanticTag -> Interpreter ()
evalUnitExpr (Expr UnitCExpr _) = return ()
evalUnitExpr e = do
    v <- evalExpr e
    case v of
        UnitVal -> return ()
        _ -> throwRunTime ("Expected unit value while evaluating expr: " ++ show e)

evalCharExpr :: Expr SemanticTag -> Interpreter CharConstant
evalCharExpr (Expr (CharCExpr c) _) = return c
evalCharExpr e = do
    v <- evalExpr e
    case v of
        CharVal c -> return c
        _ -> throwRunTime ("Expected char value while evaluating expr: " ++ show e)
