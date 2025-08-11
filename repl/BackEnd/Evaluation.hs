module BackEnd.Evaluation (runAST, evalExpr) where

import GHC.IORef (newIORef, readIORef, writeIORef, IORef)
import qualified Data.Map as M
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad ((>=>), unless, foldM)

import Common.Token (Identifier, CharConstant)
import Common.AST
import Semantics.Utils (SemanticTag (..))

import Common.Value
import Common.RunTimeEnv
import Common.Interpreter

-- This module contains all the logic
-- for the interpretation of Llama into Haskell

{-
    Run time invariants:
    1. Each frame corresponds to an activation of a scope, the names are visible in subsequent scopes
       The names are also visible in the current scope if it is a RecActivation
    2. access_link contains the frame of the static scope containing current scope
    3. runLet pushes one frame
    4. evalExpr leaves the stack untouched
    5. consts are only computed during a let (rec), in other cases it is just retrieved from the stack
-}

runAST :: AST SemanticTag -> Interpreter ()
runAST []              = return ()
runAST (Left ldef:ast) = runLet ldef >> runAST ast
runAST (Right _:ast)   = runAST ast

runLet :: LetDef SemanticTag -> Interpreter ()
runLet (Let defs _) = do
    local_vals <- mapM (runDef >=> computeConst) defs
    let localDefs = M.fromList $ zipWith (\d v -> (ide d, v)) defs local_vals
    r <- getFramePointer
    let record = Activation { offset = 1 + offset r
                            , locals = localDefs
                            , control_link = Just r
                            , access_link = Just r }
    putFramePointer record
runLet (LetRec defs _) = do
    local_vals <- mapM runDef defs
    let local_defs = M.fromList $ zipWith (\d v -> (ide d, v)) defs local_vals
    r <- getFramePointer
    let record = RecActivation { offset = 1 + offset r
                            , locals = local_defs
                            , control_link = Just r
                            , access_link = Just r }
    putFramePointer record
    vals <- mapM computeConst local_vals
    let local_defs' = M.fromList $ zipWith (\d v -> (ide d, v)) defs vals
    putFramePointer record{ locals = local_defs' }

computeConst :: Value -> Interpreter Value
computeConst c = case c of
    FunVal _ [] (LlamaFun body)  -> evalExpr body >>= computeConst
    FunVal _ [] (RunTimeFun run) -> evalRunTimeLib run []
    v -> return v

-- TODO: In case of let rec - understand
-- the operational semantics of the computation
-- of array dimensions
runDef :: Def SemanticTag -> Interpreter Value
runDef (VarDef _ _)             = do
    x <- liftIO (newIORef Undefined)
    ha <- getAndIncrHeapAddress
    return (RefVal ha x)
runDef (VarDefTyped {})         = do
    x <- liftIO (newIORef Undefined)
    ha <- getAndIncrHeapAddress
    return (RefVal ha x)
runDef (ArrayDef _ ds _)        = do
    let checkDim n = if n < 0 then throwRunTime "Cannot create an array of negative dimension" else return n
    vDims <- mapM (evalIntExpr >=> checkDim) ds
    let size = product vDims
    ha <- getAndOffsetHeapAddress size
    let alloc n = do
          x  <- liftIO (newIORef Undefined)
          return (n, x)
    ar <- mapM alloc [0..(size - 1)]
    return (ArrayVal vDims ha (M.fromList ar))
runDef (ArrayDefTyped _ ds _ _) = do
    let checkDim n = if n < 0 then throwRunTime "Cannot create an array of negative dimension" else return n
    vDims <- mapM (evalIntExpr >=> checkDim) ds
    let size = product vDims
    ha <- getAndOffsetHeapAddress size
    let alloc n = do
          x  <- liftIO (newIORef Undefined)
          return (n, x)
    ar <- mapM alloc [0..(size - 1)]
    return (ArrayVal vDims ha (M.fromList ar))
runDef (FunDef i ps e _)        = return (FunVal i (map ide ps) (LlamaFun e))
runDef (FunDefTyped i ps _ e _) = return (FunVal i (map ide ps) (LlamaFun e))

finallyStack :: Interpreter a -> Interpreter a
finallyStack run = do
    fp <- getFramePointer
    let finally err = putFramePointer fp >> throwRunTimeError err
    catchRunTimeError run finally

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
    ConstConstrExpr i    -> do
        ha <- getAndIncrHeapAddress
        return (ConstrVal i ha [])
    ConstrAppExpr i args -> do
        vals <- mapM evalExpr args
        ha <- getAndIncrHeapAddress
        return (ConstrVal i ha vals)
    UnOpExpr op e1       -> evalUnOpExpr op e1
    BinOpExpr op e1 e2   -> evalBinOpExpr op e1 e2
    IfThenElseExpr cond e1 e2
                         -> do
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
        let forLoop index =
                if index <= uv
                then do
                    fp <- getFramePointer
                    let record = Activation { offset = 1 + offset fp
                                            , locals = M.fromList [(i, IntVal index)]
                                            , control_link = Just fp
                                            , access_link = Just fp }
                    putFramePointer record
                    evalUnitExpr e1
                    putFramePointer fp
                    forLoop (index + 1)
                else return UnitVal
        forLoop lv
    ForDownExpr i u l e1 -> do
        uv <- evalIntExpr u
        lv <- evalIntExpr l
        let forLoop index =
                if index >= lv
                then do
                    fp <- getFramePointer
                    let record = Activation { offset = 1 + offset fp
                                            , locals = M.fromList [(i, IntVal index)]
                                            , control_link = Just fp
                                            , access_link = Just fp }
                    putFramePointer record
                    evalUnitExpr e1
                    putFramePointer fp
                    forLoop (index - 1)
                else return UnitVal
        forLoop uv
    DeleteExpr u         -> do
        (ha, _) <- evalRefExpr u
        deallocate ha
        return UnitVal
    StringCExpr s        -> do
        let l = length s
        ha <- getAndOffsetHeapAddress (l + 1)
        let aux acc (a, c) = do
              r <- liftIO (newIORef (CharVal c))
              return ((a, r):acc)
        chars <- foldM aux [] (zip [0..(l - 1)] s)
        nullC <- liftIO (newIORef (CharVal '\0'))
        return (ArrayVal [l + 1] ha (M.fromList $ (l, nullC):chars))
    ArrayAccess {}       -> uncurry RefVal <$> evalRefExpr e
    ArrayDim ar dim      -> do
        let cont (ArrayVal ds _ _) _ = aux ds dim
            cont _ _               = throwRunTime "Cannot compute the dimension of something that is not an array"
        fp <- getFramePointer
        findNameCont ar fp cont where
            aux _ n | n < 1 = throwRunTime "Cannot compute the dimension that is less than 1"
            aux [] _        = throwRunTime $ "Array " ++ ar ++ " has less dimensions than " ++ show dim
            aux [x] 1       = return (IntVal x)
            aux (_:ds) n    = aux ds (n - 1)
evalExpr (LetIn l e _) = finallyStack $ do
    fp <- getFramePointer
    runLet l
    r <- evalExpr e
    putFramePointer fp
    return r
evalExpr (MatchExpr e cs _) = finallyStack $ do
        v <- evalExpr e
        matchPatterns v cs
evalExpr e@(NewType {}) = uncurry RefVal <$> evalRefExpr e

matchPatterns :: Value -> [Clause SemanticTag] -> Interpreter Value
matchPatterns _ [] = throwRunTime "Exhausted all patterns and found none to match"
matchPatterns v (Match pat e _:cs) = do
    (m, binds) <- matchPattern v pat
    if m then do
        fp <- getFramePointer
        let record = Activation { offset = 1 + offset fp
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
        (ConstrVal i1 _ vs, ConstrPattern i2 ps) | i1 == i2 -> aux acc vs ps where
            aux acc' [] []         = return (True, acc')
            aux acc' (_:_) []      = return (False, acc')
            aux acc' [] (_:_)      = return (False, acc')
            aux acc' (val:vals) (p:pats) = do
                (m, binds) <- auxMatch acc' val p
                if not m then return (False, binds)
                else aux binds vals pats
        _ -> return (False, acc)


evalRunTimeLib :: RunTimeLibComputation -> [(String, Value)] -> Interpreter Value
evalRunTimeLib run args = run (map snd args)

findNameCont :: Identifier
              -> ActivationRecord
              -> (Value -> ActivationRecord -> Interpreter a)
              -> Interpreter a
findNameCont i r f = searchMap (locals r) f (nextFrame r) where
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
    let cont c = return . const c
    findNameCont i fp cont

evalFunCall :: Identifier -> [Value] -> Interpreter Value
evalFunCall i vals = do
    fp <- getFramePointer
    let cont f access_record = case f of
            FunVal _ ps (LlamaFun body) -> do
                let record = Activation
                            { offset = 1 + offset fp
                            , locals = M.fromList (zip ps vals)
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
    BangOp -> do
        (ha, r) <- evalRefExpr e
        al <- isAllocated ha
        if al then liftIO (readIORef r)
        else throwRunTime "Unallocated memory access (read) attempt"

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
    AssignMutableOp -> do
        (ha, r) <- evalRefExpr e1
        al <- isAllocated ha
        if al then do
            v <- evalExpr e2
            liftIO (writeIORef r v)
            return UnitVal
        else throwRunTime "Unallocated memory access (write) attempt"

structEq :: Value -> Value -> Interpreter Bool
structEq (IntVal v1) (IntVal v2) = return (v1 == v2)
structEq (FloatVal v1) (FloatVal v2) = return (v1 == v2)
structEq (CharVal v1) (CharVal v2) = return (v1 == v2)
structEq (BoolVal v1) (BoolVal v2) = return (v1 == v2)
structEq UnitVal UnitVal = return True
structEq (ConstrVal i1 _ args1) (ConstrVal i2 _ args2) = do
    (&& (i1 == i2)) <$> eqArgs args1 args2 where
        eqArgs [] [] = return True
        eqArgs (_:_) [] = return False
        eqArgs [] (_:_) = return False
        eqArgs (x:xs) (y:ys) = (&&) <$> structEq x y <*> eqArgs xs ys
structEq (RefVal ha1 _) (RefVal ha2 _) = return (ha1 == ha2)
structEq (FunVal i _ _) _ = throwRunTime ("Cannot perform structural equality on function: " ++ i)
structEq _ (FunVal i _ _) = throwRunTime ("Cannot perform structural equality on function: " ++ i)
structEq (ArrayVal {}) _ = throwRunTime "Cannot perform structural equality on array"
structEq _ (ArrayVal {}) = throwRunTime "Cannot perform structural equality on array"
structEq Undefined _ = throwRunTime "Cannot compare an undefined value"
structEq _ Undefined = throwRunTime "Cannot compare an undefined value"
structEq _ _ = throwRunTime "Cannot compare values of different types"

natEq :: Expr SemanticTag -> Expr SemanticTag -> Interpreter Bool
natEq e1 e2 = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (ConstrVal _ ha1 _, ConstrVal _ ha2 _) -> return (ha1 == ha2)
        _ -> structEq v1 v2

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

evalRefExpr :: Expr SemanticTag -> Interpreter (Int, IORef Value)
evalRefExpr (NewType _ _) = do
    ha <- getAndIncrHeapAddress
    allocate ha
    r <- liftIO (newIORef Undefined)
    return (ha, r)
evalRefExpr (Expr (ArrayAccess i dims) _) = do
    vDims <- mapM evalIntExpr dims
    let cont (ArrayVal ds ha ar) _ = do
            unless (validDims ds vDims) $
                throwRunTime ("Out of bounds access dimensions for array " ++ i)
            memOffset <- convertOffset ds vDims
            case M.lookup memOffset ar of
                Just x  -> return (ha + memOffset, x)
                Nothing -> throwRunTime ("Unable to access offset " ++ show memOffset ++ " of array " ++ i)
        cont _ _                = throwRunTime "Cannot array-access something that is not an array"
    fp <- getFramePointer
    findNameCont i fp cont where
        validDims [] []         = True
        validDims [] _          = False
        validDims _ []          = False
        validDims (d:ds) (v:vs) = (0 <= v) && (v < d) && validDims ds vs
        -- m0,m1,.. are dimensions
        -- A(i,j,k,...) -> A0[i + j*m0 + k*m0*m1 + ...]
        convertOffset :: [Int] -> [Int] -> Interpreter Int
        convertOffset = aux (1 :: Int) where
            aux _ _ [] = return 0
            aux m (d:ds) (v:vs) = (v * m +) <$> aux (m * d) ds vs
            aux _ [] (_:_) = throwRunTime "Failed to convert multi-dim offset to memory offset"
evalRefExpr e = do
    v <- evalExpr e
    case v of
        RefVal ha r -> return (ha, r)
        _ -> throwRunTime ("Expected ref value while evaluating expr: " ++ show e)
