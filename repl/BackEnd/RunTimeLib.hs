module BackEnd.RunTimeLib (module BackEnd.RunTimeLib) where

import Text.Read (readMaybe)
import Data.Char (ord, chr)
import Prelude hiding (round)
import qualified Data.Map as M
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, writeIORef)

import RunTime.LibHeaders (libSigs)
import Common.Token (IntConstant, CharConstant, FloatConstant, StringConstant)

import Common.Value
import Common.Interpreter (throwRunTime, Interpreter, writeToBuffer, readFromBuffer)

-- This module contains all definitions of runtime functions

runTimeLib :: [(String, Value)]
runTimeLib =
    let sigMap = M.fromList (map (\(i, _, ps) -> (i, ps)) libSigs)
        createRun (i, impl) = (i, FunVal i (sigMap M.! i) (RunTimeFun impl))
    in map createRun implementions

implementions :: [(String, RunTimeLibComputation)]
implementions = [("print_int", printInt)
                ,("print_bool", printBool)
                ,("print_float", printFloat)
                ,("print_char", printChar)
                ,("print_string", printString)
                ,("read_int", readInt)
                ,("read_bool", readBool)
                ,("read_char", readChar)
                ,("read_float", readFloat)
                ,("read_string", readString)
                ,("abs", iabs)
                ,("fabs", fabs)
                ,("sqrt", fsqrt)
                ,("sin", fsin)
                ,("cos", fcos)
                ,("tan", ftan)
                ,("atan", fatan)
                ,("exp", fexp)
                ,("ln", fln)
                ,("pi", fpi)
                ,("incr", incr)
                ,("decr", decr)
                ,("float_of_int", floatOfInt)
                ,("int_of_float", intOfFloat)
                ,("round", round)
                ,("int_of_char", intOfChar)
                ,("char_of_int", charOfInt)
                ,("strlen", strLen)
                ,("strcmp", strCmp)
                ,("strcpy", strCpy)
                ,("strcat", strCat)
                ]

printInt :: RunTimeLibComputation
printInt [IntVal n] = do
    liftIO . putStr . show $ n
    return UnitVal
printInt args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for print_int")

printBool :: RunTimeLibComputation
printBool [BoolVal v] = do
    let s = if v then "true" else "false"
    liftIO (putStr s)
    return UnitVal
printBool args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for print_bool")

printFloat :: RunTimeLibComputation
printFloat [FloatVal v] = do
    liftIO . putStr . show $ v
    return UnitVal
printFloat args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for print_float")

printChar :: RunTimeLibComputation
printChar [CharVal c] = do
    liftIO (putChar c)
    return UnitVal
printChar args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for print_char")

printString :: RunTimeLibComputation
printString [ArrayVal _ _ m] = do
    let charRefs = map snd $ M.toAscList m
    let aux [] = throwRunTime "Cannot print a non null-terminated string"
        aux (CharVal '\0':_) = return ()
        aux (c:cs) = printChar [c] >> aux cs
    chars <- mapM (liftIO . readIORef) charRefs
    aux chars
    return UnitVal
printString args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for print_string")

readInt :: RunTimeLibComputation
readInt [UnitVal] = do
    line <- liftIO getLine
    case readMaybe line :: Maybe IntConstant of
        Just n  -> return (IntVal n)
        Nothing -> throwRunTime ("Failed to read int from: " ++ line)
readInt args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for read_int")

readBool :: RunTimeLibComputation
readBool [UnitVal] = do
    line <- liftIO getLine
    case line of
        "true"  -> return (BoolVal True)
        "false" -> return (BoolVal False)
        _       -> throwRunTime ("Failed to read int from: " ++ line)
readBool args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for read_bool")

readChar :: RunTimeLibComputation
readChar [UnitVal] = do
    line <- liftIO getLine
    case readMaybe ("'" ++ line ++ "'") :: Maybe CharConstant of
        Just c  -> return (CharVal c)
        Nothing -> throwRunTime ("Failed to read char from: " ++ line)
readChar args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for read_char")

readFloat :: RunTimeLibComputation
readFloat [UnitVal] = do
    line <- liftIO getLine
    case readMaybe line :: Maybe FloatConstant of
        Just f  -> return (FloatVal f)
        Nothing -> throwRunTime ("Failed to read float from: " ++ line)
readFloat args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for read_float")

readString :: RunTimeLibComputation
readString [ArrayVal [d] ha ar] = do
    line <- liftIO getLine
    rline <- case readMaybe ("\"" ++ line ++ "\"") :: Maybe StringConstant of
        Just l  -> return l
        Nothing -> throwRunTime ("Failed to read string from: " ++ line)
    writeToBuffer rline
    s <- readFromBuffer (d - 1)
    let aux (n, c) = do
            case M.lookup n ar of
                Just ref -> liftIO $ writeIORef ref (CharVal c)
                Nothing  -> throwRunTime ("Unallocated memory position " ++ show (ha + n) ++ " of array, while reading string")
    mapM_ aux (zip [0..(length s - 1)] s)
    aux (length s, '\0')
    return UnitVal
readString args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for read_string")

iabs :: RunTimeLibComputation
iabs [IntVal n] = return . IntVal . abs $ n
iabs args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for abs")

fabs :: RunTimeLibComputation
fabs [FloatVal f] = return . FloatVal . abs $ f
fabs args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for fabs")

fsqrt :: RunTimeLibComputation
fsqrt [FloatVal f] = return . FloatVal . sqrt $ f
fsqrt args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for sqrt")

fsin :: RunTimeLibComputation
fsin [FloatVal f] = return . FloatVal . sin $ f
fsin args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for sin")

fcos :: RunTimeLibComputation
fcos [FloatVal f] = return . FloatVal . cos $ f
fcos args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for cos")

ftan :: RunTimeLibComputation
ftan [FloatVal f] = return . FloatVal . tan $ f
ftan args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for tan")

fatan :: RunTimeLibComputation
fatan [FloatVal f] = return . FloatVal . atan $ f
fatan args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for atan")

fexp :: RunTimeLibComputation
fexp [FloatVal f] = return . FloatVal . exp $ f
fexp args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for exp")

fln :: RunTimeLibComputation
fln [FloatVal f] = return . FloatVal . log $ f
fln args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for ln")

fpi :: RunTimeLibComputation
fpi [UnitVal] = return . FloatVal $ pi
fpi args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for pi")

incr :: RunTimeLibComputation
incr [RefVal _ ref] = do
    v <- liftIO (readIORef ref)
    case v of
        IntVal i -> do
            liftIO $ writeIORef ref (IntVal (i + 1))
            return UnitVal
        Undefined -> throwRunTime "Cannot increase the value of a ref that is not instantiated yet"
        _ -> throwRunTime "Cannot increase the value of a ref that is not int"
incr args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for incr")

decr :: RunTimeLibComputation
decr [RefVal _ ref] = do
    v <- liftIO (readIORef ref)
    case v of
        IntVal i -> do
            liftIO $ writeIORef ref (IntVal (i - 1))
            return UnitVal
        Undefined -> throwRunTime "Cannot decrease the value of a ref that is not instantiated yet"
        _ -> throwRunTime "Cannot dencrease the value of a ref that is not int"
decr args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for decr")

floatOfInt :: RunTimeLibComputation
floatOfInt [IntVal n] = return . FloatVal . fromIntegral $ n
floatOfInt args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for float_of_int")

intOfFloat :: RunTimeLibComputation
intOfFloat [FloatVal v] = return . IntVal . fst . properFraction $ v
intOfFloat args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for int_of_float")

round :: RunTimeLibComputation
round [FloatVal v] = return . IntVal . aux $ v
    where aux f = let (i, d) = properFraction f
                  in if abs d < 0.5 then i else
                        if i >=0 then i + 1 else i - 1
round args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for round")

intOfChar :: RunTimeLibComputation
intOfChar [CharVal c] = return . IntVal . ord $ c
intOfChar args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for int_of_char")

charOfInt :: RunTimeLibComputation
charOfInt [IntVal i] = return . CharVal . chr $ i
charOfInt args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for int_of_char")

strLen :: RunTimeLibComputation
strLen [ArrayVal [_] _ ar] = aux 0 (M.toAscList ar)
    where aux :: IntConstant -> [(Int, IORef Value)] -> Interpreter Value
          aux _ []            = throwRunTime "Cannot compute the length of a non null-terminated string"
          aux acc ((_, r):rs) = do
            v <- liftIO (readIORef r)
            case v of
                CharVal '\0'              -> return (IntVal acc)
                CharVal _ | not (null rs) -> aux (acc + 1) rs
                CharVal _ | null rs       -> throwRunTime "Cannot compute the length of a non null-terminated string"
                Undefined                 -> throwRunTime "Cannot compute the length of a string that is not fully instantiated"
                _                         -> throwRunTime "Cannot compute the length of a string that is not an array of chars"
strLen args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for strlen")

arrayToString :: String -> M.Map Int (IORef Value) -> Interpreter StringConstant
arrayToString s ar = reverse <$> aux "" (M.toAscList ar) where
    aux _ []            = throwRunTime $ "Cannot " ++ s ++ " of a non null-terminated string"
    aux acc ((_, r):rs) = do
            v <- liftIO (readIORef r)
            case v of
                CharVal '\0'              -> return acc
                CharVal c | not (null rs) -> aux (c:acc) rs
                CharVal _ | null rs       -> throwRunTime $ "Cannot " ++ s ++ " a non null-terminated string"
                Undefined                 -> throwRunTime $ "Cannot " ++ s ++ " a string that is not fully instantiated"
                _                         -> throwRunTime $ "Cannot " ++ s ++ " a string that is not an array of chars"

strCmp :: RunTimeLibComputation
strCmp [ArrayVal [_] _ a1, ArrayVal [_] _ a2] = do
    s1 <- arrayToString "compare" a1
    s2 <- arrayToString "compare" a2
    case compare s1 s2 of
        LT -> return (IntVal (-1))
        EQ -> return (IntVal 0)
        GT -> return (IntVal 1)
strCmp args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for strcmp")

strCpy :: RunTimeLibComputation
strCpy [ArrayVal [d] ha dest, ArrayVal [_] _ source] = do
    s <- arrayToString "copy" source
    let l = length s
    when (d < l + 1) $
            throwRunTime ("Cannot copy a string of length " ++ show l ++ " to an array of size " ++ show d)
    let aux (n, c) = do
            case M.lookup n dest of
                Just ref -> liftIO $ writeIORef ref (CharVal c)
                Nothing  -> throwRunTime ("Unallocated memory position " ++ show (ha + n) ++ " of array, while copying string")
    mapM_ aux (zip [0..(l - 1)] s)
    aux (l, '\0')
    return UnitVal
strCpy args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for strcpy")

strCat :: RunTimeLibComputation
strCat [ArrayVal [d] ha dest, ArrayVal [_] _ source] = do
    ds <- arrayToString "concatenate" dest
    s <- arrayToString "concatenate" source
    let dl = length ds
    let l = length s
    when (d < dl + l + 1) $
            throwRunTime ("Cannot concatenate two strings of length " ++ show (dl + l) ++ " to an array of size " ++ show d)
    let aux (n, c) = do
            case M.lookup n dest of
                Just ref -> liftIO $ writeIORef ref (CharVal c)
                Nothing  -> throwRunTime ("Unallocated memory position " ++ show (ha + n) ++ " of array, while concatenating strings")
    mapM_ aux (zip [dl..(dl + l - 1)] s)
    aux (dl + l, '\0')
    return UnitVal
strCat args = throwRunTime ("Incorrect argument list " ++ show args ++ " passed for strcat")
