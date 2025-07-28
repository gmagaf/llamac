module BackEnd.RunTimeLib (module BackEnd.RunTimeLib) where

import Data.IORef
import qualified Data.Map as M
import Control.Monad

import Common.Value
import RunTime.LibHeaders (libSigs)

-- This module contains all definitions of runtime functions

runTimeLib :: [(String, Value)]
runTimeLib =
    let sigMap = M.fromList (map (\(i, _, ps) -> (i, ps)) libSigs)
        createRun (i, impl) = (i, FunVal i (sigMap M.! i) (RunTimeFun impl))
    in map createRun implementions

-- TODO: Define all functions

implementions :: [(String, RunTimeLibComputation)]
implementions = [("print_int", printInt)
                ,("print_char", printChar)
                ,("print_string", printString)]

returnVal :: Value -> IO (Either RunTimeError Value)
returnVal = return . Right

returnErr :: String -> IO (Either RunTimeError Value)
returnErr = return . Left . RunTimeError

printInt :: RunTimeLibComputation
printInt [IntVal n] = do
    putStr . show $ n
    returnVal UnitVal
printInt args = returnErr ("Incorrect argument list " ++ show args ++ "  passed for print_int")

printChar :: RunTimeLibComputation
printChar [CharVal c] = putStr c >>  return (Right UnitVal)
printChar args = returnErr ("Incorrect argument list " ++ show args ++ "  passed for print_char")


printString :: RunTimeLibComputation
printString [ArrayVal _ _ m] = do
    let charRefs = map snd $ M.toAscList m
    chars <- mapM readIORef charRefs
    let aux (CharVal "\0") = return ()
        aux c = void (printChar [c])
    mapM_ aux chars
    return (Right UnitVal)
printString args = returnErr ("Incorrect argument list " ++ show args ++ "  passed for print_string")