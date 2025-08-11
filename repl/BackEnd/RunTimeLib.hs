module BackEnd.RunTimeLib (module BackEnd.RunTimeLib) where

import Data.IORef
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import Control.Monad

import Common.Value
import RunTime.LibHeaders (libSigs)
import Common.Interpreter (throwRunTime)

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
                ,("print_string", printString)
                ]

printInt :: RunTimeLibComputation
printInt [IntVal n] = do
    liftIO . putStr . show $ n
    return UnitVal
printInt args = throwRunTime ("Incorrect argument list " ++ show args ++ "  passed for print_int")

printChar :: RunTimeLibComputation
printChar [CharVal c] = liftIO (putChar c) >>  return UnitVal
printChar args = throwRunTime ("Incorrect argument list " ++ show args ++ "  passed for print_char")

printString :: RunTimeLibComputation
printString [ArrayVal _ _ m] = do
    let charRefs = map snd $ M.toAscList m
    let aux (CharVal '\0') = return ()
        aux c = void (printChar [c])
    chars <- mapM (liftIO . readIORef) charRefs
    mapM_ aux chars
    return UnitVal
printString args = throwRunTime ("Incorrect argument list " ++ show args ++ "  passed for print_string")