module BackEnd.RunTimeLib (module BackEnd.RunTimeLib) where

import qualified Data.Map as M

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
implementions = [("print_int", printInt)]

returnVal :: Value -> IO (Either RunTimeError Value)
returnVal = return . Right

returnErr :: String -> IO (Either RunTimeError Value)
returnErr = return . Left . RunTimeError

printInt :: RunTimeLibComputation
printInt [IntVal n] = do
    putStr . show $ n
    returnVal UnitVal
printInt args = returnErr ("Incorrect argument list " ++ show args ++ "  passed for print_int")