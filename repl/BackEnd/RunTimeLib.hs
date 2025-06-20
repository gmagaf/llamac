module BackEnd.RunTimeLib (module BackEnd.RunTimeLib) where

import Common.Value

-- This module contains all definitions of runtime functions

-- TODO: Define all functions

returnVal :: Value -> IO (Either RunTimeError Value)
returnVal = return . Right

returnErr :: String -> IO (Either RunTimeError Value)
returnErr = return . Left . RunTimeError

printInt :: RunTimeLibComputation
printInt [IntVal n] = do
    putStr . show $ n
    returnVal UnitVal
printInt args = returnErr ("Incorrect argument list " ++ show args ++ "  passed for print_int")