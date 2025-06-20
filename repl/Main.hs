module Main (main) where

import System.Environment (getArgs)
import FrontEnd.FrontEnd (initRepl)

main :: IO ()
main = do
  putStrLn "Welcome to the Llama Interpreter"
  args <- getArgs
  let f = case args of 
        file:_ -> Just file
        []     -> Nothing
  initRepl f
