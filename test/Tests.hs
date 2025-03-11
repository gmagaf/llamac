module Main (main) where

import Property.Property (checkForSizes, checkParsedPrettyAST)

main :: IO ()
main = do
  putStrLn "Hello from tests!"
  putStrLn "Starting property based testing using QuickCheck"
  checkForSizes checkParsedPrettyAST sizes
  putStrLn "Finished testing!" where
    sizes = [0, 1, 2, 3, 4, 5, 10, 30, 100, 1000, 2000, 5000]
