module Main (main) where

import Property.Property
import Unit.Unit

main :: IO ()
main = do
  putStrLn "Hello from tests!"
  putStrLn "Starting unit (non-guided) testing using HSpec"
  testParserSuite 1000
  putStrLn "Starting unit guided testing using HSpec"
  testParserGuidedSuite
  putStrLn "Starting property based testing using QuickCheck"
  checkForSizes checkParsedPrettyAST sizes
  putStrLn "Finished testing!" where
    sizes = [0, 1, 2, 3, 4, 5, 10, 30, 100, 1000, 2000, 5000]
