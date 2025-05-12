module Main (main) where

import Property.Property (checkForSizes, checkParsedPrettyAST, checkSemanticASTisOK)
import Unit.Unit (testParserGuidedSuite, testParserSuite)

main :: IO ()
main = do
  putStrLn "Hello from tests!"
  putStrLn "Starting unit (non-guided) testing using HSpec"
  testParserSuite 1000
  putStrLn "Starting unit guided testing using HSpec"
  testParserGuidedSuite
  putStrLn "Starting property based testing using QuickCheck"
  checkForSizes checkParsedPrettyAST sizes
  checkForSizes checkSemanticASTisOK (sizes ++ hugeSizes)
  putStrLn "Finished testing!" where
    sizes = [0, 1, 2, 3, 4, 5, 10, 30, 100, 1000, 2000, 5000]
    hugeSizes = [10000, 20000, 50000, 100000]
