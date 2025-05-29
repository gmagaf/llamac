module Main (main) where

import Unit.Unit (testParserGuidedSuite, testParserSuite, testSemSuite)
import Property.Utils (checkForSizes)
import Property.Property (checkParsedPrettyAST, checkSemTypesAST, checkSemScopesAST)

main :: IO ()
main = do
  putStrLn "Hello from tests!"
  putStrLn "Starting unit (non-guided) testing using HSpec"
  testParserSuite 1000
  putStrLn "Starting unit guided testing using HSpec"
  testParserGuidedSuite
  testSemSuite
  putStrLn "Starting property based testing using QuickCheck"
  checkForSizes checkParsedPrettyAST sizes
  checkForSizes checkSemTypesAST sizes
  checkForSizes checkSemScopesAST sizes
  putStrLn "Finished testing!" where
    sizes = [0, 1, 2, 3, 4, 5, 10, 30, 100]
