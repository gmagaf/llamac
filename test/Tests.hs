module Main (main) where

import Unit.Unit (testParserGuidedSuite, testParserSuite, testSemGuidedSuite, testSemSuite)
import Property.Utils (checkForSizes)
import Property.Property (checkParsedPrettyAST, checkSemTypesAST, checkSemScopesAST)

main :: IO ()
main = do
  putStrLn "Hello from tests!"
  putStrLn "Starting unit (non-guided) testing using HSpec"
  testParserSuite 1000
  testSemSuite
  putStrLn "Starting unit guided testing using HSpec"
  testParserGuidedSuite
  testSemGuidedSuite
  putStrLn "Starting property based testing using QuickCheck"
  checkForSizes checkParsedPrettyAST sizes
  checkForSizes checkSemTypesAST sizes
  checkForSizes checkSemScopesAST sizes
  putStrLn "Finished testing!" where
    sizes = [0, 2, 4, 8, 16, 32, 64, 128, 256]
