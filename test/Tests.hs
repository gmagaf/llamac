module Main (main) where

import Unit.Unit (testParserSuite, testSemSuite, testGuidedParser, testGuidedSem, testPrettySuite)
import Property.Utils (checkForSizes)
import Property.Property (checkLexer, checkParsedPrettyAST, checkSemTypesAST, checkSemScopesAST)

main :: IO ()
main = do
  putStrLn "Hello from tests!"
  putStrLn "Starting unit (non-guided) testing using HSpec"
  testParserSuite 10
  testSemSuite
  putStrLn "Starting unit guided testing using HSpec"
  testPrettySuite
  testGuidedParser
  testGuidedSem
  putStrLn "Starting property based testing using QuickCheck"
  checkForSizes (uncurry checkLexer) [(s, floor (logBase (2 :: Double) (fromIntegral s)):: Int) | s <- sizes, s > 0, s < 1000]
  checkForSizes (checkParsedPrettyAST True) sizes
  checkForSizes checkSemTypesAST sizes
  checkForSizes checkSemScopesAST sizes
  putStrLn "Finished testing!" where
    sizes = [0, 2, 4, 8, 16, 32, 64, 128, 256]
