module Property.Property (parsedPrettyASTisAST,
                          checkForSize,
                          checkForSizes,
                          checkParsedPrettyAST) where

import Test.QuickCheck
import Lexer.Lexer
import Parser.Parser
import Common.AST
import Common.PrintAST
import Property.ArbitraryAST

parsedPrettyASTisAST :: Gen Program -> Property
parsedPrettyASTisAST gen =
  forAll gen (\p ->
    let s = prettyProgram p
        ast = runAlex s calc
    in case ast of
        Right pp -> p == pp
        _        -> False)

checkForSize :: (Gen a -> Property) -> Gen a -> Int -> IO Result
checkForSize prop gen size = quickCheckResult . prop $ resize size gen

checkForSizes :: (Int -> IO Result) -> [Int] -> IO ()
checkForSizes _ [] = return ()
checkForSizes t (x:xs) = do
  res <- t x
  if isSuccess res then checkForSizes t xs
  else do
    putStrLn "Test failed :("

checkParsedPrettyAST :: Int -> IO Result
checkParsedPrettyAST n = do
 putStrLn $ "Testing property (parse . pretty $ AST == AST) for size: " ++ (show n)
 checkForSize parsedPrettyASTisAST arbitraryProgram n
