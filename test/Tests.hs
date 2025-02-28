module Main (checkForSize, main) where

import Test.QuickCheck
import Lexer.Lexer
import Parser.Parser
import Common.AST
import Property.ArbitraryAST
import Property.PrintAST

parsedPrettyASTisAST :: Gen Program -> Property
parsedPrettyASTisAST gen =
  forAll gen (\p ->
    let s = prettyProgram p
        ast = runAlex s calc
    in case ast of
        Right pp -> p == pp
        _        -> False)

checkForSize :: (Gen a -> Property) -> Gen a -> Int -> IO ()
checkForSize prop gen size = quickCheck . prop $ resize size gen

checkParsedPrettyAST :: Int -> IO ()
checkParsedPrettyAST n = do
 putStrLn $ "Testing property (parsed . pretty $ AST == AST) for size: " ++ (show n)
 checkForSize parsedPrettyASTisAST arbitraryProgram n

main :: IO ()
main = do
  putStrLn "Hello from tests!"
  mapM_ checkParsedPrettyAST sizes where
     sizes = [0, 1, 2, 3, 4, 5, 10, 30, 100, 1000, 2000, 5000]
