module Main where

import Test.QuickCheck
import Lexer.Lexer
import Parser.Parser
import Common.AST
import Property.ArbitraryAST
import Property.PrintAST

parsedAstIsAst :: Gen Program -> Property
parsedAstIsAst gen =
  forAll gen (\p ->
    let s = prettyProgram p
        ast = runAlex s calc
    in case ast of
        Right pp -> p == pp
        _        -> False)

checkForSize :: Int -> IO ()
checkForSize size = quickCheck . parsedAstIsAst $ resize size arbitraryProgram

main :: IO ()
main = do
    putStrLn "Hello from tests!"
    checkForSize 30
