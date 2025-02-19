module Main (main) where

import Lexer.Lexer
import Parser.Parser

testLexer :: String -> IO ()
testLexer s = do
  let res = lexer s
  case res of
    Left err -> putStrLn err
    Right tokens -> mapM_ (\token -> putStrLn $ "Token: " ++ (show token)) tokens


main :: IO ()
main = do
  s <- getContents
  testLexer s
  putStrLn $ show (test s)
