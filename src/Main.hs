module Main (main) where

import Lexer.Lexer

main :: IO ()
main = do
  s <- getContents
  let res = lexer s
  case res of
    Left err -> putStrLn err
    Right tokens -> mapM_ (\token -> putStrLn $ "Token: " ++ (show token)) tokens
