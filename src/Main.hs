module Main (main) where

import Parser.Parser

main :: IO ()
main = do
  s <- getContents
  putStrLn $ show (test s)
