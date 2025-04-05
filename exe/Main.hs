module Main (main) where

import Parser.Parser (parse)

main :: IO ()
main = do
  s <- getContents
  putStrLn $ show (parse s)
