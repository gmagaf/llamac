module Main (main) where

import Parser.Utils (parse)

main :: IO ()
main = do
  s <- getContents
  putStrLn $ show (parse s)
