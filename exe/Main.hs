module Main (main) where

import Parser.Utils (debug)

main :: IO ()
main = do
  s <- getContents
  debug s
