module Main (main) where

import Criterion.Main

import Lexer.Lexer (lexer)
import Parser.Utils (parse, analyze)

-- A benchmark suite using the criterion library

fileEnv :: String -> IO String
fileEnv b = do
  readFile $ "test/resources/" ++ b ++ ".llama"

benchmarks :: [String]
benchmarks =  [ "helloWorld"
              , "hanoi"
              , "hanoiType"
              , "primes"
              , "reverse"
              , "bubbleSort"
              , "mean"
              , "arrayMult"
              , "binTrees"
              ]

-- Our benchmark harness.
main :: IO ()
main = defaultMain
  [ bgroup "lex" $ map (\b -> env (fileEnv b) (bench b . whnf lexer)) benchmarks
  , bgroup "parse" $ map (\b -> env (fileEnv b) (bench b . whnf parse)) benchmarks
  , bgroup "sem" $ map (\b -> env (fileEnv b) (bench b . whnf analyze)) benchmarks
  ]