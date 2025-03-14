module Unit.Unit (testParserSuite,
                  testParserGuidedSuite) where

import Test.Hspec
import Test.QuickCheck
import Unit.TestExpectations
import Parser.Parser
import Common.AST

testGuidedParser :: (String, Program, FilePath) -> IO ()
testGuidedParser (descr, p, f) = do
  s <- readFile f
  hspec $ do
    describe "Unit testing suite: (parse program == Expected AST)" $ do
      it descr $ do
        parse s `shouldBe` Right p

testParserGuidedSuite :: IO ()
testParserGuidedSuite = mapM_ testGuidedParser suite where
    suite = [("helloWorld.llama", helloWorldAST, "./test/resources/helloWorld.llama"),
             ("hanoi.llama", hanoiAST, "./test/resources/hanoi.llama"),
             ("hanoiType.llama", hanoiTypeAST, "./test/resources/hanoiType.llama"),
             ("primes.llama", primesAST, "./test/resources/primes.llama")]

parserSpec :: [(String, String)] -> Spec
parserSpec [] = return ()
parserSpec ((descr, s):ts) = do
  it descr $ do
    isCorrect s `shouldBe` True
  parserSpec ts where
      isCorrect i = case parse i of
        Left _  -> False
        Right _ -> True

testParserSuite :: Int -> IO ()
testParserSuite k = do
  ns <- generate $ vectorOf k (elements [1..1000] :: Gen Int)
  s <- mapM fun ns
  hspec $ do
    describe "Unit (random) testing suite: (parse program -> Correct Syntax)" $ do
      parserSpec s where
    fun n = do
      let fileName = "p" ++ (show n) ++ ".lla"
      f <- readFile $ "test/resources/1000-llamas/" ++ fileName
      return (fileName, f)
