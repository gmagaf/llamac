module Unit.Unit (testParserSuite,
                  testParserGuidedSuite,
                  testSemSuite) where

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.QuickCheck (Gen, generate, vectorOf, elements)
import Unit.Parser.ExpectedASTs
import Unit.Semantics.SemanticTestSuites
import Lexer.Lexer (AlexPosn)
import Parser.Utils (parse, analyze)
import Common.AST (AST)

testGuidedParser :: (String, AST AlexPosn, FilePath) -> IO ()
testGuidedParser (descr, p, f) = do
  s <- readFile f
  hspec $ do
    describe "Unit testing suite: (parse program == Expected AST)" $ do
      it descr $ do
        parse s `shouldBe` Right p

testParserGuidedSuite :: IO ()
testParserGuidedSuite = mapM_ testGuidedParser suite where
    suite = [("helloWorld.llama", helloWorldAST, "./test/resources/helloWorld.llama")
            ,("hanoi.llama", hanoiAST, "./test/resources/hanoi.llama")
            ,("hanoiType.llama", hanoiTypeAST, "./test/resources/hanoiType.llama")
            ,("primes.llama", primesAST, "./test/resources/primes.llama")
            ,("reverse.llama", reverseAST, "./test/resources/reverse.llama")
            ,("bubbleSort.llama", bubbleSortAST, "./test/resources/bubbleSort.llama")
            ,("mean.llama", meanAST, "./test/resources/mean.llama")
            ,("arrayMult.llama", arrayMultAST, "./test/resources/arrayMult.llama")
            ,("binTrees.llama", binTreesAST, "./test/resources/binTrees.llama")
            ]

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
  randNs <- generate $ vectorOf k (elements [1..1000] :: Gen Int)
  let ns = if k == 1000 then [1..1000] else randNs
  let descr = if k == 1000
      then "Unit testing suite: (parse program -> Correct Syntax)"
      else "Unit (random " ++ show k ++ ") testing suite: (parse program -> Correct Syntax)"
  s <- mapM fun ns
  hspec $ do
    describe descr $ do
      parserSpec s where
    fun n = do
      let fileName = "p" ++ show n ++ ".lla"
      f <- readFile $ "test/resources/1000-llamas/" ++ fileName
      return (fileName, f)

semSpec :: String -> Int -> [(String, Bool)] -> Spec
semSpec _ _ [] = return ()
semSpec descr i ((p, expectation):ts) = do
  it (descr ++ "-" ++ show i) $ do
    isCorrect p `shouldBe` expectation
  semSpec descr (i + 1) ts where
      isCorrect pp = case analyze pp of
        Left _  -> False
        Right _ -> True

testSemSuite :: IO ()
testSemSuite = do
  let descr = "Unit testing suite: (analyze program -> Correct semantics)"
  hspec $ do
    describe descr $ do
      semSpec "types" 0 typeDefSuite
      semSpec "let" 0 letDefSuite
      semSpec "let-rec" 0 letRecSuites
      semSpec "expr" 0 exprSuites