module Unit.Unit (testParserSuite,
                  testParserGuidedSuite,
                  testSemSuite,
                  testSemGuidedSuite) where

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.QuickCheck (Gen, generate, vectorOf, elements)
import Unit.Parser.ExpectedASTs
import Unit.Semantics.SemanticTestSuites
import Unit.Semantics.AnalyzedASTs
import Common.FileUtils (readFileB)
import Common.AST (AST)
import Common.Source (Source (FileIn))
import Lexer.Lexer (AlexPosn)
import Parser.Utils (parse, analyze)
import Semantics.Utils (SemanticTag)

testGuidedParser :: (String, AST AlexPosn, FilePath) -> IO ()
testGuidedParser (descr, p, f) = do
  s <- readFileB f
  hspec $ do
    describe "Unit testing suite: (parse program == Expected AST)" $ do
      it descr $ do
        parse (FileIn f) s `shouldBe` Right p

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
    isCorrect descr s `shouldBe` True
  parserSpec ts where
      isCorrect f i = case parse (FileIn f) i of
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
      f <- readFileB $ "test/resources/1000-llamas/" ++ fileName
      return (fileName, f)

testGuidedSem :: (String, AST SemanticTag, FilePath) -> IO ()
testGuidedSem (descr, p, f) = do
  s <- readFileB f
  hspec $ do
    describe "Unit testing suite: (sem program == Expected AST)" $ do
      it descr $ do
        analyze (FileIn f) s `shouldBe` Right p

testSemGuidedSuite :: IO ()
testSemGuidedSuite = mapM_ testGuidedSem suite where
    suite = [ ("helloWorld.llama", helloWorldSemAST, "./test/resources/helloWorld.llama")
            , ("hanoi.llama", hanoiSemAST, "./test/resources/hanoi.llama")
            , ("hanoiType.llama", hanoiTypeSemAST, "./test/resources/hanoiType.llama")
            ]

semSpec :: String -> Int -> [(String, Bool)] -> Spec
semSpec _ _ [] = return ()
semSpec descr i ((p, expectation):ts) = do
  let src = descr ++ "-" ++ show i
  it src $ do
    isCorrect src p `shouldBe` expectation
  semSpec descr (i + 1) ts where
      isCorrect f pp = case analyze (FileIn f) pp of
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