module Unit.Unit (testParserSuite,
                  testGuidedParser,
                  testSemSuite,
                  testGuidedSem) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (hspecWith, defaultConfig)
import Test.Hspec.Api.Formatters.V1 (Formatter, useFormatter, checks)
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

-- Some utils

formatter :: Formatter
formatter = checks

hspec :: Spec -> IO ()
hspec = hspecWith (useFormatter ("checks-formatter", formatter) defaultConfig)

hspecWithDescr :: String -> Spec -> IO ()
hspecWithDescr descr spec = hspec $ do
    describe descr spec

hspecSuite :: Foldable t => String -> (a -> Spec) -> t a -> IO ()
hspecSuite descr f suite = hspecWithDescr descr (mapM_ f suite)

-- Simple Unit Tests

-- Parser
parserSpec :: (String, String) -> Spec
parserSpec (descr, s) = do
  it descr $ do
    isCorrect descr s `shouldBe` True where
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
  hspecSuite descr parserSpec s where
    fun n = do
      let fileName = "p" ++ show n ++ ".lla"
      f <- readFileB $ "test/resources/1000-llamas/" ++ fileName
      return (fileName, f)

-- Semantic Analysis
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
  hspecWithDescr descr $ do
      semSpec "types" 0 typeDefSuite
      semSpec "let" 0 letDefSuite
      semSpec "let-rec" 0 letRecSuites
      semSpec "expr" 0 exprSuites

-- Guided Unit Tests

resourcePath :: String
resourcePath = "./test/resources/"

prepareSuite :: (String, e) -> IO (String, e, (String, String))
prepareSuite (fileName, expected) = do
  let f = resourcePath ++ fileName
  s <- readFileB f
  return (fileName, expected, (f, s))

hspecGuidedSuite :: Traversable t => String -> ((String, e, (String, String)) -> Spec) -> t (String, e) -> IO ()
hspecGuidedSuite descr spec suite = do
  suite' <- mapM prepareSuite suite
  hspecSuite descr spec suite'

-- Parser
guidedParserSuite :: [(String, AST AlexPosn)]
guidedParserSuite = [("helloWorld.llama", helloWorldAST)
        ,("hanoi.llama", hanoiAST)
        ,("hanoiType.llama", hanoiTypeAST)
        ,("primes.llama", primesAST)
        ,("reverse.llama", reverseAST)
        ,("bubbleSort.llama", bubbleSortAST)
        ,("mean.llama", meanAST)
        ,("arrayMult.llama", arrayMultAST)
        ,("binTrees.llama", binTreesAST)
        ]

guidedParserSpec :: (String, AST AlexPosn, (String, String)) -> Spec
guidedParserSpec (descr, p, (f, s)) = do
    it descr $ do
      parse (FileIn f) s `shouldBe` Right p

testGuidedParser :: IO ()
testGuidedParser = do
  let descr = "Unit testing suite: (parse program == Expected AST)"
  hspecGuidedSuite descr guidedParserSpec guidedParserSuite

-- Semantic Analysis
guidedSemSuite :: [(String, AST SemanticTag)]
guidedSemSuite =
        [ ("helloWorld.llama", helloWorldSemAST)
        , ("hanoi.llama", hanoiSemAST)
        , ("hanoiType.llama", hanoiTypeSemAST)
        ]

guidedSemSpec :: (String, AST SemanticTag, (String, String)) -> Spec
guidedSemSpec (descr, p, (f, s)) = do
    it descr $ do
      analyze (FileIn f) s `shouldBe` Right p

testGuidedSem :: IO ()
testGuidedSem = do
  let descr = "Unit testing suite: (sem program == Expected AST)"
  hspecGuidedSuite descr guidedSemSpec guidedSemSuite
