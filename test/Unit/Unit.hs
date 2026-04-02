module Unit.Unit (testPrettySuite,
                  testParserSuite,
                  testGuidedParser,
                  testSemSuite,
                  testGuidedSem) where

import Data.List (sort)
import Test.Hspec (Spec, describe, it, shouldBe, Expectation)
import Test.Hspec.Runner (hspecWith, defaultConfig)
import Test.Hspec.Api.Formatters.V1 (Formatter, useFormatter, checks)
import Test.QuickCheck (Gen, generate, vectorOf, elements)

import Common.FileUtils (readFileB)
import Common.AST (AST)
import Common.Source (Source (FileIn))
import Common.PrintAST (Pretty (pretty))
import Lexer.Lexer (AlexPosn)
import Parser.Utils (parse, analyze)
import Semantics.Utils (SemanticTag)

import Unit.PrintAST.PrettyASTSuites
import Unit.Parser.ExpectedASTs
import Unit.Semantics.SemanticTestSuites
import Unit.Semantics.AnalyzedASTs

-- Some utils

formatter :: Formatter
formatter = checks

hspec :: Spec -> IO ()
hspec = hspecWith (useFormatter ("checks-formatter", formatter) defaultConfig)

hspecWithDescr :: String -> Spec -> IO ()
hspecWithDescr descr spec = hspec $ do
    describe descr spec

hspecSuite :: String
            -> (d -> Int -> a -> String)
            -> (a -> Expectation)
            -> [(d, [a])]
            -> IO ()
hspecSuite descr label f suites = hspecWithDescr descr suiteSpecs where
  createSpec _ _ [] = return ()
  createSpec dSuite acc (a:as) = do
    it (label dSuite acc a) $ f a
    createSpec dSuite (acc + 1) as
  createSuite (dSuite, suite) = createSpec dSuite 0 suite
  suiteSpecs :: Spec
  suiteSpecs = mapM_ createSuite suites

-- Simple Unit Tests

-- Pretty Printing

testPrettySuite :: IO ()
testPrettySuite = do
  let descr = "Unit testing pretty printin suite: (print AST == Expected program)"
      label dspec c _ = dspec ++ "-" ++ show c
      expectation (ex, ast) = pretty ast `shouldBe` ex
  hspecSuite descr label expectation [("pretty-expr", prettyExpr)]

-- Parser
testParserSuite :: Int -> IO ()
testParserSuite k = do
  randNs <- generate $ vectorOf k (elements [1..1000] :: Gen Int)
  let ns = if k == 1000 then [1..1000] else sort randNs
  let descr = if k == 1000
      then "Unit testing suite: (parse program -> Correct Syntax)"
      else "Unit (random " ++ show k ++ ") testing suite: (parse program -> Correct Syntax)"
  suite <- mapM fun ns
  let label _ _ (fileName, _) = fileName
  let expectation (fileName, s) = isCorrect fileName s `shouldBe` True where
        isCorrect f i = case parse (FileIn f) i of
          Left _  -> False
          Right _ -> True
  hspecSuite descr label expectation [("", suite)] where
    fun n = do
      let fileName = "p" ++ show n ++ ".lla"
      f <- readFileB $ "test/resources/1000-llamas/" ++ fileName
      return (fileName, f)

-- Semantic Analysis
testSemSuite :: IO ()
testSemSuite = do
  let descr = "Unit testing suite: (analyze program -> Correct semantics)"
  let label d i _ = d ++ "-" ++ show i
  let expectation (p, ex) = isCorrect descr p `shouldBe` ex where
        isCorrect f pp = case analyze (FileIn f) pp of
          Left _  -> False
          Right _ -> True
  hspecSuite descr label expectation
    [ ("types", typeDefSuite)
    , ("let", letDefSuite)
    , ("let-rec", letRecSuites)
    , ("expr", exprSuites)
    ]

-- Guided Unit Tests

resourcePath :: String
resourcePath = "./test/resources/"

prepareSuite :: (String, e) -> IO (String, [((FilePath, String), e)])
prepareSuite (fileName, expected) = do
  let f = resourcePath ++ fileName
  s <- readFileB f
  return (fileName, [((f, s), expected)])

hspecGuidedSuite :: String -> (((String, String), e) -> Expectation) -> [(String, e)] -> IO ()
hspecGuidedSuite descr spec suite = do
  suite' <- mapM prepareSuite suite
  let label fname _ _ = fname
  hspecSuite descr label spec suite'

-- Parser
guidedParserSuite :: [(String, AST AlexPosn)]
guidedParserSuite =
        [ ("helloWorld.llama", helloWorldAST)
        , ("hanoi.llama", hanoiAST)
        , ("hanoiType.llama", hanoiTypeAST)
        , ("primes.llama", primesAST)
        , ("reverse.llama", reverseAST)
        , ("bubbleSort.llama", bubbleSortAST)
        , ("mean.llama", meanAST)
        , ("arrayMult.llama", arrayMultAST)
        , ("binTrees.llama", binTreesAST)
        ]

testGuidedParser :: IO ()
testGuidedParser = do
  let descr = "Unit testing suite: (parse program == Expected AST)"
  let expectation ((f, s), p) = parse (FileIn f) s `shouldBe` Right p
  hspecGuidedSuite descr expectation guidedParserSuite

-- Semantic Analysis
guidedSemSuite :: [(String, AST SemanticTag)]
guidedSemSuite =
        [ ("helloWorld.llama", helloWorldSemAST)
        , ("hanoi.llama", hanoiSemAST)
        , ("hanoiType.llama", hanoiTypeSemAST)
        , ("primes.llama", primesSemAST)
        , ("reverse.llama", reverseSemAST)
        , ("bubbleSort.llama", bubbleSortSemAST)
        , ("mean.llama", meanSemAST)
        , ("arrayMult.llama", arrayMultSemAST)
        , ("binTrees.llama", binTreesSemAST)
        ]

testGuidedSem :: IO ()
testGuidedSem = do
  let descr = "Unit testing suite: (sem program == Expected AST)"
  let expectation ((f, s), p) = analyze (FileIn f) s `shouldBe` Right p
  hspecGuidedSuite descr expectation guidedSemSuite
