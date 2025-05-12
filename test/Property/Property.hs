module Property.Property (checkForSize,
                          checkForSizes,
                          parsedPrettyASTisAST,
                          semanticASTisOK,
                          checkParsedPrettyAST,
                          checkSemanticASTisOK) where

import Test.QuickCheck
    ( Gen,
      resize,
      forAll,
      isSuccess,
      quickCheckResult,
      Property,
      Result )
import Parser.Utils (parse)
import Common.AST (mapAST, AST)
import Common.PrintAST (prettyAST)
import Property.ArbitraryAST (arbitraryAST)
import Property.SemanticAST (semanticAST)
import Lexer.Lexer (AlexPosn (AlexPn))
import Semantics.Semantics (analyzeAST)
import Parser.ParserState (initParserState)
import Parser.ParserM (evalParser)

removeASTtags :: AST b -> AST ()
removeASTtags = mapAST (const ())

setDummyPosn :: AST b -> AST AlexPosn
setDummyPosn = mapAST (const $ AlexPn 0 0 0)

parsedPrettyASTisAST :: Show b => Gen (AST b) -> Property
parsedPrettyASTisAST gen =
  forAll gen (\p ->
    let s = prettyAST p
        ast = parse s
    in case ast of
        Right pp -> removeASTtags p == removeASTtags pp
        _        -> False)

semanticASTisOK :: (Show b) => Gen (AST b) -> Property
semanticASTisOK gen =
  forAll gen (\p ->
    let p' = setDummyPosn p
        parser = analyzeAST p'
        res = evalParser (initParserState "") parser
    in case res of
        Right _ -> True
        Left _  -> False)

checkForSize :: (Gen a -> Property) -> Gen a -> Int -> IO Result
checkForSize prop gen size = quickCheckResult . prop $ resize size gen

checkForSizes :: (Int -> IO Result) -> [Int] -> IO ()
checkForSizes _ [] = return ()
checkForSizes t (x:xs) = do
  res <- t x
  if isSuccess res then checkForSizes t xs
  else do
    putStrLn "Test failed :("

checkParsedPrettyAST :: Int -> IO Result
checkParsedPrettyAST n = do
 putStrLn $ "Testing property (parse . pretty $ AST == AST) for size: " ++ show n
 checkForSize parsedPrettyASTisAST (arbitraryAST :: Gen (AST ())) n

checkSemanticASTisOK :: Int -> IO Result
checkSemanticASTisOK n = do
 putStrLn $ "Testing property (analyzeAST $ (correct) AST == True) for size: " ++ show n
 checkForSize semanticASTisOK (semanticAST :: Gen (AST ())) n
 