module Property.Property (parsedPrettyASTisAST,
                          checkForSize,
                          checkForSizes,
                          checkParsedPrettyAST) where

import Test.QuickCheck
    ( Gen,
      resize,
      forAll,
      isSuccess,
      quickCheckResult,
      Property,
      Result )
import Parser.Parser (parse)
import Common.AST (mapAST, AST)
import Common.PrintAST (prettyAST)
import Property.ArbitraryAST (arbitraryAST)

removeASTtags :: AST b -> AST ()
removeASTtags = mapAST (const ())

parsedPrettyASTisAST :: Show b => Gen (AST b) -> Property
parsedPrettyASTisAST gen =
  forAll gen (\p ->
    let s = prettyAST p
        ast = parse s
    in case ast of
        Right pp -> removeASTtags p == removeASTtags pp
        _        -> False)

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
