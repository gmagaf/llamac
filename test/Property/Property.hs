module Property.Property (checkParsedPrettyAST,
                          checkSemTypesAST,
                          checkSemScopesAST) where

import Test.QuickCheck (Gen, Property, Result, forAll)

import Common.AST (mapAST, AST)
import Common.PrintAST (prettyAST)
import Parser.Utils (parse)
import Parser.ParserState (initParserState)
import Parser.ParserM (evalParser)
import Semantics.Utils (SemanticTag(..))
import Semantics.Semantics (analyzeAST)
import Property.Utils (checkForSize)
import Property.Parser.ArbitraryAST (arbitraryAST, ArbPosn (arb_posn))
import Property.Semantics.SemanticAST (semanticTypesAST, semanticScopesAST)

-- This module defines the desired test properties and tests

-- Parser tests
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

checkParsedPrettyAST :: Int -> IO Result
checkParsedPrettyAST n = do
  putStrLn $ "Testing property (parse . pretty $ AST == AST) for size: " ++ show n
  checkForSize parsedPrettyASTisAST (arbitraryAST :: Gen (AST ())) n

-- Semantic tests
semanticASTisOK :: Gen (AST ArbPosn) -> Property
semanticASTisOK gen =
  forAll gen (\p ->
    let p' = mapAST arb_posn p
        parser = analyzeAST p'
        res = evalParser (initParserState "") parser
    in case res of
        Right r -> mapAST posn r == p' -- check that semantic analysis only affects tags
        Left _  -> False)

checkSemTypesAST :: Int -> IO Result
checkSemTypesAST n = do
  putStrLn $ "Testing property (analyzeAST typesAST == True) for size: " ++ show n
  checkForSize semanticASTisOK (semanticTypesAST :: Gen (AST ArbPosn)) n

checkSemScopesAST :: Int -> IO Result
checkSemScopesAST n = do
  putStrLn $ "Testing property (analyzeAST scopeAST == True) for size: " ++ show n
  checkForSize semanticASTisOK (semanticScopesAST :: Gen (AST ArbPosn)) n
