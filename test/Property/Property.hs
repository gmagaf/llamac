module Property.Property (checkLexer,
                          checkParsedPrettyAST,
                          checkSemTypesAST,
                          checkSemScopesAST) where

import Test.QuickCheck (Gen, Property, Result, forAll)

import Common.AST (mapAST, AST)
import Common.PrintAST (prettyAST)
import Common.SymbolType (Source (FileIn))
import Parser.Utils (parse)
import Parser.ParserState (initParserState)
import Parser.ParserM (evalParser)
import Semantics.Utils (SemanticTag(..))
import Semantics.Semantics (analyzeAST)

import Property.Utils (checkForSize)
import Property.Parser.ArbitraryAST (arbitraryAST, ArbPosn (arb_posn))
import Property.Semantics.SemanticAST (semanticTypesAST, semanticScopesAST)
import Common.Token (Token)
import Lexer.Lexer (lexer)
import Property.Lexer.ArbitraryTokens (arbTokens)

-- This module defines the desired test properties and tests

-- Lexer tests
scannedTokensIsTokens :: Gen ([Token], String) -> Property
scannedTokensIsTokens gen =
  forAll gen (\(ts, input) ->
    let res = lexer input
    in case res of
        Right resTs -> ts == resTs
        Left  _     -> False)

checkLexer :: Int -> Int -> IO Result
checkLexer l n = do
  putStrLn $ "Testing property (lexer . show Token == Token) for " ++ show l ++ " tokens of size: " ++ show n
  checkForSize scannedTokensIsTokens (arbTokens l :: Gen ([Token], String)) n

-- Parser tests
removeASTtags :: AST b -> AST ()
removeASTtags = mapAST (const ())

parsedPrettyASTisAST :: Show b => Gen (AST b) -> Property
parsedPrettyASTisAST gen =
  forAll gen (\p ->
    let s = prettyAST p
        ast = parse (FileIn "test.llama") s
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
        res = evalParser (initParserState (FileIn "test.llama") "") parser
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
