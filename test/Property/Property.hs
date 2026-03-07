{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Property.Property (checkLexer,
                          checkParsedPrettyAST,
                          checkSemTypesAST,
                          checkSemScopesAST) where

import Test.QuickCheck (Gen, Property, Result, forAll)
import Test.Hspec (shouldBe)

import Common.Token (Token)
import Common.AST (AST)
import Common.PrintAST (Pretty(prettyP))
import Common.Source (Source (FileIn))
import Lexer.Lexer (lexer)
import Parser.Utils (parse)
import Parser.ParserState (initParserState)
import Parser.ParserM (evalParser)
import Semantics.Utils (SemanticTag(..))
import Semantics.Semantics (sem)

import Property.Utils (checkForSize)
import Property.Parser.ArbitraryAST (arbitraryAST, ArbPosn (arb_posn))
import Property.Semantics.SemanticAST (semanticTypesAST, semanticScopesAST)
import Property.Lexer.ArbitraryTokens (arbTokens)

-- This module defines the desired test properties and tests

-- Lexer tests
scannedTokensIsTokens :: Gen ([Token], String) -> Property
scannedTokensIsTokens gen =
  forAll gen (\(ts, input) ->
    let res = lexer input
    in res `shouldBe` Right ts)

checkLexer :: Int -> Int -> IO Result
checkLexer l n = do
  putStrLn $ "Testing property (lexer . show Token == Token) for " ++ show l ++ " tokens of size: " ++ show n
  checkForSize scannedTokensIsTokens (arbTokens l :: Gen ([Token], String)) n

-- Parser tests
removeASTtags :: AST b -> AST ()
removeASTtags = fmap (const ())

parsedPrettyASTisAST :: Show b => Bool -> Gen (AST b) -> Property
parsedPrettyASTisAST parens gen =
  forAll gen $ \p ->
    let s = prettyP False p
        ast = parse (FileIn "test.llama") s
        res = fmap removeASTtags ast
        sp = prettyP True p
        astp = parse (FileIn "test.llama") sp
        resp = fmap removeASTtags astp
    in if parens
       then (res, resp) `shouldBe` (Right (removeASTtags p), Right (removeASTtags p))
       else res `shouldBe` Right (removeASTtags p)

checkParsedPrettyAST :: Bool -> Int -> IO Result
checkParsedPrettyAST parens n = do
  if parens
    then putStrLn $ "Testing property (parse . prettyP False $ AST == parse . prettyP True $ AST == AST) for size: " ++ show n
    else putStrLn $ "Testing property (parse . prettyP " ++ show parens ++ " $ AST == AST) for size: " ++ show n
  checkForSize (parsedPrettyASTisAST parens) (arbitraryAST :: Gen (AST ())) n

-- Semantic tests
semanticASTisOK :: Gen (AST ArbPosn) -> Property
semanticASTisOK gen =
  forAll gen (\p ->
    let p' = fmap arb_posn p
        parser = sem p'
        semAst = evalParser (initParserState (FileIn "test.llama") "") parser
        res = fmap (fmap posn) semAst
    in res `shouldBe` Right p') -- check that semantic analysis only affects tags

checkSemTypesAST :: Int -> IO Result
checkSemTypesAST n = do
  putStrLn $ "Testing property (removeTags . analyzeAST typesAST == typesAST) for size: " ++ show n
  checkForSize semanticASTisOK (semanticTypesAST :: Gen (AST ArbPosn)) n

checkSemScopesAST :: Int -> IO Result
checkSemScopesAST n = do
  putStrLn $ "Testing property (removeTags . analyzeAST scopeAST == scopeAST) for size: " ++ show n
  checkForSize semanticASTisOK (semanticScopesAST :: Gen (AST ArbPosn)) n
