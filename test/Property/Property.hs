{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Property.Property (checkLexer,
                          checkParsedPrettyAST,
                          checkSemTypesAST,
                          checkSemScopesAST) where

import Test.QuickCheck (Gen, Property, Result, forAll)
import Test.Hspec (shouldBe)

import Common.Token (Token)
import Common.AST (AST, TypeF (UserDefinedType))
import Common.PrintAST (Pretty(prettyP))
import Common.Source (Source (FileIn))
import Common.SymbolType
import Lexer.Lexer (lexer, AlexPosn (AlexPn))
import Parser.Utils (parse)
import Parser.ParserState (initParserState)
import Parser.ParserM (evalParser)
import Semantics.Utils (SemanticTag(..), TypeInfo (NodeType, NotTypable, DefType))
import Semantics.Semantics (sem)

import Property.Utils (checkForSize)
import Property.Lexer.ArbitraryTokens (arbTokens)
import Property.Parser.ArbitraryAST (arbitraryAST)
import Property.Semantics.SemanticAST (semanticTypesAST)
import Property.Semantics.SemanticScopeAST (semanticScopesAST)

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
semanticASTisOK :: Gen (AST SemanticTag) -> Property
semanticASTisOK gen =
  forAll gen (\ast ->
    let p = fmap posn ast
        parser = sem p
        res = evalParser (initParserState (FileIn "test.llama") "") parser
        res' = fmap (fmap f) res
    in res' `shouldBe` Right ast) where
      removeDefPosn (SymType (UserDefinedType p)) = SymType (UserDefinedType p{ def_posn = AlexPn 0 0 0 })
      removeDefPosn t = t
      removeDefPosnST = bottomUp removeDefPosn
      f (SemTag p ti) =
        let ti' = case ti of
              NodeType st -> NodeType $ removeDefPosnST st
              DefType ts  -> DefType $ mapTypeScheme removeDefPosnST ts
              NotTypable  -> ti
        in SemTag p ti'

checkSemTypesAST :: Int -> IO Result
checkSemTypesAST n = do
  putStrLn $ "Testing property (analyzeAST . removeTags $ typesAST == typesAST) for size: " ++ show n
  checkForSize semanticASTisOK semanticTypesAST n

checkSemScopesAST :: Int -> IO Result
checkSemScopesAST n = do
  putStrLn $ "Testing property (analyzeAST . removeTags $ scopeAST == scopeAST) for size: " ++ show n
  checkForSize semanticASTisOK semanticScopesAST n
