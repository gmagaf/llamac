module Semantics.LetAnalysis (analyzeLet) where

import Common.AST (LetDef)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser)
import Semantics.Utils (SemanticTag)

-- This module contains the signature
-- of analyzeLet function to break the
-- cycle between LetAnalysis.hs and ExprAnalysis.hs

analyzeLet :: LetDef AlexPosn -> Parser (LetDef SemanticTag)
