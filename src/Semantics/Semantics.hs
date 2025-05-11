module Semantics.Semantics (Analyzable, sem, semTag,
                            TypeAble, infer, typeCheck,
                            analyzeAST) where

import Common.AST (Expr, Type, TypeDef, LetDef, AST, Node(..))
import Common.SymbolType (SymbolType)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser)
import Semantics.Utils (SemanticTag(..), TypeInfo (..), throwSemAtPosn)
import Semantics.TypeAnalysis (analyzeTypeDef, analyzeType)
import Semantics.ExprAnalysis (analyzeLet, analyzeExpr)

-- This module contains the semantic analysis of the nodes
-- and decorates them with the semantic tag

-- Organize the semantic analyzable nodes in a class
class Node f => Analyzable f where
    sem :: f AlexPosn -> Parser (f SemanticTag)
    semTag :: f AlexPosn -> Parser SemanticTag
    semTag f = tag <$> sem f

class Analyzable f => TypeAble f where
    infer :: f AlexPosn -> Parser SymbolType
    infer f = do
        tg <- semTag f
        case typeInfo tg of
            NodeType t -> return t
            _          -> throwSemAtPosn "Could not infer type of expr" (posn tg)
    typeCheck :: f AlexPosn -> SymbolType -> Parser Bool
    typeCheck f t = (t ==) <$> infer f

-- Functions for analyzing nodes
analyzeAST :: AST AlexPosn -> Parser (AST SemanticTag)
analyzeAST []                 = return []
analyzeAST (Left def : ast)   = (:) . Left <$> sem def <*> analyzeAST ast
analyzeAST (Right tDef : ast) = (:) . Right <$> sem tDef <*> analyzeAST ast

instance Analyzable TypeDef where
    sem = analyzeTypeDef
instance Analyzable Type where
    sem = analyzeType
instance Analyzable LetDef where
    sem = analyzeLet
instance Analyzable Expr where
    sem = analyzeExpr

instance TypeAble Expr where
