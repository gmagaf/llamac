module Semantics.Semantics (Analyzable, sem, semTag,
                            TypeAble, infer, typeCheck) where

import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Lens.Getter (view)
import Control.Lens.Setter (set)

import Common.AST (Expr, Type, TypeDef, LetDef, AST (AST), Node(..))
import Common.SymbolType (SymbolType)
import Lexer.Lexer (AlexPosn)
import Parser.ParserM (Parser)
import Semantics.Utils (SemanticTag(..), TypeInfo (..), throwSemAtPosn,
    resolveTableEntry, resolveType, resolveTypeScheme)
import Semantics.TypeAnalysis (analyzeTypeDef, analyzeType)
import Semantics.LetAnalysis (analyzeLet)
import Semantics.ExprAnalysis (analyzeExpr)
import Common.SymbolTable (basicInfo)
import Parser.SymbolTableUtils (overNamesM)

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
instance Analyzable AST where
    sem (AST ast p) = do
        -- Run semantic analysis on let and type definitions
        semAst <- mapM (bitraverse sem sem) ast
        -- Resolve let defs using the unifier
        finalAst <- mapM (bitraverse (mapM resolveTag) return) semAst
        -- Resolve all the def entries in symbol table
        overNamesM $ mapM (\fullEntry -> do
                    e <- resolveTableEntry (view basicInfo fullEntry)
                    return (set basicInfo e fullEntry))
        return $ AST finalAst (SemTag p NotTypable)

-- Util function to resolve the type of a tag
resolveTag :: SemanticTag -> Parser SemanticTag
resolveTag tg = case typeInfo tg of
    NodeType t -> do
        rt <- resolveType t
        return tg{typeInfo = NodeType rt}
    DefType t  -> do
        rt <- resolveTypeScheme t
        return tg{typeInfo = DefType rt}
    NotTypable -> return tg

instance Analyzable TypeDef where
    sem = analyzeTypeDef
instance Analyzable Type where
    sem = analyzeType
instance Analyzable LetDef where
    sem = analyzeLet
instance Analyzable Expr where
    sem = analyzeExpr

instance TypeAble Expr where
