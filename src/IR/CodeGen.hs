{-# LANGUAGE RecursiveDo #-}
module IR.CodeGen (genAST, genExpr) where

import Data.String (IsString(..))
import Control.Lens (use, (%=), view)

import qualified LLVM.AST as L (Operand (..), mkName)
import qualified LLVM.IRBuilder as L
import qualified LLVM.AST.Type as L

import Common.AST (Expr(..), ExprF (..), BinOp (..), LetDef (..), Def (FunDef), Param(..), NameDef (ide), AST)
import Common.SymbolTable (mkFullTableEntry, insert, names, TableEntry (ParamEntry, FunEntry), query, optInfo)
import Parser.SymbolTableUtils (openScopeInNames, closeScopeInNames)
import Parser.ParserState (symbols)
import Parser.ParserM (Parser, throwCGenError)
import Semantics.Utils (SemanticTag (..), inst, getNodeType, getDefScheme)
import IR.Utils
import IR.LLVMUtils (function, named)
import IR.TypeUtils


genAST :: AST SemanticTag -> Parser ()
genAST [] = return ()
genAST (Left def:ast) = genDefs def >> genAST ast
genAST (Right _:ast) = genAST ast -- TODO: Define types gen

genDefs :: LetDef SemanticTag -> Parser ()
genDefs (Let defs _) = do
    gens <- mapM genDef defs
    registerDefs defs gens
genDefs (LetRec defs _) = mdo
    registerDefs defs gens
    gens <- mapM genDef defs
    return ()

registerDefs :: [Def SemanticTag] -> [L.Operand] -> Parser ()
registerDefs [] _ = return ()
registerDefs (f@(FunDef fname params _ _ _):ds) ops = do -- TODO: Define register for the rest of the cases
    s <- getDefScheme f
    symbols . names %= insert fname (mkFullTableEntry (FunEntry s (map ide params)) (Just (head ops)))
    registerDefs ds (tail ops)

genParam :: Param SemanticTag -> Parser (L.Type, L.ParameterName)
genParam p = do
    t <- getNodeType p
    lt <- genType t
    return (lt, fromString (ide p))

genDef :: Def SemanticTag -> Parser L.Operand
genDef n@(FunDef fname params _ body _) = do -- TODO: Define defs gen
    scheme <- getDefScheme n
    st <- inst scheme
    let retsty = getRetStType st
    retty <- genType retsty
    genParams <- mapM genParam params
    function (L.mkName fname) genParams retty genBody where
        registerFormalParam :: (Param SemanticTag, L.Operand) -> Parser ()
        registerFormalParam (p, op) = do
            t <- getNodeType p
            symbols . names %= insert (ide p) (mkFullTableEntry (ParamEntry t (ide p)) (Just op))
        genBody :: [L.Operand] -> Parser ()
        genBody ops = do
            openScopeInNames
            let ps = zip params ops
            mapM_ registerFormalParam ps
            _entry <- L.block `named` "entry"
            res <- genExpr body
            L.ret res
            closeScopeInNames

genExpr :: Expr SemanticTag -> Parser L.Operand -- TODO: Define expr gen
genExpr (Expr ef _) = case ef of
    TrueCExpr    -> return . boolConst $ True
    FalseCExpr   -> return . boolConst $ False
    CharCExpr c  -> return . charConst $ c
    IntCExpr i   -> return . intConst $ i
    FloatCExpr f -> return . floatConst $ f
    BinOpExpr op e1 e2 -> genBinOp op e1 e2
    ConstExpr x -> do
                ns <- use (symbols . names)
                case query x ns of
                    Just fte -> case view optInfo fte of
                        Just o -> return o
                        Nothing -> throwCGenError $ "Const: " ++ x ++ " is not yet generated"
                    Nothing -> throwCGenError $ "Const: " ++ x ++ " is not yet in symbol table"

genBinOp :: BinOp -> Expr SemanticTag -> Expr SemanticTag -> Parser L.Operand
genBinOp op lhs rhs = case op of
    PlusOp -> do
        l <- genExpr lhs
        r <- genExpr rhs
        L.add l r
    MinusOp -> do
        l <- genExpr lhs
        r <- genExpr rhs
        L.sub l r
