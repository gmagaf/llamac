module IR.TypeUtils (charStar, genConstType, genType, getArgStTypes, getRetStType) where

import qualified LLVM.AST.Type as L
import LLVM.Prelude (Word64)

import Common.Token (ConstrIdentifier)
import Common.AST (TypeF(..))
import Common.SymbolType (ConstType(..), SymbolType, cataM, cataUn, cataUnM, funToArgs, stCoAlg, outFunType)
import Common.SymbolTable (TypeTableEntry(..))
import Parser.ParserM (Parser, throwCGenError)
import Parser.SymbolTableUtils (queryTypeP)

{-
  This module contains helpful functions
  for generating llvm types from llama types
-}

charStar :: L.Type
charStar = L.ptr L.i8

-- Util for computing the size needed
-- to hold data of a type
constTypeSize :: ConstType -> Word64
constTypeSize = cataUn unConstType sizeOfType

sizeOfType :: TypeF Word64 -> Word64
sizeOfType tf = case tf of
    UnitType          -> 0
    BoolType          -> 1
    CharType          -> 1
    IntType           -> 4
    FloatType         -> 8
    FunType _ _       -> 8 -- the rest will be sized as pointers
    RefType _         -> 8 -- to the corresponding data in memory
    ArrayType _ _     -> 8
    UserDefinedType _ -> 8

-- Util for generating a constant type
genConstType :: ConstType -> Parser L.Type
genConstType = cataUnM unConstType genTypeF

genType :: SymbolType -> Parser L.Type
genType = cataM (genTypeF, \_ -> return charStar)

genTypeF :: TypeF L.Type -> Parser L.Type
genTypeF tf = case tf of
    UnitType                                -> return L.void
    BoolType                                -> return L.i1
    CharType                                -> return L.i8
    IntType                                 -> return L.i32
    FloatType                               -> return L.float
    FunType t1 (L.FunctionType rt2 argt2 _) -> return (L.FunctionType rt2 (t1:argt2) False)
    FunType t1 t2                           -> return (L.FunctionType t2 [t1] False)
    RefType L.VoidType                      -> return charStar -- (void *) is illegal in llvm
    RefType t                               -> return (L.ptr t)
    ArrayType _ t                           -> return (L.ptr t)  -- TODO: Define this
    UserDefinedType i                       -> do
        res <- queryTypeP i
        case res of
            Just (TypeEntry entry) -> do
                -- res = map (\(ci, ct) -> (ci, map genConstType ct)) tentry
                let sizeOfConstr :: (ConstrIdentifier, [ConstType]) -> Word64
                    sizeOfConstr = foldr (\t -> (constTypeSize t +)) 0 . snd
                    size = foldr (\c acc -> max acc (sizeOfConstr c)) 0 entry
                return $ L.StructureType True (L.i8:([L.ArrayType size L.i8 | size /= 0])) -- TODO: Define this
            Nothing -> throwCGenError ("Unable to find type: " ++ i ++ " to generate")


getArgStTypes :: SymbolType -> [SymbolType]
getArgStTypes = funToArgs stCoAlg

getRetStType :: SymbolType -> SymbolType
getRetStType = outFunType stCoAlg