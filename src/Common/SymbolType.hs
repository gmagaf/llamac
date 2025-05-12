module Common.SymbolType (
                        ConstType(..),
                        SymbolType(..),
                        TypeScheme(..),
                        typeToConstType,
                        typeToSymbolType,
                        constTypeToSymbolType,
                        paramsToFunType,
                        funTypeToTypes,
                        funTypeToArgTypes,
                        paramsToConstFunType) where

import Common.AST (Type(..), TypeF(..))
import Common.PrintAST (Pretty(prettyPrec))


-- A representation for semantic types

newtype ConstType = ConstType (TypeF ConstType)
    deriving (Show, Eq)

instance Pretty ConstType where
    prettyPrec d (ConstType t) = prettyPrec d t

typeToConstType :: Type b -> ConstType
typeToConstType (Type tf _) = ConstType (fmap typeToConstType tf)


data SymbolType = SymType (TypeF SymbolType)
                | TVar Int
    deriving (Show, Eq)

instance Pretty SymbolType where
    prettyPrec d (SymType t) = prettyPrec d t
    prettyPrec _ (TVar i)    = showString $ "@" ++ show i

constTypeToSymbolType :: ConstType -> SymbolType
constTypeToSymbolType (ConstType tf) = SymType $ fmap constTypeToSymbolType tf

typeToSymbolType :: Type b -> SymbolType
typeToSymbolType (Type tf _) = SymType (fmap typeToSymbolType tf)


data TypeScheme = MonoType SymbolType
                | AbsType Int TypeScheme
    deriving Show

instance Pretty TypeScheme where
    prettyPrec d (MonoType t)  = prettyPrec d t
    prettyPrec d (AbsType v t) =
        showString ("forall @" ++ show v ++ ". ") .
        prettyPrec d t

paramsToFunType :: [SymbolType] -> SymbolType -> SymbolType
paramsToFunType [] out = out
paramsToFunType (t:ts) out = SymType (FunType t (paramsToFunType ts out))

funTypeToTypes :: SymbolType -> [SymbolType]
funTypeToTypes = reverse . aux [] where
    aux :: [SymbolType] -> SymbolType -> [SymbolType]
    aux acc (SymType (FunType t1 t2)) = aux (t1:acc) t2
    aux acc s = s:acc

funTypeToArgTypes :: SymbolType -> [SymbolType]
funTypeToArgTypes s =
    let ts = funTypeToTypes s
        f [] = []
        f [_] = []
        f (x:xs) = x : f xs
    in if ts == [] then [] else f ts

paramsToConstFunType :: [ConstType] -> ConstType -> ConstType
paramsToConstFunType [] out = out
paramsToConstFunType (t:ts) out = ConstType (FunType t (paramsToConstFunType ts out))
