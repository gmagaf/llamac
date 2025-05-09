module Common.SymbolType (
                        ConstType(..),
                        SymbolType(..),
                        TypeScheme(..),
                        typeToConstType,
                        typeToSymbolType,
                        constTypeToSymbolType) where

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