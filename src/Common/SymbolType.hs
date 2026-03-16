{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.SymbolType (module Common.SymbolType) where

import qualified Data.Set as S
import Control.Monad ((>=>))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Bifunctor (Bifunctor(first))
import Data.Bitraversable (bimapM)

import Common.Source (Source(..), printSource)
import Common.Token (Identifier)
import Common.AST (Type(..), TypeF(..))
import Common.PrintAST (Pretty(pretty, prettyPrec))
import Lexer.Lexer (AlexPosn, printPosnShort, getColumnOfPosn)

-- A positioned identifier for user defined types with definition position info

data PosnId = PosnId
            { identifier :: Identifier
            , def_source :: Source
            , def_posn   :: AlexPosn
            }
    deriving (Show, Eq, Ord)

instance Pretty PosnId where
    pretty = pretty . identifier

printTypePosn :: PosnId -> String
printTypePosn p = case def_source p of
    ReplIn _ -> printSource (def_source p) ++ ":" ++ show (getColumnOfPosn . def_posn $ p)
    FileIn _ -> printSource (def_source p) ++ ":" ++ printPosnShort (def_posn p)

-- A representation for semantic types

-- Const types are types without type variables
newtype ConstType = ConstType { unConstType :: TypeF PosnId ConstType }
    deriving (Show, Eq, Ord)

instance Pretty ConstType where
    prettyPrec always d (ConstType t) = prettyPrec always d t

-- Some useful consts
intConstType :: ConstType
intConstType = ConstType IntType
floatConstType :: ConstType
floatConstType = ConstType FloatType
boolConstType :: ConstType
boolConstType = ConstType BoolType
charConstType :: ConstType
charConstType = ConstType CharType
unitConstType :: ConstType
unitConstType = ConstType UnitType
stringConstType :: ConstType
stringConstType = ConstType (ArrayType 1 (ConstType CharType))

-- Symbol Types are types including type variables
data SymbolType = SymType (TypeF PosnId SymbolType)
                | TVar Int
    deriving (Show, Eq)

instance Pretty SymbolType where
    prettyPrec always d (SymType t) = prettyPrec always d t
    prettyPrec _ _ (TVar i)         = showString $ "@" ++ show i

-- Type schemes are polymorphic types
data TypeScheme = MonoType SymbolType
                | AbsType Int TypeScheme
    deriving (Show, Eq)

instance Pretty TypeScheme where
    prettyPrec always d (MonoType t)  = prettyPrec always d t
    prettyPrec always d (AbsType v t) =
        showString ("forall @" ++ show v ++ ". ") .
        prettyPrec always d t

-- Abstract away the implementation details of different type data types

class Traversable f => TypeFWrapper f t | t -> f where
    -- The type for the identifiers used for user defined types
    type TypeId t
    -- Get out the TypeF functor wrapped in f
    out :: t -> f (TypeF (TypeId t) t)
    -- Get out the TypeF functor if possible else return t
    coAlg :: t -> Either t (TypeF (TypeId t) t)

class TypeFWrapper f t => TypeFixPoint f t | t -> f where
    -- Default fix for TypeF functor
    fix :: TypeF (TypeId t) t -> t
    -- Fix for TypeF functor wrapped in f
    fixf :: f (TypeF (TypeId t) t) -> t

instance TypeFWrapper Identity (Type b) where
    type TypeId (Type b) = Identifier
    out (Type tf _) = Identity tf
    coAlg = Right . runIdentity . out

instance TypeFWrapper (Either Int) SymbolType where
    type TypeId SymbolType = PosnId
    out (SymType tf) = Right tf
    out (TVar v)     = Left v
    coAlg = either (Left . TVar) Right . out

instance TypeFWrapper Identity ConstType where
    type TypeId ConstType = PosnId
    out (ConstType tf) = Identity tf
    coAlg = Right . runIdentity . out

instance TypeFixPoint (Either Int) SymbolType where
    fix = SymType
    fixf = either TVar SymType

instance TypeFixPoint Identity ConstType where
    fix = ConstType
    fixf = ConstType . runIdentity

-- Some convertion utils
typeTo :: TypeFixPoint f t => (TypeId (Type b) -> TypeId t) -> Type b -> t
typeTo c = cata aux where
    aux (Identity tf) = fix $ first c tf

typeToM :: (Monad m, TypeFixPoint f t) => (TypeId (Type b) -> m (TypeId t)) -> Type b -> m t
typeToM c = cataM aux where
    aux (Identity tf) = fix <$> bimapM c pure tf

typeToParaM :: (Monad m, TypeFixPoint f t) => (Type b -> TypeId (Type b) -> m (TypeId t)) -> Type b -> m t
typeToParaM c = paraM2 aux where
    aux t (Identity tf) = fix <$> bimapM (c t) pure tf

constTypeToSymbolType :: ConstType -> SymbolType
constTypeToSymbolType (ConstType tf) = SymType $ fmap constTypeToSymbolType tf

-- Recursion Utils
bottomUp :: TypeFixPoint f t => (t -> t) -> t -> t
bottomUp alg = alg . fixf . fmap (fmap (bottomUp alg)) . out

bottomUpM :: (Monad m, TypeFixPoint f t) => (t -> m t) -> t -> m t
bottomUpM alg = mapM (mapM (bottomUpM alg)) . out >=> alg . fixf

cata :: TypeFWrapper f t => (f (TypeF (TypeId t) a) -> a) -> t -> a
cata alg = alg . fmap (fmap (cata alg)) . out

cataM :: (Monad m, TypeFWrapper f t) => (f (TypeF (TypeId t) a) -> m a) -> t -> m a
cataM alg = mapM (mapM (cataM alg)) . out >=> alg

para :: TypeFWrapper f t => (f (TypeF (TypeId t) (t, a)) -> a) -> t -> a
para alg = alg . fmap (fmap fanout) . out where
    fanout t = (t, para alg t)

paraM :: (Monad m, TypeFWrapper f t) => (f (TypeF (TypeId t) (t, a)) -> m a) -> t -> m a
paraM alg = mapM (mapM fanout) . out >=> alg where
    fanout t = do
        a <- paraM alg t
        return (t, a)

paraM2 :: (Monad m, TypeFWrapper f t) => (t -> f (TypeF (TypeId t) a) -> m a) -> t -> m a
paraM2 alg t = (mapM (mapM (paraM2 alg)) . out $ t) >>= alg t

-- Type theoretic utils
subst :: Int -> SymbolType -> SymbolType -> SymbolType
subst v t = bottomUp f where
    f :: SymbolType -> SymbolType
    f s'@(TVar u) | v == u = t
                  | otherwise = s'
    f s' = s'

substScheme :: Int -> SymbolType -> TypeScheme -> TypeScheme
substScheme v r (MonoType t) = MonoType $ subst v r t
substScheme v r (AbsType u t) | v == u    = AbsType u t
                              | otherwise = AbsType u $ substScheme v r t

tvarsInType :: SymbolType -> [Int]
tvarsInType st = cata (either (:) aux) st [] where
    aux :: TypeF p ([Int] -> [Int]) -> [Int] -> [Int]
    aux (FunType f g)   = f . g
    aux (ArrayType _ f) = f
    aux (RefType f)     = f
    aux _               = id

tvarsInScheme :: TypeScheme -> [Int]
tvarsInScheme (MonoType t)  = tvarsInType t
tvarsInScheme (AbsType _ t) = tvarsInScheme t

freeVarsInScheme :: TypeScheme -> [Int]
freeVarsInScheme t = S.toList $ S.difference allvars bound where
    allvars = S.fromList $ tvarsInScheme t
    bound = S.fromList (aux t)
    aux (MonoType _)  = []
    aux (AbsType v s) = v : aux s

tvarInType :: Int -> SymbolType -> Bool
tvarInType v (TVar u)
                | v == u = True
                | otherwise = False
tvarInType v (SymType t) = any (tvarInType v) t

notVarInType :: Int -> SymbolType -> Bool
notVarInType v = not . tvarInType v

-- Fun Types Utils
paramsToFun :: TypeFixPoint f t => [t] -> t -> t
paramsToFun [] o = o
paramsToFun (t:ts) o = fix (FunType t (paramsToFun ts o))

funToTypes :: TypeFWrapper f t => t -> [t]
funToTypes = reverse . aux [] where
    aux acc t = case coAlg t of
        Right (FunType t1 t2) -> aux (t1:acc) t2
        _                    -> t:acc

funToArgs :: TypeFWrapper f t => t -> [t]
funToArgs s =
    let ts = funToTypes s
        f [] = []
        f [_] = []
        f (x:xs) = x : f xs
    in if null ts then [] else f ts

outFunType :: TypeFWrapper f t => t -> t
outFunType t = case coAlg t of
    Right (FunType _ t2) -> outFunType t2
    _ -> t