module Common.SymbolType (module Common.SymbolType) where

import qualified Data.Set as S
import Common.AST (Type(..), TypeF(..))
import Common.PrintAST (Pretty(prettyPrec))


-- A representation for semantic types

-- Const types are types without type variables
newtype ConstType = ConstType (TypeF ConstType)
    deriving (Show, Eq)

instance Pretty ConstType where
    prettyPrec d (ConstType t) = prettyPrec d t

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
data SymbolType = SymType (TypeF SymbolType)
                | TVar Int
    deriving (Show, Eq)

instance Pretty SymbolType where
    prettyPrec d (SymType t) = prettyPrec d t
    prettyPrec _ (TVar i)    = showString $ "@" ++ show i

-- Some convertion utils
typeTo :: (TypeF a -> a) -> Type b -> a
typeTo alg (Type tf _) = alg (fmap (typeTo alg) tf)

constTypeToSymbolType :: ConstType -> SymbolType
constTypeToSymbolType (ConstType tf) = SymType $ fmap constTypeToSymbolType tf

-- Type schemes are polymorphic types
data TypeScheme = MonoType SymbolType
                | AbsType Int TypeScheme
    deriving (Show, Eq)

instance Pretty TypeScheme where
    prettyPrec d (MonoType t)  = prettyPrec d t
    prettyPrec d (AbsType v t) =
        showString ("forall @" ++ show v ++ ". ") .
        prettyPrec d t

-- Recursion Utils
bottomUp :: (SymbolType -> SymbolType) -> SymbolType -> SymbolType
bottomUp f (SymType t) = f (SymType $ fmap (bottomUp f) t)
bottomUp f v@(TVar _)  = f v

-- bottomUpM :: Monad m => (SymbolType -> m SymbolType) -> SymbolType -> m SymbolType
-- bottomUpM f (SymType t) = mapM (bottomUpM f) t >>= f . SymType
-- bottomUpM f v@(TVar _)  = f v

cata :: (TypeF a -> a, Int -> a) -> SymbolType -> a
cata alg@(f, _) (SymType t) = f $ fmap (cata alg) t
cata (_, g) (TVar v) = g v

cataM :: Monad m => (TypeF a -> m a, Int -> m a) -> SymbolType -> m a
cataM alg@(f, _) (SymType t) = mapM (cataM alg) t >>= f
cataM (_, g) (TVar v) = g v

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
tvarsInType st = cata (aux, (:)) st [] where
    aux :: TypeF ([Int] -> [Int]) -> [Int] -> [Int]
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
paramsToFun :: (TypeF a -> a) -> [a] -> a -> a
paramsToFun _ [] out       = out
paramsToFun alg (t:ts) out = alg (FunType t (paramsToFun alg ts out))

funToTypes :: (a -> Either (TypeF a) a) -> a -> [a]
funToTypes coalg = reverse . aux [] where
    aux acc t = case coalg t of
        Left (FunType t1 t2) -> aux (t1:acc) t2
        _                    -> t:acc

funToArgs :: (a -> Either (TypeF a) a) -> a -> [a]
funToArgs coalg s =
    let ts = funToTypes coalg s
        f [] = []
        f [_] = []
        f (x:xs) = x : f xs
    in if null ts then [] else f ts

outFunType :: (a -> Either (TypeF a) a) -> a -> a
outFunType out t = case out t of
    Left (FunType _ t2) -> outFunType out t2
    _ -> t

ctCoAlg :: ConstType -> Either (TypeF ConstType) ConstType
ctCoAlg (ConstType t) = Left t

stCoAlg :: SymbolType -> Either (TypeF SymbolType) SymbolType
stCoAlg (SymType t) = Left t
stCoAlg st          = Right st
