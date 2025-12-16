{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Semantics.TypeConstraints (TypeConstraint(..), mkAllowedTypes
                                 , Tag(..), ConstraintSet
                                 , emptyConstraintSet, singletonSet
                                 , union, traverse, showDPair
                                 , ConstraintsMap
                                 , showConstraintsMap, emptyConstraintsMap
                                 , lookupConstr, lookupConstrTg, insertConstr, insertConstrWith
                                 ) where

import Prelude hiding (traverse)
import qualified Data.Set as S
import qualified Data.Dependent.Map as DM
import qualified Data.Dependent.Sum as DS
import qualified Data.IntMap as IM (IntMap, foldrWithKey, empty, lookup, insertWith, insert)
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Constraint.Extras.TH (deriveArgDict)

import Common.SymbolType (ConstType, SymbolType (TVar))
import Common.PrintAST (pretty)

data TypeConstraintK = AllowedTypesT
                     | NotAllowedFunTypeT
                     | NotAllowedArrayTypeT
                     | ArrayOfAtLeastDimT
                     | AllowedUserDefinedTypeT
                     | NotPolymorphicVarT
    deriving Show

data Tag (tg :: TypeConstraintK) where
    AllowedTypesTg           :: Tag 'AllowedTypesT
    NotAllowedFunTypeTg      :: Tag 'NotAllowedFunTypeT
    NotAllowedArrayTypeTg    :: Tag 'NotAllowedArrayTypeT
    ArrayOfAtLeastDimTg      :: Tag 'ArrayOfAtLeastDimT
    AllowedUserDefinedTypeTg :: Tag 'AllowedUserDefinedTypeT
    NotPolymorphicVarTg      :: Tag 'NotPolymorphicVarT

deriving instance Show (Tag a)
deriveGEq      ''Tag
deriveArgDict  ''Tag
deriveGCompare ''Tag
deriveGShow    ''Tag

data TypeConstraint (tg :: TypeConstraintK) where
    AllowedTypes           :: S.Set ConstType -> String -> TypeConstraint 'AllowedTypesT
    NotAllowedFunType      :: String -> TypeConstraint 'NotAllowedFunTypeT
    NotAllowedArrayType    :: String -> TypeConstraint 'NotAllowedArrayTypeT
    ArrayOfAtLeastDim      :: Int -> String -> TypeConstraint 'ArrayOfAtLeastDimT
    AllowedUserDefinedType :: String -> TypeConstraint 'AllowedUserDefinedTypeT
    NotPolymorphicVar      :: String -> TypeConstraint 'NotPolymorphicVarT

deriving instance Show (TypeConstraint a)

showDPair :: DS.DSum Tag TypeConstraint -> String
showDPair (_ DS.:=> c) = show c

mkAllowedTypes :: [ConstType] -> String -> TypeConstraint 'AllowedTypesT
mkAllowedTypes types = AllowedTypes (S.fromList types)

newtype ConstraintSet = ConstraintSet { getMap :: DM.DMap Tag TypeConstraint }

instance Show ConstraintSet where
    show (ConstraintSet m) = "{" ++ fst (DM.foldrWithKey showVal ("}", True) m)
        where showVal _ v (acc, first) = (show v ++ (if first then "" else ", ") ++ acc, False)

emptyConstraintSet :: ConstraintSet
emptyConstraintSet = ConstraintSet DM.empty

getTag :: forall (c :: TypeConstraintK). TypeConstraint c -> Tag c
getTag (AllowedTypes {})           = AllowedTypesTg
getTag (NotAllowedFunType {})      = NotAllowedFunTypeTg
getTag (NotAllowedArrayType {})    = NotAllowedArrayTypeTg
getTag (ArrayOfAtLeastDim {})      = ArrayOfAtLeastDimTg
getTag (AllowedUserDefinedType {}) = AllowedUserDefinedTypeTg
getTag (NotPolymorphicVar {})      = NotPolymorphicVarTg

singletonSet :: forall (c :: TypeConstraintK). TypeConstraint c -> ConstraintSet
singletonSet tc = ConstraintSet $ DM.singleton (getTag tc) tc

combineConstrs :: forall v. Tag v -> TypeConstraint v -> TypeConstraint v -> TypeConstraint v
combineConstrs AllowedTypesTg (AllowedTypes s1 e1) (AllowedTypes s2 _)                             = AllowedTypes (S.intersection s1 s2) e1
combineConstrs NotAllowedFunTypeTg (NotAllowedFunType s) (NotAllowedFunType _)                     = NotAllowedFunType s
combineConstrs NotAllowedArrayTypeTg (NotAllowedArrayType s) (NotAllowedArrayType _)               = NotAllowedArrayType s
combineConstrs ArrayOfAtLeastDimTg (ArrayOfAtLeastDim d1 s1) (ArrayOfAtLeastDim d2 s2) | d1 < d2   = ArrayOfAtLeastDim d2 s2
                                                                                       | otherwise = ArrayOfAtLeastDim d1 s1
combineConstrs AllowedUserDefinedTypeTg (AllowedUserDefinedType s) (AllowedUserDefinedType _)      = AllowedUserDefinedType s
combineConstrs NotPolymorphicVarTg (NotPolymorphicVar s) (NotPolymorphicVar _)                     = NotPolymorphicVar s

union :: ConstraintSet -> ConstraintSet -> ConstraintSet
union s1 s2 = ConstraintSet $ DM.unionWithKey combineConstrs (getMap s1) (getMap s2)

traverse :: Applicative t => (forall c. Tag c -> TypeConstraint c -> t ()) -> ConstraintSet -> t ()
traverse f = DM.traverseWithKey_ f . getMap

type ConstraintsMap = IM.IntMap ConstraintSet

showConstraintsMap :: ConstraintsMap -> String
showConstraintsMap m = "[" ++ fst (IM.foldrWithKey showVal ("]", True) m)
        where showVal k v (acc, first) = (pretty (TVar k) ++ ": " ++ show v ++ (if first then "" else ", ") ++ acc, False)

emptyConstraintsMap :: ConstraintsMap
emptyConstraintsMap = IM.empty

lookupConstr :: Int -> ConstraintsMap -> Maybe ConstraintSet
lookupConstr = IM.lookup

lookupConstrTg :: forall c. Int -> Tag c -> ConstraintsMap -> Maybe (TypeConstraint c)
lookupConstrTg v tg m = do
    cSet <- lookupConstr v m
    DM.lookup tg (getMap cSet)

insertConstr :: Int -> ConstraintSet -> ConstraintsMap -> ConstraintsMap
insertConstr = IM.insert

insertConstrWith :: (ConstraintSet -> ConstraintSet -> ConstraintSet) -> Int -> ConstraintSet -> ConstraintsMap -> ConstraintsMap
insertConstrWith = IM.insertWith