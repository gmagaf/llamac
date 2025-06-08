module Semantics.SemanticState (SemanticState(..),
                                Unifier,
                                Constraints,
                                TypeConstraint(..),
                                addConstraints,
                                constraintsToList,
                                fromConstraint,
                                initSemanticState) where

import Data.List (intercalate, delete)
import qualified Data.Map as M
import qualified Data.Set as S

import Common.PrintAST (Pretty(pretty))
import Common.SymbolType (SymbolType (..), ConstType)
import Lexer.Lexer (AlexPosn, alexStartPos)

type Unifier = SymbolType -> Maybe SymbolType
type Constraints = M.Map Int TypeConstraints

data TypeConstraints = TypeConstraints
  {
    allowed_types :: Maybe ([ConstType], String)
  , allowed_fun   :: Maybe String
  , allowed_arr   :: Maybe String
  , allowed_dim   :: Maybe (Int, String)
  , allowed_user  :: Maybe String
  }

data TypeConstraint = AllowedTypes [ConstType] String
                    | NotAllowedFunType String
                    | NotAllowedArrayType String
                    | ArrayOfAtLeastDim Int String
                    | AllowedUserDefinedType String
    deriving (Show)

data SemanticState = SemanticState
  { varTypeC     :: Int         -- a counter to the var types used
  , posnOfSem    :: AlexPosn    -- a posn to the current place of analysis, used for error messages
  , unifier      :: Unifier     -- this should always be the most
                                -- general unifier and it should keep track
                                -- of variables not in scope. This should work
                                -- if all the fresh variables have a new name.
                                -- This is ensured by the varTypeC. However, the
                                -- result for each type in scope should be
                                -- a type in scope.
  , constraints  :: Constraints -- a map to hold all the constraints for each type var
  , freeTVars :: S.Set Int      -- a set of all free type variables at current scope
  }

instance Show SemanticState where
  show s = "SemanticState {" ++
    "varTypeC = " ++ show (varTypeC s)
    ++ ", " ++ "posnOfSem = " ++ show (posnOfSem s)
    ++ ", " ++ "unifier = " ++ showUnifierVals (unifier s) (varTypeC s)
    ++ ", " ++ "constraints = " ++ showConstraints (constraints s)
    ++ ", " ++ "freeTVarsInScope = " ++ showFreeTVars (freeTVars s)
    ++ "}"

showUnifierVals :: Unifier -> Int -> String
showUnifierVals _ 0 = "_"
showUnifierVals f n = intercalate ", " $ map (showUnifierVal f . TVar) [0..(n - 1)]

showUnifierVal :: Unifier -> SymbolType -> String
showUnifierVal f st = "U(" ++ pretty st ++ ") = " ++ val where
  val = maybe "Nothing" pretty (f st)

showConstraints :: Constraints -> String
showConstraints m = "[" ++ intercalate ", " (map showConstraint (M.toList m)) ++ "]"

showConstraint :: (Int, TypeConstraints) -> String
showConstraint (v, c) = pretty (TVar v) ++ ": " ++ show (constraintsToList c)

showFreeTVars :: S.Set Int -> String
showFreeTVars s = "[" ++ intercalate ", " (map (pretty . TVar) (S.toList s)) ++ "]"

constraintsToList :: TypeConstraints -> [TypeConstraint]
constraintsToList c =
  maybe id (\(ct, s) acc -> AllowedTypes ct s : acc) (allowed_types c) .
  maybe id (\s acc -> NotAllowedFunType s : acc) (allowed_fun c) .
  maybe id (\s acc -> NotAllowedArrayType s : acc) (allowed_arr c) .
  maybe id (\(d, s) acc -> ArrayOfAtLeastDim d s : acc) (allowed_dim c) .
  maybe id (\s acc -> AllowedUserDefinedType s : acc) (allowed_user c) $ []

emptyConstraints :: TypeConstraints
emptyConstraints = TypeConstraints Nothing Nothing Nothing Nothing Nothing

fromConstraint :: TypeConstraint -> TypeConstraints
fromConstraint c = case c of
  AllowedTypes ct s        -> emptyConstraints {allowed_types = Just (ct, s)}
  NotAllowedFunType s      -> emptyConstraints {allowed_fun = Just s}
  NotAllowedArrayType s    -> emptyConstraints {allowed_arr = Just s}
  ArrayOfAtLeastDim d s    -> emptyConstraints {allowed_dim = Just (d, s)}
  AllowedUserDefinedType s -> emptyConstraints {allowed_user = Just s}

addConstraints :: TypeConstraints -> TypeConstraints -> TypeConstraints
addConstraints c1 c2 =
  let same (x:xs) ys = if x `elem` ys
                 then x:same xs (delete x ys)
                 else same xs ys
      same [] _ = []
      add_allowed_types Nothing c = c
      add_allowed_types c Nothing = c
      add_allowed_types (Just (ts, s)) (Just (ts', _)) = Just (same ts ts', s)
      add_allowed_fun Nothing c = c
      add_allowed_fun (Just s) _ = Just s
      add_allowed_arr Nothing c = c
      add_allowed_arr (Just s) _ = Just s
      add_allowed_dim Nothing c = c
      add_allowed_dim c Nothing = c
      add_allowed_dim (Just (d, s)) (Just (d', s')) = Just (if d < d' then (d', s') else (d, s))
      add_allowed_user Nothing c = c
      add_allowed_user (Just s) _ = Just s
  in TypeConstraints
    {
      allowed_types = add_allowed_types (allowed_types c1) (allowed_types c2)
    , allowed_fun   = add_allowed_fun (allowed_fun c1) (allowed_fun c2)
    , allowed_arr   = add_allowed_arr (allowed_arr c1) (allowed_arr c2)
    , allowed_dim   = add_allowed_dim (allowed_dim c1) (allowed_dim c2)
    , allowed_user  = add_allowed_user (allowed_user c1) (allowed_user c2)
    }

initSemanticState :: SemanticState
initSemanticState = SemanticState
                {
                  varTypeC = 0
                , posnOfSem = alexStartPos
                , unifier = initUnifier
                , constraints = M.empty
                , freeTVars = S.empty
                }

initUnifier :: SymbolType -> Maybe SymbolType
initUnifier (SymType tf) = SymType <$> mapM initUnifier tf
initUnifier (TVar _) = Nothing
