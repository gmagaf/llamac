module Semantics.SemanticState (SemanticState(..),
                                Unifier,
                                Constraints,
                                TypeConstraint(..),
                                initSemanticState) where

import Data.List (intercalate)
import qualified Data.Map as M

import Common.PrintAST (Pretty(pretty))
import Common.SymbolType (SymbolType (..), ConstType)
import Lexer.Lexer (AlexPosn, alexStartPos)

type Unifier = SymbolType -> Maybe SymbolType
type Constraints = M.Map Int [TypeConstraint]

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
  }

instance Show SemanticState where
  show s = "SemanticState {" ++
    "varTypeC = " ++ show (varTypeC s)
    ++ ", " ++ "posnOfSem = " ++ show (posnOfSem s)
    ++ ", " ++ "unifier = " ++ showUnifierVals (unifier s) (varTypeC s)
    ++ ", " ++ "constraints = " ++ showConstraints (constraints s)
    ++ "}"

showUnifierVals :: Unifier -> Int -> String
showUnifierVals _ 0 = "_"
showUnifierVals f n = intercalate ", " $ map (showUnifierVal f . TVar) [0..(n - 1)]

showUnifierVal :: Unifier -> SymbolType -> String
showUnifierVal f st = "U(" ++ pretty st ++ ") = " ++ val where
  val = maybe "Nothing" pretty (f st)

showConstraints :: Constraints -> String
showConstraints m = "[" ++ intercalate ", " (map showConstraint (M.toList m)) ++ "]"

showConstraint :: (Int, [TypeConstraint]) -> String
showConstraint (v, c) = pretty (TVar v) ++ ": " ++ show c

initSemanticState :: SemanticState
initSemanticState = SemanticState
                {
                  varTypeC = 0
                , posnOfSem = alexStartPos
                , unifier = initUnifier
                , constraints = M.empty
                }

initUnifier :: SymbolType -> Maybe SymbolType
initUnifier (SymType tf) = SymType <$> mapM initUnifier tf
initUnifier (TVar _) = Nothing
