module Property.Utils (module Property.Utils) where

import Data.Bitraversable (bimapM)

import Test.QuickCheck
import System.Exit (exitFailure)

import Common.Source (Source(..))
import Common.Token (Identifier)
import Common.AST (Type(..), TypeF(..))
import Common.SymbolType
import Lexer.Lexer (AlexPosn(..))
import Semantics.Utils (SemanticTag (..), TypeInfo (..))

-- A wrapper for arbitrary positions

newtype ArbPosn = ArbPosn { arb_posn :: AlexPosn }
  deriving Show

instance Arbitrary ArbPosn where
   arbitrary = ArbPosn <$> (AlexPn <$> arbitrary <*> arbitrary <*> arbitrary)

arbTag :: TypeInfo -> Gen SemanticTag
arbTag t = do
  p <- arbitrary
  return SemTag { posn = arb_posn p, typeInfo = t }

logSize :: Integral p => p -> Int
logSize n = ceiling (logBase 2 (fromIntegral n + 1) :: Double) :: Int

-- Some type convertion utils

addPosn :: Identifier -> PosnId
addPosn i = PosnId {identifier = i, def_source = FileIn "test.llama", def_posn = AlexPn 0 0 0}

constUserT :: Identifier -> ConstType
constUserT i = ConstType (UserDefinedType $ addPosn i)

ctToType :: ConstType -> Gen (Type SemanticTag)
ctToType (ConstType tf) = do
    b <- arbTag NotTypable
    tf' <- bimapM (return . identifier) ctToType tf
    return (Type tf' b)

typeToCt :: Type b -> ConstType
typeToCt = typeTo addPosn

typeToSt :: Type b -> ConstType
typeToSt = typeTo addPosn

-- Some util functions for Gen

boundedListOf :: (Int, Int) -> Gen a -> Gen [a]
boundedListOf (l, u) gen = do
  k <- choose (l, u)
  vectorOf k gen

listGen :: Int -> Gen a -> Gen [a]
listGen = vectorOf

checkForSize :: (Gen a -> Property) -> Gen a -> Int -> IO Result
checkForSize prop gen size = quickCheckResult . prop $ resize size gen

checkForSizes :: (a -> IO Result) -> [a] -> IO ()
checkForSizes _ [] = return ()
checkForSizes t (x:xs) = do
  res <- t x
  if isSuccess res then checkForSizes t xs
  else do
    putStrLn "Test failed :("
    exitFailure
