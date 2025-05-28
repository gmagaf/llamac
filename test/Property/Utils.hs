module Property.Utils (module Property.Utils) where

import Test.QuickCheck
import Common.Token
import Lexer.Lexer (AlexPosn(..))

-- Some util functions for Gen

boundedListOf :: (Int, Int) -> Gen a -> Gen [a]
boundedListOf (l, u) gen = do
  k <- choose (l, u)
  vectorOf k gen

checkForSize :: (Gen a -> Property) -> Gen a -> Int -> IO Result
checkForSize prop gen size = quickCheckResult . prop $ resize size gen

checkForSizes :: (Int -> IO Result) -> [Int] -> IO ()
checkForSizes _ [] = return ()
checkForSizes t (x:xs) = do
  res <- t x
  if isSuccess res then checkForSizes t xs
  else do
    putStrLn "Test failed :("

newtype ArbPosn = ArbPosn {arb_posn :: AlexPosn}
  deriving Show

instance Arbitrary ArbPosn where
   arbitrary = ArbPosn <$> (AlexPn <$> arbitrary <*> arbitrary <*> arbitrary)

-- Some arbitrary names and constants from a predefined set

arbitraryIdentifier :: Gen Identifier
arbitraryIdentifier = elements ["a", "b", "c", "d", "e", "f", "g", "foo", "bar", "main"]

arbitraryConstrIdentifier :: Gen ConstrIdentifier
arbitraryConstrIdentifier = elements ["A", "B", "C", "D", "Nil", "Cons", "Empty", "Tree"]

arbitraryIntConstant :: Gen IntConstant
arbitraryIntConstant = elements [0..42]

arbitraryFloatConstant :: Gen FloatConstant
arbitraryFloatConstant = elements [0.0, 2.56, 3.14, 0.420e+2, 42000.0e-3]

arbitraryCharConstant :: Gen CharConstant
arbitraryCharConstant = elements ["a", "7", "\\n", "\\\"", "\\xE9"]

arbitraryStringConstant :: Gen StringConstant
arbitraryStringConstant = elements ["foo", "bar", "Route66", "Name:\\t\\\"DouglasAdams\\\"\\nValue:\\t42\\n"]
