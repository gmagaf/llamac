module Property.Lexer.ArbitraryTokens (module Property.Lexer.ArbitraryTokens) where

import Test.QuickCheck
import Common.Token
import Property.Utils

arbIdWithLength :: Int -> Gen Identifier
arbIdWithLength l = (:) <$> elements ['a'..'z'] <*> listGen (l - 1) g where
  g = elements ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

arbConstrIdWithLength :: Int -> Gen Identifier
arbConstrIdWithLength l = (:) <$> elements ['A'..'Z'] <*> listGen (l - 1) g where
  g = elements ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

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
