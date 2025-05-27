module Property.Lexer.ArbitraryTokens (arbIdWithLength, arbConstrIdWithLength) where

import Test.QuickCheck
import Common.Token
import Property.Utils

arbIdWithLength :: Int -> Gen Identifier
arbIdWithLength l = (:) <$> elements ['a'..'z'] <*> boundedListOf (l - 1, l - 1) g where
  g = elements ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

arbConstrIdWithLength :: Int -> Gen Identifier
arbConstrIdWithLength l = (:) <$> elements ['A'..'Z'] <*> boundedListOf (l - 1, l - 1) g where
  g = elements ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
