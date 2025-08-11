module Common.Value (module Common.Value) where

-- This module contains the signature
-- of Value data type to break the
-- cycle between Value.hs and Interpreter.hs

data Value

instance Show Value