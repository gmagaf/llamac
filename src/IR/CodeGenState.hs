module IR.CodeGenState (CodeGenState(..), initCodeGenState) where

-- This module defines the state of the code generation

data CodeGenState = CodeGenState
    deriving Show

initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState