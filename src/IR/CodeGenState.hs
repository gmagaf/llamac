module IR.CodeGenState (CodeGenState(..), initCodeGenState) where

import LLVM.IRBuilder (ModuleBuilderState, IRBuilderState, emptyIRBuilder, emptyModuleBuilder)

-- This module defines the state of the code generation

data CodeGenState = CodeGenState
    { moduleState  :: ModuleBuilderState
    , irState      :: IRBuilderState
    }

instance Show CodeGenState where
    show (CodeGenState {}) = "CodeGenState"

initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState
    { moduleState= emptyModuleBuilder
    , irState    = emptyIRBuilder
    }