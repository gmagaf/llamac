{-# LANGUAGE TemplateHaskell #-}
module IR.CodeGenState (CodeGenState, moduleState, irState, initCodeGenState) where

import Control.Lens (makeLenses)
import LLVM.IRBuilder (ModuleBuilderState (..), IRBuilderState (..), emptyIRBuilder, emptyModuleBuilder)
import LLVM.IRBuilder.Internal.SnocList (SnocList(unSnocList))
import LLVM.Pretty (ppll)

import Common.DebugPrint (Debug(..), PrintConfig(..), debugMode)

-- This module defines the state of the code generation

data CodeGenState = CodeGenState
    { _moduleState  :: ModuleBuilderState
    , _irState      :: IRBuilderState
    }
makeLenses ''CodeGenState

instance Show CodeGenState where
    show (CodeGenState ms irs) = "CodeGenState {" ++
        "moduleStateGlobals = " ++ (show . map ppll . unSnocList . builderDefs $ ms)
        ++ ", " ++ "moduleStateTypes = " ++ show (fmap ppll . builderTypeDefs $ ms)
        ++ ", " ++ "irBuilderStateBBs = " ++ (show . map ppll . unSnocList . builderBlocks $ irs)
        ++ "}"

instance Debug CodeGenState where
  debugMode _ = Left (PrintConfig { color = True, wrapParens = True })

initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState
    { _moduleState = emptyModuleBuilder
    , _irState     = emptyIRBuilder
    }