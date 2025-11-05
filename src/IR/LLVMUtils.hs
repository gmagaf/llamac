module IR.LLVMUtils (named, function) where

import Data.String (fromString)
import Control.Lens (view, use)
import Control.Lens.Combinators (set)
import qualified Data.Bifunctor as B

import LLVM.AST (Operand (..), Parameter (..), Definition (..), BasicBlock, Name (..), mkName)
import LLVM.AST.Type (Type (FunctionType), ptr)
import LLVM.AST.Global (Global(..))
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.IRBuilder as IR
import LLVM.IRBuilder.Internal.SnocList (getSnocList)

import Parser.ParserT (runParserT, ParserT, parserT)
import Parser.ParserM (Parser)
import Parser.ParserState (cgen_state, ParserState)
import IR.CodeGenState (irState)

{-
  This module contains re-implementations
  of some functions from the llvm-hs-pure
  library with minor modifications to fit
  the project's need
-}

-- Utility for assigning names
infixl 7 `named`
named :: IR.MonadIRBuilder m => m r -> String -> m r
m `named` nm = IR.named m (fromString nm)

-- byteStrName :: String -> ShortByteString
-- byteStrName = fromString

-- Utility for easily building global functions
functionDefaults :: L.Linkage -> Name -> [(Type, Name)] -> Type -> [BasicBlock] -> Global
functionDefaults l n ps ret bbs =
  Function {
    linkage = l,
    visibility = V.Default,
    dllStorageClass = Nothing,
    callingConvention = CC.C,
    returnAttributes = [],
    returnType = ret,
    name = n,
    parameters = ([Parameter ty pn [] | (ty, pn) <- ps], False),
    functionAttributes = [],
    section = Nothing,
    comdat = Nothing,
    alignment = 0,
    garbageCollectorName = Nothing,
    prefix = Nothing,
    basicBlocks = bbs,
    personalityFunction = Nothing,
    metadata = []
  }

-- Utility for declaring external functions
extern
  :: IR.MonadModuleBuilder m
  => String           -- ^ Definition identifier
  -> [(Type, String)] -- ^ Parameter types and identifiers pairs
  -> Type             -- ^ Type
  -> m Operand
extern nm argtys retty = do
  let funty = ptr $ FunctionType retty (map fst argtys) False
  IR.emitDefn $ GlobalDefinition (functionDefaults L.External (mkName nm) (map (B.second mkName) argtys) retty [])
  pure $ ConstantOperand $ C.GlobalReference funty (mkName nm)

-- Util to run a parser within a parser
-- with a given initial IRBuilderState
-- This is used to generate the basic blocks
-- from a parser
runIRBuilder :: Monad m => IR.IRBuilderState -> ParserT e ParserState m a -> ParserT e ParserState m (a, [BasicBlock])
runIRBuilder irs p = parserT $ \ps -> do
  ~(eitherNames, s') <- runParserT (p <* IR.block) (set (cgen_state . irState) irs ps)
  case eitherNames of
    Left e  -> return (Left e, s')
    Right a -> do
      let blocks = getSnocList . IR.builderBlocks . view (cgen_state . irState) $ s'
      return (Right (a, blocks), s')

-- Utility for emitting a function
function
  :: Name                       -- ^ Function name
  -> [(Type, IR.ParameterName)] -- ^ Parameter types and name suggestions
  -> Type                       -- ^ Return type
  -> ([Operand] -> Parser ())   -- ^ Function body builder
  -> Parser Operand
function label argtys retty body = do
  flabel <- case label of
    Name l   -> IR.fresh `IR.named` l
    UnName _ -> IR.fresh
  irs <- use (cgen_state . irState)
  let irs' = IR.emptyIRBuilder{
                  IR.builderUsedNames = IR.builderUsedNames irs,
                  IR.builderNameSuggestion = IR.builderNameSuggestion irs}
  ~(tyNamePairs, blocks) <- runIRBuilder irs' $ do
      (formalVars, tyNamePairs) <- genParams argtys
      body formalVars
      return tyNamePairs
  let def = GlobalDefinition (functionDefaults L.Internal flabel tyNamePairs retty blocks)
      funty = ptr $ FunctionType retty (fst <$> argtys) False
  IR.emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty flabel where
    genParams [] = return ([], [])
    genParams ((ty, n):ps) = do
      param <- case n of
          IR.NoParameterName -> IR.fresh
          IR.ParameterName p -> IR.fresh `IR.named` p
      (\(x, y) (xs, ys) -> (x:xs, y:ys)) (LocalReference ty param, (ty, param)) <$> genParams ps
