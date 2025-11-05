module IR.Utils (module IR.Utils) where

import Control.Lens (use)
import Data.String (fromString)
import qualified Data.Text.Internal.Lazy as T

import qualified LLVM.AST as L (Operand, Module (moduleName, moduleDefinitions), defaultModule)
import qualified LLVM.IRBuilder as L
import qualified LLVM.IRBuilder.Internal.SnocList as L
import LLVM.Pretty (ppllvm)

import Common.Token
import Parser.ParserM (Parser)
import Parser.ParserState (cgen_state)
import IR.CodeGenState (moduleState)

boolConst :: Bool -> L.Operand
boolConst True  = L.bit 1
boolConst False = L.bit 0

charConst :: CharConstant -> L.Operand
charConst = L.int8 . toInteger . fromEnum

intConst :: IntConstant -> L.Operand
intConst = L.int32 . toInteger

floatConst :: FloatConstant -> L.Operand
floatConst = L.single

codegenProgram :: String -> Parser T.Text
codegenProgram mname = do
    ms <- use (cgen_state . moduleState)
    let defs = L.getSnocList . L.builderDefs $ ms
    let mdl = mkModule defs
    return (ppllvm mdl) where
        mkModule ds = L.defaultModule { L.moduleName = fromString mname, L.moduleDefinitions = ds }
