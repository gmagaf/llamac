{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Common.DebugPrint (DebugPrint, debugPrint) where

import qualified Data.Text.Lazy as L
import Text.Pretty.Simple (CheckColorTty(CheckColorTty),
                          OutputOptions(outputOptionsIndentAmount, outputOptionsStringStyle),
                          StringOutputStyle (Literal),
                          defaultOutputOptionsDarkBg,
                          pPrintOpt, pShowOpt)
import Common.Token (Token)
import Common.AST (LetDef, TypeDef)

class Show a => DebugPrint a where
    debugPrint :: a -> IO ()
    debugPrint = debugIO

instance DebugPrint a => DebugPrint [a]

instance (DebugPrint a, DebugPrint b) => DebugPrint (Either a b)

instance DebugPrint L.Text where
    debugPrint = putStrLn . L.unpack

instance DebugPrint Token

instance (Show b) => DebugPrint (LetDef b)

instance (Show b) => DebugPrint (TypeDef b)

-- instance (Show b) => DebugPrint (AST b)

-- Debug printing utils
debugText :: Show a => a -> L.Text
debugText = let smallIndent = defaultOutputOptionsDarkBg
                            { outputOptionsIndentAmount = 2
                            , outputOptionsStringStyle = Literal
                            }
             in pShowOpt smallIndent

debugIO :: Show a => a -> IO ()
debugIO = let smallIndent = defaultOutputOptionsDarkBg
                            { outputOptionsIndentAmount = 2
                            , outputOptionsStringStyle = Literal
                            }
             in pPrintOpt CheckColorTty smallIndent