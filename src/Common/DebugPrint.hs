{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Common.DebugPrint (DebugPrint, debugPrint, debugIO) where

import qualified Data.Text.Lazy as L
import Text.Pretty.Simple (CheckColorTty(CheckColorTty),
                          OutputOptions(..),
                          StringOutputStyle (..),
                          defaultOutputOptionsDarkBg,
                          defaultOutputOptionsNoColor,
                          pPrintOpt, pShowOpt)
import System.Console.ANSI

class Show a => DebugPrint a where
    debugPrint :: a -> IO ()
    debugPrint = debugIO True False

instance DebugPrint a => DebugPrint [a]
instance (DebugPrint a, DebugPrint b) => DebugPrint (Either a b)

instance DebugPrint L.Text where
    debugPrint s = do
        setSGR [ SetColor Foreground Vivid Blue
               , SetConsoleIntensity BoldIntensity
               ]
        putStrLn $ L.unpack s
        setSGR [Reset]

-- Debug printing utils
debugText :: Show a => a -> L.Text
debugText = let smallIndent = defaultOutputOptionsDarkBg
                            { outputOptionsIndentAmount = 2
                            , outputOptionsStringStyle = Literal
                            }
             in pShowOpt smallIndent

debugIO :: Show a => Bool -> Bool -> a -> IO ()
debugIO color wrapParens =
    let base = if color then defaultOutputOptionsDarkBg else defaultOutputOptionsNoColor
        smallIndent = base
                        { outputOptionsIndentAmount = 2
                        , outputOptionsCompact = True
                        , outputOptionsCompactParens = wrapParens
                        , outputOptionsInitialIndent = 0
                        , outputOptionsStringStyle = Literal
                        }
    in pPrintOpt CheckColorTty smallIndent