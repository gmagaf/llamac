module Common.DebugPrint (Debug (..), PrintConfig(..), debugPrintDef, debugWriteDef) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as TIO
import Text.Pretty.Simple (CheckColorTty(CheckColorTty),
                          OutputOptions(..),
                          StringOutputStyle (..),
                          defaultOutputOptionsDarkBg,
                          defaultOutputOptionsNoColor,
                          pPrintOpt, pShowOpt)
import System.Console.ANSI

-- Debug printing utils

defaultOptions :: Bool -> Bool -> OutputOptions
defaultOptions c p =
    let base = if c then defaultOutputOptionsDarkBg else defaultOutputOptionsNoColor
        opt = base { outputOptionsIndentAmount = 2
                   , outputOptionsCompact = True
                   , outputOptionsCompactParens = p
                   , outputOptionsInitialIndent = 0
                   , outputOptionsStringStyle = Literal
                   }
    in opt

data PrintConfig = PrintConfig { color :: Bool, wrapParens :: Bool }

class Show a => Debug a where
    debugPrint :: a -> IO ()
    debugPrint = debugPrintDef (PrintConfig { color = True, wrapParens = False })
    debugWrite :: String -> a -> IO ()
    debugWrite = debugWriteDef (PrintConfig { color = False, wrapParens = False })

debugPrintDef :: Show a => PrintConfig -> a -> IO ()
debugPrintDef conf = pPrintOpt CheckColorTty (defaultOptions (color conf) (wrapParens conf))

debugWriteDef :: Show a => PrintConfig -> String -> a -> IO ()
debugWriteDef conf f = TIO.writeFile f . pShowOpt (defaultOptions False (wrapParens conf))


instance Debug a => Debug [a] where
    debugPrint = debugPrintDef (PrintConfig { color = True, wrapParens = False })
    debugWrite = debugWriteDef (PrintConfig { color = True, wrapParens = False })

instance (Debug a, Debug b) => Debug (Either a b) where
    debugPrint = debugPrintDef (PrintConfig { color = True, wrapParens = False })
    debugWrite = debugWriteDef (PrintConfig { color = False, wrapParens = False })

instance Debug L.Text where
    debugPrint s = do
        setSGR [ SetColor Foreground Vivid Blue
               , SetConsoleIntensity BoldIntensity
               ]
        TIO.putStrLn s
        setSGR [Reset]
    debugWrite = TIO.writeFile