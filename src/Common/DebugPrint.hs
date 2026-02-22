module Common.DebugPrint (Debug (..), PrintConfig(..)) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as TIO
import Text.Pretty.Simple (CheckColorTty(CheckColorTty),
                          OutputOptions(..),
                          StringOutputStyle (..),
                          ColorOptions (..), Style (..), Color (Blue), Intensity (Vivid),
                          defaultOutputOptionsDarkBg,
                          defaultColorOptionsDarkBg,
                          defaultOutputOptionsNoColor,
                          pPrintOpt, pShowOpt)
import qualified System.Console.ANSI as ANSI
import Debug.Trace (trace, traceM)

import Common.FileUtils (writeToFile)

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
    debugMode :: a -> Either PrintConfig String
    debug :: a -> b -> b
    debug a = trace (debugString a)
    debugM :: Monad m => a -> m ()
    debugM a = traceM (debugString a)
    debugPrint :: a -> IO ()
    debugPrint a = case debugMode a of
        Left conf -> debugPrintDef conf a
        Right str -> putStr str
    debugWrite :: String -> a -> IO ()
    debugWrite f a = case debugMode a of
        Left conf -> debugWriteDef conf f a
        Right str -> writeToFile f str

debugPrintDef :: Show a => PrintConfig -> a -> IO ()
debugPrintDef conf = pPrintOpt CheckColorTty (defaultOptions (color conf) (wrapParens conf))

debugWriteDef :: Show a => PrintConfig -> String -> a -> IO ()
debugWriteDef conf f = TIO.writeFile f . pShowOpt (defaultOptions False (wrapParens conf))

debugString :: Debug a => a -> String
debugString a = case debugMode a of
    Left conf -> L.unpack (pShowOpt (defaultOptions (color conf) (wrapParens conf)) a)
    Right str -> str

instance Debug a => Debug [a] where
    debugMode _ = Left (PrintConfig { color = True, wrapParens = False })

instance Debug L.Text where
    debugMode s = Right (L.unpack s)
    debug s = trace (L.unpack $ pShowOpt textOpt s)
    debugM s = traceM (L.unpack $ pShowOpt textOpt s)
    debugPrint s = do
        ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue
                    , ANSI.SetConsoleIntensity ANSI.BoldIntensity
                    ]
        TIO.putStrLn s
        ANSI.setSGR [ANSI.Reset]
    debugWrite = TIO.writeFile


textOpt :: OutputOptions
textOpt = defaultOutputOptionsDarkBg
    { outputOptionsColorOptions =
        Just (defaultColorOptionsDarkBg
                { colorString = Style
                                { styleColor = Just (Blue, Vivid)
                                , styleBold = True
                                , styleItalic = False
                                , styleUnderlined = False
                                }
                }
            )
    }