module FrontEnd.ReplInput (ReplInput(..), ReplPreInput(..), DebugCmdOptions(..)) where

import Control.Monad (unless)
import Prelude hiding (all)
import Text.ParserCombinators.ReadP
    (ReadP, (<++), eof, get, look, manyTill, pfail, satisfy, skipSpaces, string)
import Text.Read (Read(readPrec), lift, readPrec_to_P)

import Common.AST (AST, Expr)
import Lexer.Lexer (AlexPosn)

-- This module defines the command line
-- input data types. Also, it implements
-- the read function for ReplPreInput

data ReplInput = Program (AST AlexPosn)
               | Expression (Expr AlexPosn)
               | Help
               | Load String
               | Script String
               | Reload
               | InferType (Expr AlexPosn)
               | Info String
               | Debug DebugCmdOptions
               | Failed String
               | Quit
    deriving Show

-- TODO: add more debug choices
data DebugCmdOptions = Symbols
    deriving Show

instance Read DebugCmdOptions where
    readPrec = lift
                (string "symbols" >> return Symbols)

data ReplPreInput = LlamaPreInput String
                  | HelpCmd
                  | LoadCmd String
                  | ScriptCmd String
                  | ReloadCmd
                  | InferTypeCmd String
                  | InfoCmd String
                  | DebugCmd DebugCmdOptions
                  | FailedCmd String
                  | QuitCmd
    deriving Show

instance Read ReplPreInput where
    readPrec =
        let nWhite :: ReadP Char
            nWhite = satisfy (not . (`elem` " \t\r\n"))
            tok :: ReadP [Char]
            tok = (do
                c <- nWhite
                (c:) <$> tok) <++
                return []
            readTok :: [Char] -> ReadP ()
            readTok s = do
                skipSpaces
                t <- tok
                unless (t == s) pfail
            all :: ReadP [Char]
            all  = manyTill get eof
            ret :: b -> ReadP b
            ret r = all >> return r
        in lift $
            (readTok ":h" >> ret HelpCmd) <++
            (readTok ":?" >> ret HelpCmd) <++
            (readTok ":help" >> ret HelpCmd) <++
            (readTok ":r" >> ret ReloadCmd) <++
            (readTok ":reload" >> ret ReloadCmd) <++
            (readTok ":q" >> ret QuitCmd) <++
            (readTok ":quit" >> ret QuitCmd) <++
            (do readTok ":l"
                skipSpaces
                f <- tok
                ret (LoadCmd f)) <++
            (do readTok ":load"
                skipSpaces
                f <- tok
                ret (LoadCmd f)) <++
            (do readTok ":s"
                skipSpaces
                f <- tok
                ret (ScriptCmd f)) <++
            (do readTok ":script"
                skipSpaces
                f <- tok
                ret (ScriptCmd f)) <++
            (do readTok ":t"
                skipSpaces
                llama <- all
                ret (InferTypeCmd llama)) <++
            (do readTok ":type"
                skipSpaces
                llama <- all
                ret (InferTypeCmd llama)) <++
            (do readTok ":i"
                skipSpaces
                name <- tok
                ret (InfoCmd name)) <++
            (do readTok ":info"
                skipSpaces
                name <- tok
                ret (InfoCmd name)) <++
            (do readTok ":d"
                skipSpaces
                opt <- readPrec_to_P readPrec 0
                ret (DebugCmd opt)) <++
            (do readTok ":debug"
                skipSpaces
                opt <- readPrec_to_P readPrec 0
                ret (DebugCmd opt)) <++
            (do skipSpaces
                l <- look
                if null l || head l /= ':'
                    then pfail
                    else do
                        c <- tok
                        ret (FailedCmd ("Either wrong usage of "
                            ++ c ++ " or command is not supported." ++
                            " Use :help command for help"))) <++
            (LlamaPreInput <$> all)