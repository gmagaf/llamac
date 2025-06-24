module Common.Utils (module Common.Utils) where

import qualified Data.Map as M

import Parser.ParserState (ParserState)

import Common.Interpreter
import BackEnd.RunTimeLib

-- Interpeter Utils module

-- TODO: initialize with all runtime functions when defined
initInterpreterState :: ParserState -> Maybe String -> InterpreterState
initInterpreterState p f =
    let global_frame = Activation
                    { offset = 0
                    , return_val = Nothing
                    , params = M.empty
                    , locals = M.fromList runTimeLib
                    , control_link = Nothing
                    , access_link = Nothing }
    in InterpreterState
    { parser_state = p
    , run_time_env = RunTimeEnv { frame_pointer = global_frame }
    , code_file = f
    }
