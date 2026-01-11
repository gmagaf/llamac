module Common.Utils (module Common.Utils) where

import qualified Data.Map as M

import Parser.ParserState (ParserState)

import Common.Interpreter
import Common.RunTimeEnv
import BackEnd.RunTimeLib

-- Interpeter Utils module

initInterpreterState :: ParserState -> Maybe String -> InterpreterState
initInterpreterState p f =
    let global_frame = Activation
                    { offset = 0
                    , locals = M.fromList runTimeLib
                    , control_link = Nothing
                    , access_link = Nothing }
    in InterpreterState
    { parser_state = p
    , run_time_env = RunTimeEnv { frame_pointer = global_frame
                                , heap_address = 0
                                , user_mallocs = M.empty
                                , inputBuffer = "" }
    , code_file = f
    , line_no = 0
    }
