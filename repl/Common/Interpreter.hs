module Common.Interpreter (module Common.Interpreter) where

import qualified Data.Map as M
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE, catchE)
import Control.Monad.Trans.State (StateT(runStateT), get, put, runState, state, evalStateT)

import Common.Token (Identifier)
import Parser.ParserM (Parser, ParserT)
import Parser.ParserState (ParserState)

import Common.Value

-- This modules defines the most basic
-- data types around the interpreter

data ActivationRecord =
    Activation
    { return_val :: Maybe Value
    , params :: M.Map Identifier Value
    , locals :: M.Map Identifier Value
    , control_link :: Maybe ActivationRecord
    , access_link :: Maybe ActivationRecord
    } deriving Show

newtype RunTimeEnv = RunTimeEnv { frame_pointer :: ActivationRecord }
    deriving Show

type Interpreter a = ParserT RunTimeError InterpreterState IO a

data InterpreterState = InterpreterState
    { parser_state :: ParserState
    , run_time_env :: RunTimeEnv
    , code_file    :: Maybe String
    } deriving Show

getRunTime :: Interpreter RunTimeEnv
getRunTime = run_time_env <$> lift get

getFramePointer :: Interpreter ActivationRecord
getFramePointer = frame_pointer <$> getRunTime

getCodeFile :: Interpreter (Maybe String)
getCodeFile = code_file <$> lift get

putRunTime :: RunTimeEnv -> Interpreter ()
putRunTime recs = lift $ do
    s <- get
    put s{run_time_env = recs}

putFramePointer :: ActivationRecord -> Interpreter ()
putFramePointer r = do
    rt <- getRunTime
    putRunTime rt{frame_pointer = r}

putCodeFile :: Maybe String -> Interpreter ()
putCodeFile f = lift $ do
    s <- get
    put s{code_file = f}

liftParser :: Parser a -> Interpreter a
liftParser p = ExceptT (state f) where
    f s = let (res, ps) = runState (runExceptT p) (parser_state s)
          in (either (Left . ParserError. show) Right res, s{parser_state = ps})

throwRunTimeError :: RunTimeError -> Interpreter a
throwRunTimeError = throwE

catchRunTimeError :: Interpreter a -> (RunTimeError -> Interpreter a) -> Interpreter a
catchRunTimeError = catchE

runInterpreter :: InterpreterState -> Interpreter a -> IO (Either RunTimeError a, InterpreterState)
runInterpreter s i = runStateT (runExceptT i) s

evalInterpreter :: InterpreterState -> Interpreter a -> IO (Either RunTimeError a)
evalInterpreter s i = evalStateT (runExceptT i) s
