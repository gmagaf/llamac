module Common.Interpreter (module Common.Interpreter) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Bifunctor
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE, catchE)
import Control.Monad.Trans.State (StateT(runStateT), get, put, runState, state, evalStateT)

import Common.Token (Identifier)
import Parser.ParserM (Parser, ParserT)
import Parser.ParserState (ParserState)

import Common.Value
import Common.PrintAST

-- This modules defines the most basic
-- data types around the interpreter

data ActivationRecord =
    Activation
    { offset :: Int -- This is only for debugging purposes
    , return_val :: Maybe Value
    , params :: M.Map Identifier Value
    , locals :: M.Map Identifier Value
    , control_link :: Maybe ActivationRecord
    , access_link :: Maybe ActivationRecord
    }
    | RecActivation
    { offset :: Int -- This is only for debugging purposes
    , return_val :: Maybe Value
    , params :: M.Map Identifier Value
    , locals :: M.Map Identifier Value
    , control_link :: Maybe ActivationRecord
    , access_link :: Maybe ActivationRecord
    } deriving Show

data RunTimeEnv = RunTimeEnv { frame_pointer :: ActivationRecord
                             , heap_address :: Int
                             , user_mallocs :: M.Map Int Bool
                             }
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

getAndIncrHeapAddress :: Interpreter Int
getAndIncrHeapAddress = do
    rt <- getRunTime
    let ha = heap_address rt
    putRunTime rt{ heap_address = ha + 1}
    return ha

getAndOffsetHeapAddress :: Int -> Interpreter Int
getAndOffsetHeapAddress n = do
    rt <- getRunTime
    let ha = heap_address rt
    putRunTime rt{ heap_address = ha + n}
    return ha

isAllocated :: Int -> Interpreter Bool
isAllocated ha = do
    allocated <- user_mallocs <$> getRunTime
    let alloc = M.lookup ha allocated
    return (fromMaybe True alloc) -- if it is not allocated from the user we assume it is allocated by the system

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

allocate :: Int -> Interpreter ()
allocate ha = do
    rt <- getRunTime
    putRunTime rt{ user_mallocs = M.insert ha True (user_mallocs rt) }

deallocate :: Int -> Interpreter ()
deallocate ha = do
    rt <- getRunTime
    putRunTime rt{ user_mallocs = M.update (const (Just False)) ha (user_mallocs rt) }

liftParser :: Parser a -> Interpreter a
liftParser p = ExceptT (state f) where
    f s = let (res, ps) = runState (runExceptT p) (parser_state s)
          in (either (Left . ParserError. show) Right res, s{parser_state = ps})

throwRunTimeError :: RunTimeError -> Interpreter a
throwRunTimeError = throwE

throwRunTime :: String -> Interpreter a
throwRunTime = throwE . RunTimeError

catchRunTimeError :: Interpreter a -> (RunTimeError -> Interpreter a) -> Interpreter a
catchRunTimeError = catchE

runInterpreter :: InterpreterState -> Interpreter a -> IO (Either RunTimeError a, InterpreterState)
runInterpreter s i = runStateT (runExceptT i) s

evalInterpreter :: InterpreterState -> Interpreter a -> IO (Either RunTimeError a)
evalInterpreter s i = evalStateT (runExceptT i) s

instance Pretty ActivationRecord where
    pretty ar =
        let prettyMaybe = maybe "null"
            prettyNestedAr nar = show (offset nar)
        in "|return_val: " ++ prettyMaybe pretty (return_val ar)
           ++ " | params: " ++ show (map (Data.Bifunctor.second pretty) $ M.toList (params ar))
           ++ " | locals: " ++ show (map (Data.Bifunctor.second show) $ M.toList (locals ar))
           ++ " | control_link: " ++ prettyMaybe prettyNestedAr (control_link ar)
           ++ " | access_link: " ++ prettyMaybe prettyNestedAr (access_link ar)
           ++ "|"

instance Pretty RunTimeEnv where
    pretty rtenv =
        let prettyAr ar = show (offset ar) ++ " * " ++ pretty ar
            traverseStack ar = prettyAr ar ++ maybe "" (("\n" ++) . traverseStack) (control_link ar)
        in traverseStack (frame_pointer rtenv)