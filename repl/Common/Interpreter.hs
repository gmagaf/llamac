module Common.Interpreter (module Common.Interpreter) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad.State (MonadState (..), gets)

import Lexer.Lexer (AlexPosn, printPosn)
import Parser.ParserT (ParserT(..), pureParserT, runParserT, evalParserT, throw, catch)
import Parser.ParserM (Parser, runParser)
import Parser.ParserState (ParserState)

import Common.RunTimeEnv
import {-# SOURCE #-} Common.Value (Value)

-- This modules defines the most basic
-- data types around the interpreter
-- and utility monadic functions

type Interpreter a = ParserT RunTimeError InterpreterState IO a
type RunTimeEnv = RunTimeEnvF Value
type ActivationRecord = ActivationRecordF Value

data InterpreterState = InterpreterState
    { parser_state :: ParserState
    , run_time_env :: RunTimeEnv
    , code_file    :: Maybe String
    } deriving Show

getRunTime :: Interpreter RunTimeEnv
getRunTime = gets run_time_env

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
getCodeFile = gets code_file

readFromBuffer :: Int -> Interpreter String
readFromBuffer n = do
    rt <- getRunTime
    let b = inputBuffer rt
    let (res, b') = splitAt n b
    putRunTime rt{ inputBuffer = b' }
    return res

putRunTime :: RunTimeEnv -> Interpreter ()
putRunTime recs = do
    s <- get
    put s{run_time_env = recs}

putFramePointer :: ActivationRecord -> Interpreter ()
putFramePointer r = do
    rt <- getRunTime
    putRunTime rt{frame_pointer = r}

putCodeFile :: Maybe String -> Interpreter ()
putCodeFile f = do
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

writeToBuffer :: String -> Interpreter ()
writeToBuffer s = do
    rt <- getRunTime
    let b = inputBuffer rt
    putRunTime rt{ inputBuffer = b ++ s }

liftParser :: Parser a -> Interpreter a
liftParser p = pureParserT f where
    f s = let (res, ps) = runParser (parser_state s) p
          in (either (Left . ParserError. show) Right res, s{parser_state = ps})

throwRunTimeError :: RunTimeError -> Interpreter a
throwRunTimeError = throw

throwRunTime :: String -> Interpreter a
throwRunTime = throw . RunTimeError

throwRunTimeAtPosn :: String -> AlexPosn -> Interpreter a
throwRunTimeAtPosn s p = throwRunTime (s ++ " at " ++ printPosn p)

catchRunTimeError :: Interpreter a -> (RunTimeError -> Interpreter a) -> Interpreter a
catchRunTimeError = flip catch

runInterpreter :: InterpreterState -> Interpreter a -> IO (Either RunTimeError a, InterpreterState)
runInterpreter = flip runParserT

evalInterpreter :: InterpreterState -> Interpreter a -> IO (Either RunTimeError a)
evalInterpreter = flip evalParserT
