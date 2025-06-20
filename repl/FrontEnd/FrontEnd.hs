module FrontEnd.FrontEnd (module FrontEnd.FrontEnd) where

import System.IO (hFlush, stdout)
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Console.Haskeline
    (defaultSettings, getInputLine, runInputT)

import Common.PrintAST (pretty)
import qualified Common.AST as AST
import Lexer.Lexer (AlexPosn)
import Parser.Parser (calcRepl, calc)
import Parser.ParserState (initAlexState, initParserState)
import Parser.ParserM (putAlexState, getSymbols, throwInternalError)
import Parser.Utils (safeReadFile)
import Semantics.Utils (findName, getNodeType)
import Semantics.Semantics (analyzeAST, Analyzable (sem), TypeAble (infer))

import Common.Utils (initInterpreterState)
import Common.Interpreter
import BackEnd.Evaluation (runAST, evalExpr)
import FrontEnd.ReplInput (ReplPreInput (..), ReplInput (..), DebugCmdOptions (..))

-- This module contains the frontEnd of the repl

-- Intepreter combinations
loadProgramOrExpr :: String -> Interpreter (AST.ProgramOrExpr AlexPosn)
loadProgramOrExpr s = liftParser $ do
    putAlexState (initAlexState s)
    calcRepl

parseAnalyzeRun :: Interpreter ()
parseAnalyzeRun = liftParser (calc >>= analyzeAST) >>= runAST

-- IO
getSafeCodeFromFile :: String -> IO (Maybe String)
getSafeCodeFromFile fileName = do
    fop <- safeReadFile fileName
    let l err = putStrLn ("Failed to read file: " ++ fileName) >> putStrLn err >> return Nothing
    either l (return . Just) fop

getCodeFromFile :: String -> IO String
getCodeFromFile fileName = do
    fop <- getSafeCodeFromFile fileName
    maybe (return "") return fop

promptLlama' :: IO String
promptLlama' = do
    putStr "llama> "
    hFlush stdout
    getLine

promptLlama :: IO String
promptLlama = runInputT defaultSettings r where
    r = do
        minput <- getInputLine "llama> "
        case minput of
            Just input -> return input
            Nothing    -> return ""

getReplPreInput :: IO ReplPreInput
getReplPreInput = do
    s <- promptLlama
    let i = read s :: ReplPreInput
    return i

-- Get input into the intperpreter monad
getReplInput :: Interpreter ReplInput
getReplInput = do
    pre <- liftIO getReplPreInput
    case pre of
        HelpCmd         -> return Help
        ReloadCmd       -> return Reload
        LoadCmd f       -> return (Load f)
        ScriptCmd f     -> return (Script f)
        InfoCmd n       -> return (Info n)
        DebugCmd opt    -> return (Debug opt)
        FailedCmd err   -> return (Failed err)
        QuitCmd         -> return Quit
        InferTypeCmd s  -> do
            llama <- loadProgramOrExpr s
            case llama of
                AST.Program _    -> return (Failed "Can only infer the type of an expression")
                AST.Expression e -> return (InferType e)
        LlamaPreInput s -> do
            llama <- loadProgramOrExpr s
            case llama of
                AST.Program p    -> return (Program p)
                AST.Expression e -> return (Expression e)

-- Initialize repl and run repl
initRepl :: Maybe String -> IO ()
initRepl f = do
    input <- case f of
        Just fileName -> do
            putStrLn ("Loading file: " ++ fileName)
            getCodeFromFile fileName
        Nothing -> return ""
    let initState = initInterpreterState (initParserState input) f
    let catchError err = liftIO (print err >> putStrLn "Failed to load file")
    let fileInterpeter = catchRunTimeError parseAnalyzeRun catchError
    res <- evalInterpreter initState (fileInterpeter >> repl)
    return (seq res ())

-- The actual repl implementation
repl :: Interpreter ()
repl = catchRunTimeError loop (\e -> print' (show e) >> repl) where
    print' s = liftIO (putStrLn s)
    loop = do
        input <- getReplInput
        evaluate input
        case input of
            Quit -> return ()
            _    -> loop
    evaluate input = case input of
        Help -> do
            print' "HELP" -- TODO: write help message
        Debug Symbols -> do
            s <- liftParser getSymbols
            print' (pretty s)
        Failed err -> do
            print' (show err)
        Program p -> do
            semP <- liftParser (analyzeAST p)
            runAST semP
        Expression e -> do
            semE <- liftParser (sem e)
            v <- evalExpr semE
            t <- liftParser (getNodeType semE)
            print' (pretty v ++ " : " ++ pretty t)
        InferType e -> do
            semT <- liftParser (infer e)
            print' (pretty semT)
        Info n -> do
            entry <- liftParser (findName n)
            print' (pretty entry)
        Reload -> do
            f <- getCodeFile
            case f of
                Nothing -> do
                    print' "No recent files where found to reload"
                Just fileName -> do
                    code <- liftIO (getCodeFromFile fileName)
                    let newAlex = initAlexState code
                    liftParser (putAlexState newAlex)
                    parseAnalyzeRun
        Load f -> do
            fop <- liftIO (getSafeCodeFromFile f)
            case fop of
                Nothing -> return ()
                Just code -> do
                    putCodeFile (Just f)
                    let newAlex = initAlexState code
                    liftParser (putAlexState newAlex)
                    parseAnalyzeRun
        Script f -> do
            fop <- liftIO (getSafeCodeFromFile f)
            case fop of
                Nothing -> return ()
                Just code -> do
                    putCodeFile (Just f)
                    let newAlex = initAlexState code
                    liftParser (putAlexState newAlex)
                    parseAnalyzeRun
                    m <- loadProgramOrExpr "main"
                    case m of
                        AST.Expression e -> evaluate (Expression e)
                        _ -> liftParser (throwInternalError "Unexpected behaviour while running main")
        Quit -> return ()      
        