{
module Lexer.Lexer (Alex, runAlex, alexError,
                    alexGetInput, getColumnOfPosn, getLineOfPosn,
                    lexer, lexerLine, scanFile, lexerWrap) where

import Common.Token
import Text.Read (readMaybe)
import Control.Monad (when)
import Debug.Trace (trace)
}

%wrapper "monadUserState"
%encoding "utf-8"
%token "Token"

$lls      = a-z                   -- little letters
$uls      = A-Z                   -- upercase letters
$ls_ds    = [a-zA-Z0-9]           -- letters and digits
$digits   = [0-9]                 -- digits
$hex      = [0-9a-fA-f]           -- hex digits
$white    = [\ \t\r\n]            -- white characters

@escape   = \\n|\\t|\\r|\\0|\\\\|\\'|\\\"|\\x$hex$hex -- escape sequences "

rules :-
  <0> "and"                                     { keyword T_and }
  <0> "array"                                   { keyword T_array }
  <0> "begin"                                   { keyword T_begin }
  <0> "bool"                                    { keyword T_bool }
  <0> "char"                                    { keyword T_char }
  <0> "delete"                                  { keyword T_delete }
  <0> "dim"                                     { keyword T_dim }
  <0> "do"                                      { keyword T_do }
  <0> "done"                                    { keyword T_done }
  <0> "downto"                                  { keyword T_downto }
  <0> "else"                                    { keyword T_else }
  <0> "end"                                     { keyword T_end }
  <0> "false"                                   { keyword T_false }
  <0> "float"                                   { keyword T_float }
  <0> "for"                                     { keyword T_for }
  <0> "if"                                      { keyword T_if }
  <0> "in"                                      { keyword T_in }
  <0> "int"                                     { keyword T_int }
  <0> "let"                                     { keyword T_let }
  <0> "match"                                   { keyword T_match }
  <0> "mod"                                     { keyword T_mod }
  <0> "mutable"                                 { keyword T_mutable }
  <0> "new"                                     { keyword T_new }
  <0> "not"                                     { keyword T_not }
  <0> "of"                                      { keyword T_of }
  <0> "rec"                                     { keyword T_rec }
  <0> "ref"                                     { keyword T_ref }
  <0> "then"                                    { keyword T_then }
  <0> "to"                                      { keyword T_to }
  <0> "true"                                    { keyword T_true }
  <0> "type"                                    { keyword T_type }
  <0> "unit"                                    { keyword T_unit }
  <0> "while"                                   { keyword T_while }
  <0> "with"                                    { keyword T_with }
  <0> "->"                                      { keyword T_arrow }
  <0> "="                                       { keyword T_assign }
  <0> "|"                                       { keyword T_bar }
  <0> "+"                                       { keyword T_plus }
  <0> "-"                                       { keyword T_minus }
  <0> "*"                                       { keyword T_times }
  <0> "/"                                       { keyword T_div }
  <0> "+."                                      { keyword T_plus_float }
  <0> "-."                                      { keyword T_minus_float }
  <0> "*."                                      { keyword T_times_float }
  <0> "/."                                      { keyword T_div_float }
  <0> "**"                                      { keyword T_exp }
  <0> "!"                                       { keyword T_bang }
  <0> ";"                                       { keyword T_semicolon }
  <0> "&&"                                      { keyword T_and_op }
  <0> "||"                                      { keyword T_or_op }
  <0> "<>"                                      { keyword T_not_struct_eq_op }
  <0> "<"                                       { keyword T_less_than }
  <0> ">"                                       { keyword T_more_than }
  <0> "<="                                      { keyword T_less_than_eq }
  <0> ">="                                      { keyword T_more_than_eq }
  <0> "=="                                      { keyword T_nat_eq_op }
  <0> "!="                                      { keyword T_not_nat_eq_op }
  <0> ":="                                      { keyword T_assign_mutable }
  <0> "("                                       { keyword T_lparen }
  <0> ")"                                       { keyword T_rparen }
  <0> "["                                       { keyword T_lbracket }
  <0> "]"                                       { keyword T_rbracket }
  <0> ","                                       { keyword T_comma }
  <0> ":"                                       { keyword T_colon }
  <0> $lls+($ls_ds|_)*                          { identifiersAction T_id }
  <0> $uls+($ls_ds|_)*                          { identifiersAction T_id_constr }  -- identifiers for constructors
  <0> $digits+                                  { intAction }
  <0> $digits+\.$digits+([eE][\+\-]?$digits+)?  { floatAction }
  <0> \'([^\\\']|@escape)\'                     { charActionSimple }
  <0> \"([^\\\"]|@escape)*\"                    { stringActionSimple } --"
  <0> $white+                                   { skip }
  <0> \-\-.*                                    { skip }                           -- one line comment
  <0> "(*"                                      { beginComment }                   -- support for multiline nested comments
  <comment> "(*"                                { beginComment }
  <comment> "*)"                                { endComment }
  <comment> "*"|\(|$white                       { skip }
  <comment> [^\*\($white]+                      { skip }
  <0,comment> .                                 { unknownCharacter }               -- throw error when finding anything else

{


data AlexUserState = AlexUserState {commentDepth :: Int}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {commentDepth = 0}

getCommentDepth :: Alex Int
getCommentDepth = commentDepth <$> alexGetUserState

setCommentDepth :: Int -> Alex ()
setCommentDepth d = do
  state <- alexGetUserState
  alexSetUserState $ state{commentDepth = d}

getLineOfPosn :: AlexPosn -> Int
getLineOfPosn (AlexPn _ line _) = line

getColumnOfPosn :: AlexPosn -> Int
getColumnOfPosn (AlexPn _ _ col) = col

lexicalError :: AlexPosn -> String -> Alex a
lexicalError posn message = alexError $ position ++ message where
  position = "Lexical error at line: " ++ (show $ getLineOfPosn posn) ++
    " and column: " ++ (show $ getColumnOfPosn posn) ++ ". "

unknownCharacter :: AlexAction Token
unknownCharacter (posn, _, _, current_string) len =
  let lexeme = (take len current_string)
  in lexicalError posn ("Unknown character: " ++ lexeme)

alexEOF :: Alex Token
alexEOF = do
  code <- alexGetStartCode
  if code == comment
    then alexError "Lexical error: Reached end of file without closing all comments"
    else case code of
      0 -> return T_eof
      c -> alexError $ "Lexical error: Reached end of file in unsupported start code: " ++ (show c)


keyword :: Token -> AlexAction Token
keyword tokenConstr _ _ = return tokenConstr

identifiersAction :: (String -> Token) -> AlexAction Token
identifiersAction tokenConstr (_, _, _, current_string) len =
  return $ tokenConstr (take len current_string)

intAction :: AlexAction Token
intAction (posn, _, _, current_string) len =
  let lexeme = (take len current_string)
  in case readMaybe lexeme :: Maybe Int of
    Just v  -> return (T_const_int v)
    Nothing -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into an Int")

floatAction :: AlexAction Token
floatAction (posn, _, _, current_string) len =
  let lexeme = (take len current_string)
  in case readMaybe lexeme :: Maybe Float of
    Just v  -> return (T_const_float v)
    Nothing -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into a Float")

stringActionSimple :: AlexAction Token
stringActionSimple (_, _, _, current_string) len =
  let lexeme = (take len current_string)
  in return (T_const_string lexeme)

charActionSimple :: AlexAction Token
charActionSimple (_, _, _, current_string) len =
  let lexeme = (take len current_string)
  in return (T_const_char lexeme)

-- removeQuotes :: String -> Maybe String
-- removeQuotes ('\"':ls) = removeLastQuote ls where
--   removeLastQuote [] = Just []
--   removeLastQuote ('\"':[]) = Just []
--   removeLastQuote (_:[]) = Nothing
--   removeLastQuote (c:cs) = fmap (c:) (removeLastQuote cs)
-- removeQuotes _ = Nothing
--
-- stringAction :: AlexAction Token
-- stringAction (posn, prev_char, rest_bytes, current_string) len =
--   let lexeme = (take len current_string)
--   in case (trace lexeme $ removeQuotes lexeme) of
--     Just s -> return (T_const_string s)
--     Nothing -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into a String")
--
-- charAction :: AlexAction Token
-- charAction (posn, prev_char, rest_bytes, current_string) len =
--   let lexeme = (take len current_string)
--   in case lexeme of
--     '\'' : c : '\'' : [] -> return (T_const_char c)
--     otherwise -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into a Char")

beginComment :: AlexAction Token
beginComment input len = do
  alexSetStartCode comment
  d <- getCommentDepth
  setCommentDepth (d + 1)
  skip input len

endComment :: AlexAction Token
endComment input@(posn, _, _, _) len = do
  d <- getCommentDepth
  when (d == 1) (alexSetStartCode 0)
  if d > 0
    then setCommentDepth (d - 1)
    else lexicalError posn "A comment closed without being opened"
  skip input len

-- scan a string until EOF is encountered
lexer :: String -> Either String [Token]
lexer s = runAlex s gather where
  gather :: Alex [Token]
  gather = do
     t <- alexMonadScan
     case t of
       T_eof -> return [t]
       _     -> (t:) <$> gather

-- scan a file
scanFile f = do
  inp <- readFile f
  return $ lexer inp

lexerLine :: IO ()
lexerLine = do
  line <- getLine
  let res = lexer line
  case res of
    Left err -> putStrLn err
    Right tokens -> mapM_ (\t -> putStrLn $ "Token: " ++ (show t)) tokens

lexerWrap :: (Token -> Alex a) -> Alex a
lexerWrap cont = do
  a <- alexMonadScan
  cont a
}
