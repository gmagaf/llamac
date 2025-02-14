{
module Main where

import Text.Read (readMaybe)
import Control.Monad (when)
import Debug.Trace
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
  <0> $lls+($ls_ds|_)*                          { identifiersAction T_id }
  <0> $uls+($ls_ds|_)*                          { identifiersAction T_id_constr }  -- identifiers for constructors
  <0> $digits+                                  { intAction }
  <0> $digits+\.$digits+([eE][\+\-]?$digits+)?  { realAction }
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
  sc <- alexGetStartCode
  case sc of
    0 -> return T_eof
    comment -> alexError "Lexical error: Reached end of file without closing all comments"


data Token = T_eof | T_and | T_array | T_begin | T_bool | T_char | T_delete | T_dim |
  T_do | T_done | T_downto | T_else | T_end | T_false | T_float |
  T_for | T_if | T_in | T_int | T_let | T_match | T_mod | T_mutable |
  T_new | T_not | T_of | T_rec | T_ref | T_then | T_to | T_true |
  T_type | T_unit | T_while | T_with |
  T_id String | T_id_constr String | T_const_int Int | T_const_real Float | T_const_char String | T_const_string String |
  T_arrow | T_assign | T_bar | T_plus | T_minus | T_times | T_div |
  T_plus_real | T_minus_real | T_times_real | T_div_real | T_exp |
  T_bang | T_semicolon | T_and_op | T_or_op | T_not_eq_op |
  T_less_than | T_more_than | T_less_than_eq | T_more_than_eq |
  T_equals | T_not_equals | T_assign_mutable | T_lparen | T_rparen |
  T_lbracket | T_rbracket | T_comma | T_colon
  deriving (Eq, Show)

keyword :: Token -> AlexAction Token
keyword token = \input -> \len -> Alex $ \state -> Right (state, token)

identifiersAction :: (String -> Token) -> AlexAction Token
identifiersAction tokenConstr (posn, prev_char, rest_bytes, current_string) len =
  return $ tokenConstr (take len current_string)

intAction :: AlexAction Token
intAction (posn, prev_char, rest_bytes, current_string) len =
  let lexeme = (take len current_string)
  in case readMaybe lexeme :: Maybe Int of
    Just v  -> return (T_const_int v)
    Nothing -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into an Int")

realAction :: AlexAction Token
realAction (posn, prev_char, rest_bytes, current_string) len =
  let lexeme = (take len current_string)
  in case readMaybe lexeme :: Maybe Float of
    Just v  -> return (T_const_real v)
    Nothing -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into a Float")

stringActionSimple :: AlexAction Token
stringActionSimple (posn, prev_char, rest_bytes, current_string) len =
  let lexeme = (take len current_string)
  in return (T_const_string lexeme)

charActionSimple :: AlexAction Token
charActionSimple (posn, prev_char, rest_bytes, current_string) len =
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
endComment input len = do
  d <- getCommentDepth
  when (d == 1) (alexSetStartCode 0)
  if d > 0
    then setCommentDepth (d - 1)
    else alexError "More closing comments than opening"
  skip input len

-- scan a string until EOF is encountered
lexer :: String -> Either String [Token]
lexer s = runAlex s gather  where
  gather :: Alex [Token]
  gather = do
     t <- alexMonadScan
     case t of
       T_eof     -> return [t]
       otherwise -> (t:) <$> gather

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
    Right tokens -> mapM_ (\token -> putStrLn $ "Token: " ++ (show token)) tokens

main :: IO ()
main = do
  s <- getContents
  let res = lexer s
  case res of
    Left err -> putStrLn err
    Right tokens -> mapM_ (\token -> putStrLn $ "Token: " ++ (show token)) tokens
}
