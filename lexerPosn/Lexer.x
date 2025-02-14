{
module Main where
}

%wrapper "posn"
--%encoding "latin-1" TODO: figure out encoding
%token "(Token, String, AlexPosn)"

$lls      = a-z                   -- little letters
$uls      = A-Z                   -- upercase letters
$ls_ds    = [a-zA-Z0-9]           -- letters and digits
$digits   = [0-9]                 -- digits
$hex      = [0-9a-fA-f]           -- hex digits
$white    = [\ \t\r\n]            -- white characters

@escape   = \\n|\\t|\\r|\\0|\\\\|\\'|\\\"|\\x$hex$hex -- escape sequences"

-- Each action has type :: AlexPosn -> String -> token where
-- data AlexPosn = AlexPn
--   !Int            -- absolute character offset
--   !Int            -- line number
--   !Int            -- column number

rules :-
  "and"                                     {\p -> \s -> (T_and, s, p)}
  "array"                                   {\p -> \s -> (T_array, s, p)}
  "begin"                                   {\p -> \s -> (T_begin, s, p)}
  "bool"                                    {\p -> \s -> (T_bool, s, p)}
  "char"                                    {\p -> \s -> (T_char, s, p)}
  "delete"                                  {\p -> \s -> (T_delete, s, p)}
  "dim"                                     {\p -> \s -> (T_dim, s, p)}
  "do"                                      {\p -> \s -> (T_do, s, p)}
  "done"                                    {\p -> \s -> (T_done, s, p)}
  "downto"                                  {\p -> \s -> (T_downto, s, p)}
  "else"                                    {\p -> \s -> (T_else, s, p)}
  "end"                                     {\p -> \s -> (T_end, s, p)}
  "false"                                   {\p -> \s -> (T_false, s, p)}
  "float"                                   {\p -> \s -> (T_float, s, p)}
  "for"                                     {\p -> \s -> (T_for, s, p)}
  "if"                                      {\p -> \s -> (T_if, s, p)}
  "in"                                      {\p -> \s -> (T_in, s, p)}
  "int"                                     {\p -> \s -> (T_int, s, p)}
  "let"                                     {\p -> \s -> (T_let, s, p)}
  "match"                                   {\p -> \s -> (T_match, s, p)}
  "mod"                                     {\p -> \s -> (T_mod, s, p)}
  "mutable"                                 {\p -> \s -> (T_mutable, s, p)}
  "new"                                     {\p -> \s -> (T_new, s, p)}
  "not"                                     {\p -> \s -> (T_not, s, p)}
  "of"                                      {\p -> \s -> (T_of, s, p)}
  "rec"                                     {\p -> \s -> (T_rec, s, p)}
  "ref"                                     {\p -> \s -> (T_ref, s, p)}
  "then"                                    {\p -> \s -> (T_then, s, p)}
  "to"                                      {\p -> \s -> (T_to, s, p)}
  "true"                                    {\p -> \s -> (T_true, s, p)}
  "type"                                    {\p -> \s -> (T_type, s, p)}
  "unit"                                    {\p -> \s -> (T_unit, s, p)}
  "while"                                   {\p -> \s -> (T_while, s, p)}
  "with"                                    {\p -> \s -> (T_with, s, p)}
  $lls+($ls_ds|_)*                          {\p -> \s -> (T_id, s, p)}
  $uls+($ls_ds|_)*                          {\p -> \s -> (T_id_constr, s, p)}
  $digits+                                  {\p -> \s -> (T_const_int, s, p)}
  $digits+\.$digits+([eE][\+\-]?$digits+)?  {\p -> \s -> (T_const_real, s, p)}
  \'([^\\\']|@escape)\'                     {\p -> \s -> (T_const_char, s, p)}
  \"([^\\\"]|@escape)*\"                    {\p -> \s -> (T_const_string, s, p)} --"
  "->"                                      {\p -> \s -> (T_arrow, s, p)}
  "="                                       {\p -> \s -> (T_assign, s, p)}
  "|"                                       {\p -> \s -> (T_bar, s, p)}
  "+"                                       {\p -> \s -> (T_plus, s, p)}
  "-"                                       {\p -> \s -> (T_minus, s, p)}
  "*"                                       {\p -> \s -> (T_times, s, p)}
  "/"                                       {\p -> \s -> (T_div, s, p)}
  "+."                                      {\p -> \s -> (T_plus_real, s, p)}
  "-."                                      {\p -> \s -> (T_minus_real, s, p)}
  "*."                                      {\p -> \s -> (T_times_real, s, p)}
  "/."                                      {\p -> \s -> (T_div_real, s, p)}
  "**"                                      {\p -> \s -> (T_exp, s, p)}
  "!"                                       {\p -> \s -> (T_bang, s, p)}
  ";"                                       {\p -> \s -> (T_semicolon, s, p)}
  "&&"                                      {\p -> \s -> (T_and_op, s, p)}
  "||"                                      {\p -> \s -> (T_or_op, s, p)}
  "<>"                                      {\p -> \s -> (T_not_eq_op, s, p)}
  "<"                                       {\p -> \s -> (T_less_than, s, p)}
  ">"                                       {\p -> \s -> (T_more_than, s, p)}
  "<="                                      {\p -> \s -> (T_less_than_eq, s, p)}
  ">="                                      {\p -> \s -> (T_more_than_eq, s, p)}
  "=="                                      {\p -> \s -> (T_equals, s, p)}
  "!="                                      {\p -> \s -> (T_not_equals, s, p)}
  ":="                                      {\p -> \s -> (T_assign_mutable, s, p)}
  "("                                       {\p -> \s -> (T_lparen, s, p)}
  ")"                                       {\p -> \s -> (T_rparen, s, p)}
  "["                                       {\p -> \s -> (T_lbracket, s, p)}
  "]"                                       {\p -> \s -> (T_rbracket, s, p)}
  ","                                       {\p -> \s -> (T_comma, s, p)}
  ":"                                       {\p -> \s -> (T_colon, s, p)}
  $white+                                   ;
  \-\-.*                                    ; -- one line comment
  "(*"([^\*]+|\*+[^\*\)])*\*+")"            ; -- multiline comments TODO: support nested comments
-- .                                        {ERROR}

{

data Token = T_and | T_array | T_begin | T_bool | T_char | T_delete | T_dim |
  T_do | T_done | T_downto | T_else | T_end | T_false | T_float |
  T_for | T_if | T_in | T_int | T_let | T_match | T_mod | T_mutable |
  T_new | T_not | T_of | T_rec | T_ref | T_then | T_to | T_true |
  T_type | T_unit | T_while | T_with |
  T_id | T_id_constr | T_const_int | T_const_real | T_const_char | T_const_string |
  T_arrow | T_assign | T_bar | T_plus | T_minus | T_times | T_div |
  T_plus_real | T_minus_real | T_times_real | T_div_real | T_exp |
  T_bang | T_semicolon | T_and_op | T_or_op | T_not_eq_op |
  T_less_than | T_more_than | T_less_than_eq | T_more_than_eq |
  T_equals | T_not_equals | T_assign_mutable | T_lparen | T_rparen |
  T_lbracket | T_rbracket | T_comma | T_colon
  deriving (Eq, Show)


lexer :: String -> [(Token, String, AlexPosn)]
lexer = alexScanTokens

main :: IO ()
main = do
  s <- getContents
  -- s <- getLine
  let tokens = lexer s
  mapM_ (\(token, lexeme, _) -> print $ "Token: " ++ (show token) ++ " with lexeme: " ++ lexeme) tokens

}
