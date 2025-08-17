module Property.Lexer.ArbitraryTokens (arbIdWithLength, arbConstrIdWithLength,
      arbitraryIdentifier, arbitraryConstrIdentifier, arbitraryIntConstant,
      arbitraryFloatConstant, arbitraryCharConstant, arbitraryStringConstant,
      arbTokens) where

import Test.QuickCheck
import Text.Read (readMaybe)
import Data.Char (chr)

import Common.Token
import Lexer.Lexer (parseHex)

import Property.Utils

arbIdWithLength :: Int -> Gen Identifier
arbIdWithLength l = suchThat ((:) <$> elements ['a'..'z'] <*> listGen (l - 1) g) (`notElem` keywords) where
  g = elements ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
  keywords = map show [T_and, T_array, T_begin, T_bool, T_char,
    T_delete, T_dim, T_do, T_done, T_downto, T_else, T_end, T_false,
    T_float, T_for, T_if, T_in, T_int, T_let, T_match, T_mod, T_mutable,
    T_new, T_not, T_of, T_rec, T_ref, T_then, T_to, T_true, T_type, T_unit,
    T_while, T_with]

arbConstrIdWithLength :: Int -> Gen Identifier
arbConstrIdWithLength l = (:) <$> elements ['A'..'Z'] <*> listGen (l - 1) g where
  g = elements ('_':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

arbIntWithLength :: Gen String
arbIntWithLength = sized $ \l -> listGen (max l 1) g where
  g = elements ['0'..'9']

arbFloatWithLength :: Gen String
arbFloatWithLength = sized $ \l -> do
  i <- resize (div l 3) arbIntWithLength
  d <- resize (div l 3) arbIntWithLength
  e <- elements ["e", "E"]
  s <- elements ["", "+", "-"]
  ex <- resize (div l 3) arbIntWithLength
  let f = i ++ "." ++ d
  let e' = e ++ s ++ ex
  elements [f, f ++ e']

arbCharUnQuoted :: Gen (Char, String)
arbCharUnQuoted = do
  c <- arbitraryCharConstant
  hex1 <- elements ['0'..'7']
  hex0 <- elements $ ['a'..'f'] ++ ['A'..'F'] ++ ['0'..'9']
  let hex = hex1:hex0:""
  let hexV = case parseHex hex of
        Just n -> n
        _      -> error $ "Failed to generate char value for hex code: " ++ hex
  special <- elements [('\n', "\\n"), ('\t', "\\t"), ('\r', "\\r"),
    ('\0', "\\0"), ('\\', "\\\\"), ('\'', "\\\'"), ('\"', "\\\""), (chr hexV, "\\x" ++ hex)]
  elements [(c, c:""), special]

arbChar :: Gen (Char, String)
arbChar = do
  (res, lexeme) <- arbCharUnQuoted
  return (res, "\'" ++ lexeme ++ "\'")

arbString :: Gen (String, String)
arbString = sized $ \l -> do
  chars <- listGen l arbCharUnQuoted
  let (res, lexeme) = foldl (\(cs, ls) (c, lx) -> (c:cs, lx++ls)) ("", []) chars
  return (res, "\"" ++ lexeme ++ "\"")

arbWhite :: Gen String
arbWhite = sized $ \l -> listGen l (elements [' ', '\t', '\r', '\n'])

arbComment :: Gen String
arbComment = sized $ \l -> do
  comment <- suchThat (map snd <$> listGen l arbCharUnQuoted) (not . aux . concat)
  let oneLine = "--" ++ concat comment ++ "\n"
  multiline <- mline l (concat comment)
  elements [oneLine, multiline] where
    aux [] = False
    aux ('(':'*':_) = True
    aux ('*':')':_) = True
    aux (_:cs) = aux cs
    mline l c | l <= 1 = return ("(* " ++ c ++ " *)")
    mline l c          = do
      inner <- mline (l - 1) c
      return ("(* " ++ c ++ " " ++ inner ++ " " ++ c ++ " *)")

arbKeyword :: Gen Token
arbKeyword = elements [T_and, T_array, T_begin, T_bool, T_char,
  T_delete, T_dim, T_do, T_done, T_downto, T_else, T_end, T_false,
  T_float, T_for, T_if, T_in, T_int, T_let, T_match, T_mod, T_mutable,
  T_new, T_not, T_of, T_rec, T_ref, T_then, T_to, T_true, T_type, T_unit,
  T_while, T_with]

arbOperator :: Gen Token
arbOperator = elements [T_arrow, T_equals, T_bar, T_plus, T_minus,
  T_times, T_div, T_plus_float, T_minus_float, T_times_float, T_div_float,
  T_exp, T_bang, T_semicolon, T_and_op, T_or_op, T_not_equals, T_less_than,
  T_greater_than, T_less_than_eq, T_greater_than_eq, T_nat_eq_op,
  T_not_nat_eq_op, T_assign_mutable]

arbSeparator :: Gen Token
arbSeparator = elements [T_lparen, T_rparen, T_lbracket, T_rbracket,
  T_comma, T_colon]

arbTokenLexeme :: Gen (Token, String)
arbTokenLexeme = sized $ \l -> do
  i <- arbIdWithLength l
  let ip = (T_id i, i)
  ci <- arbConstrIdWithLength l
  let cip = (T_id_constr ci, ci)
  n <- resize l arbIntWithLength
  let np = case readMaybe n :: Maybe IntConstant of
        Just nv -> (T_const_int nv, n)
        _       -> error $ "Failed to parse int: " ++ n
  f <- resize l arbFloatWithLength
  let fp = case readMaybe f :: Maybe FloatConstant of
        Just fv -> (T_const_float fv, f)
        _       -> error $ "Failed to parse float: " ++ f
  (c, cLex) <- resize l arbChar
  let cp = (T_const_char c, cLex)
  (s, sLex) <- resize l arbString
  let sp = (T_const_string s, sLex)
  k <- arbKeyword
  let kp = (k, show k)
  o <- arbOperator
  let op = (o, show o)
  sep <- arbSeparator
  let sepp = (sep, show sep)
  elements [ip, cip, np, fp, cp, sp, kp, op, sepp]

arbTokens :: Int -> Gen ([Token], String)
arbTokens l = do
  (ts, s) <- aux ([T_eof], id) l
  return (ts, s "") where
    aux :: ([Token], String -> String) -> Int -> Gen ([Token], String -> String)
    aux (acc, f) 0 = return (acc, f)
    aux (acc, f) n = do
      (t, lexeme) <- arbTokenLexeme
      wh <- oneof [arbWhite, arbComment]
      let sep = if last lexeme == '-' && head wh == '-' then " " else ""
      aux (t:acc, showString lexeme . showString (sep ++ wh) . f) (n - 1)

-- Some arbitrary names and constants from a predefined set

arbitraryIdentifier :: Gen Identifier
arbitraryIdentifier = elements ["a", "b", "c", "d", "e", "f", "g", "foo", "bar", "main"]

arbitraryConstrIdentifier :: Gen ConstrIdentifier
arbitraryConstrIdentifier = elements ["A", "B", "C", "D", "Nil", "Cons", "Empty", "Tree"]

arbitraryIntConstant :: Gen IntConstant
arbitraryIntConstant = elements [0..42]

arbitraryFloatConstant :: Gen FloatConstant
arbitraryFloatConstant = elements [0.0, 2.56, 3.14, 0.420e+2, 42000.0e-3]

arbitraryCharConstant :: Gen CharConstant
arbitraryCharConstant = elements [c | c <- ['!'..'~'], c /= '\"', c /= '\'', c /= '\\' ]

arbitraryStringConstant :: Gen StringConstant
arbitraryStringConstant = elements ["foo", "bar", "Route66"]
