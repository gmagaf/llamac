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
  keywords = map show [AndT, ArrayT, BeginT, BoolT, CharT,
    DeleteT, DimT, DoT, DoneT, DowntoT, ElseT, EndT, FalseT,
    FloatT, ForT, IfT, InT, IntT, LetT, MatchT, ModT, MutableT,
    NewT, NotT, OfT, RecT, RefT, ThenT, ToT, TrueT, TypeT, UnitT,
    WhileT, WithT]

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
arbKeyword = elements [AndT, ArrayT, BeginT, BoolT, CharT,
  DeleteT, DimT, DoT, DoneT, DowntoT, ElseT, EndT, FalseT,
  FloatT, ForT, IfT, InT, IntT, LetT, MatchT, ModT, MutableT,
  NewT, NotT, OfT, RecT, RefT, ThenT, ToT, TrueT, TypeT, UnitT,
  WhileT, WithT]

arbOperator :: Gen Token
arbOperator = elements [ArrowT, EqualsT, BarT, PlusT, MinusT,
  TimesT, DivT, PlusFloatT, MinusFloatT, TimesFloatT, DivFloatT,
  ExpT, BangT, SemicolonT, AndOpT, OrOpT, NotEqualsT, LessThanT,
  GreaterThanT, LessThanEqT, GreaterThanEqT, NatEqOpT,
  NotNatEqOpT, AssignMutableT]

arbSeparator :: Gen Token
arbSeparator = elements [LParenT, RParenT, LBracketT, RBracketT,
  CommaT, ColonT]

arbTokenLexeme :: Gen (Token, String)
arbTokenLexeme = sized $ \l -> do
  i <- arbIdWithLength l
  let ip = (IdT i, i)
  ci <- arbConstrIdWithLength l
  let cip = (IdConstrT ci, ci)
  n <- resize l arbIntWithLength
  let np = case readMaybe n :: Maybe IntConstant of
        Just nv -> (ConstIntT nv, n)
        _       -> error $ "Failed to parse int: " ++ n
  f <- resize l arbFloatWithLength
  let fp = case readMaybe f :: Maybe FloatConstant of
        Just fv -> (ConstFloatT fv, f)
        _       -> error $ "Failed to parse float: " ++ f
  (c, cLex) <- resize l arbChar
  let cp = (ConstCharT c, cLex)
  (s, sLex) <- resize l arbString
  let sp = (ConstStringT s, sLex)
  k <- arbKeyword
  let kp = (k, show k)
  o <- arbOperator
  let op = (o, show o)
  sep <- arbSeparator
  let sepp = (sep, show sep)
  elements [ip, cip, np, fp, cp, sp, kp, op, sepp]

arbTokens :: Int -> Gen ([Token], String)
arbTokens l = do
  (ts, s) <- aux ([EofT], id) l
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
