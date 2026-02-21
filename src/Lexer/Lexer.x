{
{-# LANGUAGE StandaloneDeriving #-}
module Lexer.Lexer (Alex(Alex), AlexState(..), AlexPosn(AlexPn),
                    alexStartPos, alexInitUserState, alexMonadScan,
                    printPosn, getCurrentTokenPosn,
                    lexer, alexTokens, parseHex) where

import Text.Read (readMaybe)
import Data.Char (chr, isHexDigit)
import Control.Monad (when)
-- import Debug.Trace (trace)

import Common.Token (Token(..))
import Common.DebugPrint (Debug (debugMode), PrintConfig (..))
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

@escape   = \\n|\\t|\\r|\\0|\\\\|\\\'|\\\"|\\x$hex$hex -- escape sequences

rules :-
  <0> "and"                                     { keyword AndT }
  <0> "array"                                   { keyword ArrayT }
  <0> "begin"                                   { keyword BeginT }
  <0> "bool"                                    { keyword BoolT }
  <0> "char"                                    { keyword CharT }
  <0> "delete"                                  { keyword DeleteT }
  <0> "dim"                                     { keyword DimT }
  <0> "do"                                      { keyword DoT }
  <0> "done"                                    { keyword DoneT }
  <0> "downto"                                  { keyword DowntoT }
  <0> "else"                                    { keyword ElseT }
  <0> "end"                                     { keyword EndT }
  <0> "false"                                   { keyword FalseT }
  <0> "float"                                   { keyword FloatT }
  <0> "for"                                     { keyword ForT }
  <0> "if"                                      { keyword IfT }
  <0> "in"                                      { keyword InT }
  <0> "int"                                     { keyword IntT }
  <0> "let"                                     { keyword LetT }
  <0> "match"                                   { keyword MatchT }
  <0> "mod"                                     { keyword ModT }
  <0> "mutable"                                 { keyword MutableT }
  <0> "new"                                     { keyword NewT }
  <0> "not"                                     { keyword NotT }
  <0> "of"                                      { keyword OfT }
  <0> "rec"                                     { keyword RecT }
  <0> "ref"                                     { keyword RefT }
  <0> "then"                                    { keyword ThenT }
  <0> "to"                                      { keyword ToT }
  <0> "true"                                    { keyword TrueT }
  <0> "type"                                    { keyword TypeT }
  <0> "unit"                                    { keyword UnitT }
  <0> "while"                                   { keyword WhileT }
  <0> "with"                                    { keyword WithT }
  <0> "->"                                      { keyword ArrowT }
  <0> "="                                       { keyword EqualsT }
  <0> "|"                                       { keyword BarT }
  <0> "+"                                       { keyword PlusT }
  <0> "-"                                       { keyword MinusT }
  <0> "*"                                       { keyword TimesT }
  <0> "/"                                       { keyword DivT }
  <0> "+."                                      { keyword PlusFloatT }
  <0> "-."                                      { keyword MinusFloatT }
  <0> "*."                                      { keyword TimesFloatT }
  <0> "/."                                      { keyword DivFloatT }
  <0> "**"                                      { keyword ExpT }
  <0> "!"                                       { keyword BangT }
  <0> ";"                                       { keyword SemicolonT }
  <0> "&&"                                      { keyword AndOpT }
  <0> "||"                                      { keyword OrOpT }
  <0> "<>"                                      { keyword NotEqualsT }
  <0> "<"                                       { keyword LessThanT }
  <0> ">"                                       { keyword GreaterThanT }
  <0> "<="                                      { keyword LessThanEqT }
  <0> ">="                                      { keyword GreaterThanEqT }
  <0> "=="                                      { keyword NatEqOpT }
  <0> "!="                                      { keyword NotNatEqOpT }
  <0> ":="                                      { keyword AssignMutableT }
  <0> "("                                       { keyword LParenT }
  <0> ")"                                       { keyword RParenT }
  <0> "["                                       { keyword LBracketT }
  <0> "]"                                       { keyword RBracketT }
  <0> ","                                       { keyword CommaT }
  <0> ":"                                       { keyword ColonT }
  <0> $lls+($ls_ds|_)*                          { identifiersAction IdT }
  <0> $uls+($ls_ds|_)*                          { identifiersAction IdConstrT }  -- identifiers for constructors
  <0> $digits+                                  { intAction }
  <0> $digits+\.$digits+([eE][\+\-]?$digits+)?  { floatAction }
  <0> \'([^\\\']|@escape)\'                     { charAction }
  -- <0> \"([^\\\"]|@escape)*\"                    { stringAction }
  <0> \"                                        { beginString }
  <string> \"                                   { endString }
  <string> ([^\\\"]|@escape)                    { stringAction }
  <0> $white+                                   { skip }
  <0> \-\-.*                                    { skip }                           -- one line comment
  <0> "(*"                                      { beginComment }                   -- support for multiline nested comments
  <comment> "(*"                                { beginComment }
  <comment> "*)"                                { endComment }
  <comment> "*"|\(|$white                       { skip }
  <comment> [^\*\($white]+                      { skip }
  <0,string,comment> .                          { unknownCharacter }               -- throw error when finding anything else

{

-- Show and debugging state info
deriving instance Show AlexState

instance Debug AlexState where
  debugMode _ = Left (PrintConfig { color = True, wrapParens = True })

instance Ord AlexPosn where
    compare (AlexPn o _ _) (AlexPn o' _ _) = compare o o'

-- User state to hold comment depth, scanned chars of
-- a string and the position of the read token
data AlexUserState = AlexUserState { commentDepth :: Int
                                   , currentTokenPosn :: AlexPosn
                                   , readChars :: [Char]
                                   } deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {commentDepth = 0, currentTokenPosn = alexStartPos, readChars = []}

getCommentDepth :: Alex Int
getCommentDepth = commentDepth <$> alexGetUserState

setCommentDepth :: Int -> Alex ()
setCommentDepth d = do
  state <- alexGetUserState
  alexSetUserState $ state{commentDepth = d}

getCurrentTokenPosn :: AlexState -> AlexPosn
getCurrentTokenPosn = currentTokenPosn . alex_ust

setTokenPosn :: AlexPosn -> Alex ()
setTokenPosn p = do
  state <- alexGetUserState
  alexSetUserState $ state{currentTokenPosn = p}

getReadChars :: Alex [Char]
getReadChars = readChars <$> alexGetUserState

setReadChars :: [Char] -> Alex ()
setReadChars r = do
  state <- alexGetUserState
  alexSetUserState $ state{readChars = r}

appendChar :: Char -> Alex ()
appendChar c = do
  state <- alexGetUserState
  alexSetUserState $ state{readChars = c : readChars state}

-- Utils for position
getLineOfPosn :: AlexPosn -> Int
getLineOfPosn (AlexPn _ line _) = line

getColumnOfPosn :: AlexPosn -> Int
getColumnOfPosn (AlexPn _ _ col) = col

printPosn :: AlexPosn -> String
printPosn posn = "line: " ++ show (getLineOfPosn posn) ++
            " and column: " ++ show (getColumnOfPosn posn)

-- Error handling utils
lexicalError :: AlexPosn -> String -> Alex a
lexicalError posn message = alexError $ position ++ message where
  position = "Error at " ++ printPosn posn ++ ". "

unknownCharacter :: AlexAction Token
unknownCharacter (posn, _, _, current_string) len =
  let lexeme = (take len current_string)
  in lexicalError posn ("Unknown character: " ++ lexeme)

-- Handle end of file
alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  setTokenPosn p
  code <- alexGetStartCode
  if code == comment
    then alexError "Reached end of file without closing all comments"
    else case code of
      0 -> return EofT
      c -> alexError $ "Reached end of file in unsupported start code: " ++ show c

-- Utils for handling tokens
keyword :: Token -> AlexAction Token
keyword tokenConstr (posn, _, _, _) _ = do
  setTokenPosn posn
  return tokenConstr

identifiersAction :: (String -> Token) -> AlexAction Token
identifiersAction tokenConstr (posn, _, _, current_string) len = do
  setTokenPosn posn
  return $ tokenConstr (take len current_string)

intAction :: AlexAction Token
intAction (posn, _, _, current_string) len =
  let lexeme = (take len current_string)
  in case readMaybe lexeme :: Maybe Int of
    Just v  -> do
      setTokenPosn posn
      return (ConstIntT v)
    Nothing -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into an int")

floatAction :: AlexAction Token
floatAction (posn, _, _, current_string) len =
  let lexeme = (take len current_string)
  in case readMaybe lexeme :: Maybe Float of
    Just v  -> do
      setTokenPosn posn
      return (ConstFloatT v)
    Nothing -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into a float")

-- Char/String handling
beginString :: AlexAction Token
beginString input@(posn, _, _, _) len = do
  code <- alexGetStartCode
  case code of
    0 -> do
      alexSetStartCode string
      setReadChars ""
      setTokenPosn posn
    c -> lexicalError posn ("Unexpected startCode: " ++ show c ++ " in beginString")
  skip input len

endString :: AlexAction Token
endString (posn, _, _, _) _ = do
  code <- alexGetStartCode
  if code == string
    then do
      alexSetStartCode 0
      chars <- reverse <$> getReadChars
      setReadChars ""
      return (ConstStringT chars)
    else lexicalError posn ("Unexpected startCode: " ++ show code ++ " in endString")

stringAction :: AlexAction Token
stringAction input@(posn, _, _, current_string) len =
  let lexeme = (take len current_string)
      finalChar = parseCharLexeme lexeme
  in case finalChar of
      Just c  -> do
        appendChar c
        skip input len
      Nothing -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into a char of a string")

charAction :: AlexAction Token
charAction (posn, _, _, current_string) len =
  let lexeme = (take len current_string)
      finalChar = removeFromHead '\'' lexeme >>= removeFromTail '\'' >>= parseCharLexeme
  in case finalChar :: Maybe Char of
    Just ch -> do
      setTokenPosn posn
      return (ConstCharT ch)
    _ -> lexicalError posn ("Unable to parse: " ++ lexeme ++ " into a char")

-- Utils for chars
removeFromHead :: (Eq a) => a -> [a] -> Maybe [a]
removeFromHead _ [] = Nothing
removeFromHead a (x:xs) | a == x    = Just xs
                        | otherwise = Nothing

removeFromTail :: (Eq a) => a -> [a] -> Maybe [a]
removeFromTail _ []     = Nothing
removeFromTail a (x:[]) | a == x    = Just []
                        | otherwise = Nothing
removeFromTail a (x:xs) = (x:) <$> removeFromTail a xs

hexToInt :: Char -> Maybe Int
hexToInt c =
  let o = ord c
  in case isHexDigit c of
    False -> Nothing
    True | ord '0' <= o && o <= ord '9' -> Just (o - ord '0')
    True | ord 'A' <= o && o <= ord 'F' -> Just (o - ord 'A')
    True | ord 'a' <= o && o <= ord 'f' -> Just (o - ord 'a')
    _ -> Nothing

parseHex :: String -> Maybe Int
parseHex = aux (0 :: Integer) . reverse where
  aux _ []     = Nothing
  aux n [c]    = ((16 ^ n) *) <$> hexToInt c
  aux n (c:cs) = do
    cv <- hexToInt c
    csv <- aux (n + 1) cs
    return ((16 ^ n) * cv + csv)

parseCharLexeme :: String -> Maybe Char
parseCharLexeme s = case s of
  [c]    -> Just c
  "\\n"  -> Just '\n'
  "\\t"  -> Just '\t'
  "\\r"  -> Just '\r'
  "\\0"  -> Just '\0'
  "\\\\" -> Just '\\'
  "\\\'" -> Just '\''
  "\\\"" -> Just '\"'
  '\\':'x':hex1:hex0:"" | ord '0' <= ord hex1 && ord hex1 <= ord '7' -> chr <$> parseHex (hex1:hex0:"")
  _      -> Nothing

-- Comments utils
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


-- Utils for running lexer
-- Scan a string until EOF is encountered
lexer :: String -> Either String [Token]
lexer s = runAlex s alexTokens

alexTokens :: Alex [Token]
alexTokens = do
    t <- alexMonadScan
    case t of
      EofT -> return [t]
      _    -> (t:) <$> alexTokens
}
