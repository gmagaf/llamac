module Common.Token(Token(..),
                    Identifier,
                    ConstrIdentifier,
                    IntConstant,
                    FloatConstant,
                    CharConstant,
                    StringConstant,
                    lexeme) where

import Common.DebugPrint (DebugPrint (debugPrint), debugIO)

-- Mappings of Llama constants to Haskell values
type Identifier = String
type ConstrIdentifier = String
type IntConstant = Int
type FloatConstant = Float
type CharConstant = Char
type StringConstant = String

-- Definition of all the Llama tokens
data Token
  -- Keywords
  = AndT
  | ArrayT
  | BeginT
  | BoolT
  | CharT
  | DeleteT
  | DimT
  | DoT
  | DoneT
  | DowntoT
  | ElseT
  | EndT
  | FalseT
  | FloatT
  | ForT
  | IfT
  | InT
  | IntT
  | LetT
  | MatchT
  | ModT
  | MutableT
  | NewT
  | NotT
  | OfT
  | RecT
  | RefT
  | ThenT
  | ToT
  | TrueT
  | TypeT
  | UnitT
  | WhileT
  | WithT
  -- Identifiers
  | IdT Identifier
  | IdConstrT ConstrIdentifier
  -- Constants
  | ConstIntT IntConstant
  | ConstFloatT FloatConstant
  | ConstCharT CharConstant
  | ConstStringT StringConstant
  -- Symbols
  | ArrowT
  | EqualsT
  | BarT
  | PlusT
  | MinusT
  | TimesT
  | DivT
  | PlusFloatT
  | MinusFloatT
  | TimesFloatT
  | DivFloatT
  | ExpT
  | BangT
  | SemicolonT
  | AndOpT
  | OrOpT
  | NotEqualsT
  | LessThanT
  | GreaterThanT
  | LessThanEqT
  | GreaterThanEqT
  | NatEqOpT
  | NotNatEqOpT
  | AssignMutableT
  | LParenT
  | RParenT
  | LBracketT
  | RBracketT
  | CommaT
  | ColonT
  | EofT
  deriving (Eq, Show)

instance DebugPrint Token where
    debugPrint = debugIO True False

-- Print the lexeme of the token
lexeme :: Token -> String
lexeme t = case t of
    -- Keywords
    AndT               -> "and"
    ArrayT             -> "array"
    BeginT             -> "begin"
    BoolT              -> "bool"
    CharT              -> "char"
    DeleteT            -> "delete"
    DimT               -> "dim"
    DoT                -> "do"
    DoneT              -> "done"
    DowntoT            -> "downto"
    ElseT              -> "else"
    EndT               -> "end"
    FalseT             -> "false"
    FloatT             -> "float"
    ForT               -> "for"
    IfT                -> "if"
    InT                -> "in"
    IntT               -> "int"
    LetT               -> "let"
    MatchT             -> "match"
    ModT               -> "mod"
    MutableT           -> "mutable"
    NewT               -> "new"
    NotT               -> "not"
    OfT                -> "of"
    RecT               -> "rec"
    RefT               -> "ref"
    ThenT              -> "then"
    ToT                -> "to"
    TrueT              -> "true"
    TypeT              -> "type"
    UnitT              -> "unit"
    WhileT             -> "while"
    WithT              -> "with"
    -- Identifiers
    IdT v              -> v
    IdConstrT v        -> v
    -- Constants
    ConstIntT v        -> show v
    ConstFloatT v      -> show v
    ConstCharT v       -> '\'' : v : "\'"
    ConstStringT v     -> "\"" ++ v ++ "\""
    -- Symbols
    ArrowT             -> "->"
    EqualsT            -> "="
    BarT               -> "|"
    PlusT              -> "+"
    MinusT             -> "-"
    TimesT             -> "*"
    DivT               -> "/"
    PlusFloatT         -> "+."
    MinusFloatT        -> "-."
    TimesFloatT        -> "*."
    DivFloatT          -> "/."
    ExpT               -> "**"
    BangT              -> "!"
    SemicolonT         -> ";"
    AndOpT             -> "&&"
    OrOpT              -> "||"
    NotEqualsT         -> "<>"
    LessThanT          -> "<"
    GreaterThanT       -> ">"
    LessThanEqT        -> "<="
    GreaterThanEqT     -> ">="
    NatEqOpT           -> "=="
    NotNatEqOpT        -> "!="
    AssignMutableT     -> ":="
    LParenT            -> "("
    RParenT            -> ")"
    LBracketT          -> "["
    RBracketT          -> "]"
    CommaT             -> ","
    ColonT             -> ":"
    EofT               -> "eof"
