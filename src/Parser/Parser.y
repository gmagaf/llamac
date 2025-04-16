{
module Parser.Parser (calc, parse, parseFile) where

import Lexer.Lexer (AlexPosn, getLineOfPosn, getColumnOfPosn)
import Common.Token (Token(..))
import Common.AST
import Parser.ParserM (Parser, lexerWrap, runParser, getAlexPos, getTokenPosn,
                       ParserState, initParserState,
                       Error, throwParsingError)
}

%name calc
%tokentype { Token }
%monad { Parser }
%lexer { lexerWrap } { T_eof }
%error { parseError }

%token
  -- Keywords
  and                   { T_and }
  array                 { T_array }
  begin                 { T_begin }
  bool                  { T_bool }
  char                  { T_char }
  delete                { T_delete }
  dim                   { T_dim }
  do                    { T_do }
  done                  { T_done }
  downto                { T_downto }
  else                  { T_else }
  end                   { T_end }
  false                 { T_false }
  float                 { T_float }
  for                   { T_for }
  if                    { T_if }
  in                    { T_in }
  int                   { T_int }
  let                   { T_let }
  match                 { T_match }
  mod                   { T_mod }
  mutable               { T_mutable }
  new                   { T_new }
  not                   { T_not }
  of                    { T_of }
  rec                   { T_rec }
  ref                   { T_ref }
  then                  { T_then }
  to                    { T_to }
  true                  { T_true }
  type                  { T_type }
  unit                  { T_unit }
  while                 { T_while }
  with                  { T_with }
  -- Identifiers
  id                    { T_id $$ }
  id_constr             { T_id_constr $$ }
  -- Constants
  const_int             { T_const_int $$ }
  const_float           { T_const_float $$ }
  const_char            { T_const_char $$ }
  const_string          { T_const_string $$ }
  -- Symbols
  '->'                  { T_arrow }
  '='                   { T_equals }
  '|'                   { T_bar }
  '+'                   { T_plus }
  '-'                   { T_minus }
  '*'                   { T_times }
  '/'                   { T_div }
  '+.'                  { T_plus_float }
  '-.'                  { T_minus_float }
  '*.'                  { T_times_float }
  '/.'                  { T_div_float }
  '**'                  { T_exp }
  '!'                   { T_bang }
  ';'                   { T_semicolon }
  '&&'                  { T_and_op }
  '||'                  { T_or_op }
  '<>'                  { T_not_equals }
  '<'                   { T_less_than }
  '>'                   { T_greater_than }
  '<='                  { T_less_than_eq }
  '>='                  { T_greater_than_eq }
  '=='                  { T_nat_eq_op }
  '!='                  { T_not_nat_eq_op }
  ':='                  { T_assign_mutable }
  '('                   { T_lparen }
  ')'                   { T_rparen }
  '['                   { T_lbracket }
  ']'                   { T_rbracket }
  ','                   { T_comma }
  ':'                   { T_colon }

%left ';'
%nonassoc ':='
%left '||'
%left '&&'
%nonassoc '=' '<>' '>' '<' '<=' '>=' '==' '!='
%left '+' '-' '+.' '-.'
%left '*' '/' '*.' '/.' mod
%right '**'

%expect 1 -- always choose shift in dangling else conflict

%%

AST :: { AST AlexPosn }
  : AST_                            { reverse $1 }
-- reversing the lists so that all definitions are in the correct order

AST_ :: { [Either (LetDef AlexPosn) (TypeDef AlexPosn)] }
  : {- emtpy -}                     { [] }
  | AST_ LetDef                     { (Left $2) : $1 }
  | AST_ TypeDef                    { (Right $2) : $1 }

P :: { AlexPosn }
  : {- empty -}                     {% getTokenPosn }

LetDef :: { LetDef AlexPosn }
  : P let Defs                      { Let (reverse $3) $1 }
  | P let rec Defs                  { LetRec (reverse $4) $1 }

Defs :: { [Def AlexPosn] }
  : Def                             { $1 : [] }
  | Defs and Def                    { $3 : $1 }

Def :: { Def AlexPosn }
  : P id Params '=' Expr            { FunDef $2 (reverse $3) $5 $1 }
  | P id Params ':' Type '=' Expr   { FunDefTyped $2 (reverse $3) $5 $7 $1 }
  | P mutable id                    { VarDef $3 $1 }
  | P mutable id ':' Type           { VarDefTyped $3 $5 $1 }
  | P mutable id '[' ExprsComma ']' { ArrayDef $3 (reverse $5) $1 }
  | P mutable id '[' ExprsComma ']' ':' Type
                                    { ArrayDefTyped $3 (reverse $5) $8 $1 }

Params :: { [Param AlexPosn] }
  : {- empty -}                     { [] }
  | Params Param                    { $2 : $1 }

ExprsComma :: { [Expr AlexPosn] }
  : Expr                            { $1 : [] }
  | ExprsComma ',' Expr             { $3 : $1 }

TypeDef :: { TypeDef AlexPosn }
  : P type TDefs                    { TypeDef (reverse $3) $1 }

TDefs :: { [TDef AlexPosn] }
  : TDef                            { $1 : [] }
  | TDefs and TDef                  { $3 : $1 }

TDef :: { TDef AlexPosn }
  : P id '=' Constrs                { TDef $2 (reverse $4) $1 }

Constrs :: { [Constr AlexPosn] }
  : Constr                          { $1 : [] }
  | Constrs '|' Constr              { $3 : $1 }

Constr :: { Constr AlexPosn }
  : P id_constr                     { Constr $2 [] $1 }
  | P id_constr of Types            { Constr $2 (reverse $4) $1 }

Types :: { [Type AlexPosn] }
  : Type                            { $1 : [] }
  | Types Type                      { $2 : $1 }

Param :: { Param AlexPosn }
  : P id                            { Param $2 $1 }
  | P '(' id ':' Type ')'           { TypedParam $3 $5 $1 }

Type :: { Type AlexPosn }
  : ArrayType '->' Type             { Type (FunType $1 $3) (tag $1) }
  | ArrayType                       { $1 }

ArrayType :: { Type AlexPosn }
  : P array Dims of ArrayType       { Type (ArrayType $3 $5) $1 }
  | RefType                         { $1 }

Dims :: { Int }
  : {- empty -}                     { 1 }
  | '[' Stars ']'                   { $2 }

Stars :: { Int }
  : '*'                             { 1 }
  | Stars ',' '*'                   { $1 + 1 }

RefType :: { Type AlexPosn }
  : RefType ref                     { Type (RefType $1) (tag $1) }
  | BaseType                        { $1 }

BaseType :: { Type AlexPosn }
  : P unit                          { Type UnitType $1 }
  | P int                           { Type IntType $1 }
  | P char                          { Type CharType $1 }
  | P bool                          { Type BoolType $1 }
  | P float                         { Type FloatType $1 }
  | P id                            { Type (UserDefinedType $2) $1 }
  | '(' Type ')'                    { $2 }

Expr :: { Expr AlexPosn }
  : LetExpr                         { $1 }
  | SemicolonExpr                   { $1 }
  | SemicolonLetExpr                { $1 }

LetExpr :: { Expr AlexPosn }
  : LetDef in Expr                  { Expr (LetIn $1 $3) (tag $1) }

SemicolonExpr :: { Expr AlexPosn }
  : SemicolonExpr ';' SemicolonExpr { Expr (BinOpExpr SemicolonOp $1 $3) (tag $1) }
  | IfExpr                          { $1 }

SemicolonLetExpr :: { Expr AlexPosn } -- Cannot have let in lhs
  : SemicolonExpr ';' LetExpr       { Expr (BinOpExpr SemicolonOp $1 $3) (tag $1) }

LetIfExpr :: { Expr AlexPosn }
  : LetDef in IfExpr                { Expr (LetIn $1 $3) (tag $1) }
  | IfExpr                          { $1 }

IfExpr :: { Expr AlexPosn } -- Cannot have semicolon in last Expr
  : P if Expr then LetIfExpr        { Expr (IfThenExpr $3 $5) $1 }
  | P if Expr then LetIfExpr else LetIfExpr
                                    { Expr (IfThenElseExpr $3 $5 $7) $1 }
  | LogicalExpr                     { $1 }

LogicalExpr :: { Expr AlexPosn }
  : LogicalExpr ':=' LogicalExpr    { Expr (BinOpExpr AssignMutableOp $1 $3) (tag $1) }
  | LogicalExpr '||' LogicalExpr    { Expr (BinOpExpr OrOp $1 $3) (tag $1) }
  | LogicalExpr '&&' LogicalExpr    { Expr (BinOpExpr AndOp $1 $3) (tag $1) }
  | LogicalExpr '='  LogicalExpr    { Expr (BinOpExpr EqOp $1 $3) (tag $1) }
  | LogicalExpr '<>' LogicalExpr    { Expr (BinOpExpr NotEqOp $1 $3) (tag $1) }
  | LogicalExpr '<'  LogicalExpr    { Expr (BinOpExpr LTOp $1 $3) (tag $1) }
  | LogicalExpr '>'  LogicalExpr    { Expr (BinOpExpr GTOp $1 $3) (tag $1) }
  | LogicalExpr '<=' LogicalExpr    { Expr (BinOpExpr LEqOp $1 $3) (tag $1) }
  | LogicalExpr '>=' LogicalExpr    { Expr (BinOpExpr GEqOp $1 $3) (tag $1) }
  | LogicalExpr '==' LogicalExpr    { Expr (BinOpExpr NatEqOp $1 $3) (tag $1) }
  | LogicalExpr '!=' LogicalExpr    { Expr (BinOpExpr NotNatEqOp $1 $3) (tag $1) }
  | ArithmExpr                      { $1 }

ArithmExpr :: { Expr AlexPosn }
  : ArithmExpr '+'  ArithmExpr      { Expr (BinOpExpr PlusOp $1 $3) (tag $1) }
  | ArithmExpr '-'  ArithmExpr      { Expr (BinOpExpr MinusOp $1 $3) (tag $1) }
  | ArithmExpr '*'  ArithmExpr      { Expr (BinOpExpr TimesOp $1 $3) (tag $1) }
  | ArithmExpr '/'  ArithmExpr      { Expr (BinOpExpr DivOp $1 $3) (tag $1) }
  | ArithmExpr '+.' ArithmExpr      { Expr (BinOpExpr PlusFloatOp $1 $3) (tag $1) }
  | ArithmExpr '-.' ArithmExpr      { Expr (BinOpExpr MinusFloatOp $1 $3) (tag $1) }
  | ArithmExpr '*.' ArithmExpr      { Expr (BinOpExpr TimesFloatOp $1 $3) (tag $1) }
  | ArithmExpr '/.' ArithmExpr      { Expr (BinOpExpr DivFloatOp $1 $3) (tag $1) }
  | ArithmExpr mod  ArithmExpr      { Expr (BinOpExpr ModOp $1 $3) (tag $1) }
  | ArithmExpr '**' ArithmExpr      { Expr (BinOpExpr ExpOp $1 $3) (tag $1) }
  | UnOpExpr                        { $1 }

UnOpExpr :: { Expr AlexPosn }
  : P '+' UnOpExpr                  { Expr (UnOpExpr PlusUnOp $3) $1 }
  | P '-' UnOpExpr                  { Expr (UnOpExpr MinusUnOp $3) $1 }
  | P '+.' UnOpExpr                 { Expr (UnOpExpr PlusFloatUnOp $3) $1 }
  | P '-.' UnOpExpr                 { Expr (UnOpExpr MinusFloatUnOp $3) $1 }
  | P not UnOpExpr                  { Expr (UnOpExpr NotOp $3) $1 }
  | P delete UnOpExpr               { Expr (DeleteExpr $3) $1 }
  | P dim id                        { Expr (ArrayDim $3 1) $1 }
  | P dim const_int id              { Expr (ArrayDim $4 $3) $1 }
  | FunAppExpr                      { $1 }

FunAppExpr :: { Expr AlexPosn }
  : P id Args                       { Expr (FunAppExpr $2 (reverse $3)) $1 }
  | P id_constr Args                { Expr (ConstrAppExpr $2 (reverse $3)) $1 }
  | DerefExp                        { $1 }

Args :: { [Expr AlexPosn] }
  : DerefExp                        { $1 : [] }
  | Args DerefExp                   { $2 : $1 }

DerefExp :: { Expr AlexPosn }
  : P '!' DerefExp                  { Expr (UnOpExpr BangOp $3) $1 }
  | ArrayExpr                       { $1 }

ArrayExpr :: { Expr AlexPosn }
  : P id '[' ExprsComma ']'         { Expr (ArrayAccess $2 (reverse $4)) $1 }
  | NewTExpr                        { $1 }

NewTExpr :: { Expr AlexPosn }
  : P new Type                      { Expr (NewType $3) $1 }
  | BaseExpr                        { $1 }

BaseExpr :: { Expr AlexPosn }
  : P const_int                     { Expr (IntCExpr $2) $1 }
  | P const_float                   { Expr (FloatCExpr $2) $1 }
  | P const_char                    { Expr (CharCExpr $2) $1 }
  | P const_string                  { Expr (StringCExpr $2) $1 }
  | P true                          { Expr TrueCExpr $1 }
  | P false                         { Expr FalseCExpr $1 }
  | P '('')'                        { Expr UnitCExpr $1 }
  | P id                            { Expr (FunAppExpr $2 []) $1 }
  | P id_constr                     { Expr (ConstrAppExpr $2 []) $1 }
  | P '(' Expr ')'                  { $3 }
  | P begin Expr end                { Expr (BeginExpr $3) $1 }
  | P while Expr do Expr done       { Expr (WhileExpr $3 $5) $1 }
  | P for id '=' Expr to Expr do Expr done
                                    { Expr (ForExpr $3 $5 $7 $9) $1 }
  | P for id '=' Expr downto Expr do Expr done
                                    { Expr (ForDownExpr $3 $5 $7 $9) $1 }
  | P match Expr with Clauses end   { Expr (MatchExpr $3 (reverse $5)) $1 }

Clauses :: { [Clause AlexPosn] }
  : Clause                          { $1 : [] }
  | Clauses '|' Clause              { $3 : $1 }

Clause :: { Clause AlexPosn }
  : Pattern '->' Expr               { Match $1 $3 (tag $1) }

Pattern :: { Pattern AlexPosn }
  : P id_constr PatArgs             { Pattern (ConstrPattern $2 (reverse $3)) $1 }
  | PatArg                          { $1 }

PatArgs :: { [Pattern AlexPosn] }
  : PatArg                          { $1 : [] }
  | PatArgs PatArg                  { $2 : $1 }

PatArg :: { Pattern AlexPosn }
  : P const_int                     { Pattern (IntConstPattern NoSign $2) $1 }
  | P '+' const_int                 { Pattern (IntConstPattern Plus $3) $1 }
  | P '-' const_int                 { Pattern (IntConstPattern Minus $3) $1 }
  | P const_float                   { Pattern (FloatConstPattern NoSign $2) $1 }
  | P '+.' const_float              { Pattern (FloatConstPattern Plus $3) $1 }
  | P '-.' const_float              { Pattern (FloatConstPattern Minus $3) $1 }
  | P const_char                    { Pattern (CharConstPattern $2) $1 }
  | P true                          { Pattern TruePattern $1 }
  | P false                         { Pattern FalsePattern $1 }
  | P id                            { Pattern (IdPattern $2) $1 }
  | P id_constr                     { Pattern (ConstrPattern $2 []) $1 }
  | '(' Pattern ')'                 { $2 }

{
-- Handle errors
parseError :: Token -> Parser a
parseError t =
  let position p = "Error at line: " ++ show (getLineOfPosn p) ++
                   " and column: " ++ show (getColumnOfPosn p) ++ ". "
  in do
    posn <- getAlexPos
    throwParsingError $ (position posn) ++ "Unable to process token " ++ show t

-- The parsing function
parse :: String -> Either Error (AST AlexPosn)
parse s = runParser initState calc where
  initState :: ParserState
  initState = initParserState s

-- Parsing utils for files
parseFile :: FilePath -> IO ()
parseFile f = do
  s <- readFile f
  let res = parse s
  case res of
    Left err -> putStrLn (show err)
    Right p  -> putStrLn (show p)
}
