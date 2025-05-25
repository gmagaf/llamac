{
module Parser.Parser (calc) where

import Lexer.Lexer (AlexPosn)
import Common.Token (Token(..))
import Common.AST
import Parser.ParserM (Parser, lexerWrap, getAlexPos, getTokenPosn, throwAtPosn, throwParsingError)
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
  : P LetDef_                       { $2 $1 }

LetDef_ :: { AlexPosn -> LetDef AlexPosn }
  : let Defs                        { Let (reverse $2) }
  | let rec Defs                    { LetRec (reverse $3) }

Defs :: { [Def AlexPosn] }
  : Def                             { $1 : [] }
  | Defs and Def                    { $3 : $1 }

Def :: { Def AlexPosn }
  : P Def_                          { $2 $1 }

Def_ :: { AlexPosn -> Def AlexPosn }
  : id Params '=' Expr              { FunDef $1 (reverse $2) $4 }
  | id Params ':' Type '=' Expr     { FunDefTyped $1 (reverse $2) $4 $6 }
  | mutable id                      { VarDef $2 }
  | mutable id ':' Type             { VarDefTyped $2 $4 }
  | mutable id '[' ExprsComma ']'   { ArrayDef $2 (reverse $4) }
  | mutable id '[' ExprsComma ']' ':' Type
                                    { ArrayDefTyped $2 (reverse $4) $7 }

Params :: { [Param AlexPosn] }
  : {- empty -}                     { [] }
  | Params Param                    { $2 : $1 }

ExprsComma :: { [Expr AlexPosn] }
  : Expr                            { $1 : [] }
  | ExprsComma ',' Expr             { $3 : $1 }

TypeDef :: { TypeDef AlexPosn }
  : P TypeDef_                      { $2 $1 }

TypeDef_ :: { AlexPosn -> TypeDef AlexPosn }
  : type TDefs                      { TypeDef (reverse $2) }

TDefs :: { [TDef AlexPosn] }
  : TDef                            { $1 : [] }
  | TDefs and TDef                  { $3 : $1 }

TDef :: { TDef AlexPosn }
  : P TDef_                         { $2 $1 }

TDef_ :: { AlexPosn -> TDef AlexPosn }
  : id '=' Constrs                  { TDef $1 (reverse $3) }

Constrs :: { [Constr AlexPosn] }
  : Constr                          { $1 : [] }
  | Constrs '|' Constr              { $3 : $1 }

Constr :: { Constr AlexPosn }
  : P Constr_                       { $2 $1 }

Constr_ :: { AlexPosn -> Constr AlexPosn }
  : id_constr                       { Constr $1 [] }
  | id_constr of Types              { Constr $1 (reverse $3) }

Types :: { [Type AlexPosn] }
  : Type                            { $1 : [] }
  | Types Type                      { $2 : $1 }

Param :: { Param AlexPosn }
  : P Param_                        { $2 $1 }

Param_ :: { AlexPosn -> Param AlexPosn }
  : id                              { Param $1 }
  | '(' id ':' Type ')'             { TypedParam $2 $4 }

Type :: { Type AlexPosn }
  : ArrayType '->' Type             { Type (FunType $1 $3) (tag $1) }
  | ArrayType                       { $1 }

ArrayType :: { Type AlexPosn }
  : P ArrayType_                    { $2 $1 }
  | RefType                         { $1 }

ArrayType_ :: { AlexPosn -> Type AlexPosn }
  : array Dims of ArrayType         { Type (ArrayType $2 $4) }

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
  : P BaseType_                     { $2 $1 }

BaseType_ :: { AlexPosn -> Type AlexPosn }
  : unit                            { Type UnitType }
  | int                             { Type IntType }
  | char                            { Type CharType }
  | bool                            { Type BoolType }
  | float                           { Type FloatType }
  | id                              { Type (UserDefinedType $1) }
  | '(' Type ')'                    { const $2 }

Expr :: { Expr AlexPosn }
  : LetExpr                         { $1 }
  | SemicolonExpr                   { $1 }
  | SemicolonLetExpr                { $1 }

LetExpr :: { Expr AlexPosn }
  : LetDef in Expr                  { LetIn $1 $3 (tag $1) }

SemicolonExpr :: { Expr AlexPosn }
  : SemicolonExpr ';' SemicolonExpr { Expr (BinOpExpr SemicolonOp $1 $3) (tag $1) }
  | IfExpr                          { $1 }

SemicolonLetExpr :: { Expr AlexPosn } -- Cannot have let in lhs
  : SemicolonExpr ';' LetExpr       { Expr (BinOpExpr SemicolonOp $1 $3) (tag $1) }

LetIfExpr :: { Expr AlexPosn }
  : LetDef in IfExpr                { LetIn $1 $3 (tag $1) }
  | IfExpr                          { $1 }

IfExpr :: { Expr AlexPosn } -- Cannot have semicolon in last Expr
  : P IfExpr_                       { $2 $1 }
  | LogicalExpr                     { $1 }

IfExpr_ :: { AlexPosn -> Expr AlexPosn }
  : if Expr then LetIfExpr          { Expr (IfThenExpr $2 $4) }
  | if Expr then LetIfExpr else LetIfExpr
                                    { Expr (IfThenElseExpr $2 $4 $6) }

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
  : P UnOpExpr_                     { $2 $1 }
  | FunAppExpr                      { $1 }

UnOpExpr_ :: { AlexPosn -> Expr AlexPosn }
  : '+' UnOpExpr                    { Expr (UnOpExpr PlusUnOp $2) }
  | '-' UnOpExpr                    { Expr (UnOpExpr MinusUnOp $2) }
  | '+.' UnOpExpr                   { Expr (UnOpExpr PlusFloatUnOp $2) }
  | '-.' UnOpExpr                   { Expr (UnOpExpr MinusFloatUnOp $2) }
  | not UnOpExpr                    { Expr (UnOpExpr NotOp $2) }
  | delete UnOpExpr                 { Expr (DeleteExpr $2) }
  | dim id                          { Expr (ArrayDim $2 1) }
  | dim const_int id                { Expr (ArrayDim $3 $2) }

FunAppExpr :: { Expr AlexPosn }
  : P FunAppExpr_                   { $2 $1 }
  | DerefExp                        { $1 }

FunAppExpr_ :: { AlexPosn -> Expr AlexPosn }
  : id Args                         { Expr (FunAppExpr $1 (reverse $2)) }
  | id_constr Args                  { Expr (ConstrAppExpr $1 (reverse $2)) }

Args :: { [Expr AlexPosn] }
  : DerefExp                        { $1 : [] }
  | Args DerefExp                   { $2 : $1 }

DerefExp :: { Expr AlexPosn }
  : P DerefExp_                     { $2 $1 }
  | ArrayExpr                       { $1 }

DerefExp_ :: { AlexPosn -> Expr AlexPosn }
  : '!' DerefExp                    { Expr (UnOpExpr BangOp $2) }

ArrayExpr :: { Expr AlexPosn }
  : P ArrayExpr_                    { $2 $1 }
  | NewTExpr                        { $1 }

ArrayExpr_ :: { AlexPosn -> Expr AlexPosn }
  : id '[' ExprsComma ']'           { Expr (ArrayAccess $1 (reverse $3)) }

NewTExpr :: { Expr AlexPosn }
  : P NewTExpr_                     { $2 $1 }
  | BaseExpr                        { $1 }

NewTExpr_ :: { AlexPosn -> Expr AlexPosn }
  : new Type                        { NewType $2 }

BaseExpr :: { Expr AlexPosn }
  : P BaseExpr_                     { $2 $1 }

BaseExpr_ :: { AlexPosn -> Expr AlexPosn }
  : const_int                       { Expr (IntCExpr $1) }
  | const_float                     { Expr (FloatCExpr $1) }
  | const_char                      { Expr (CharCExpr $1) }
  | const_string                    { Expr (StringCExpr $1) }
  | true                            { Expr TrueCExpr }
  | false                           { Expr FalseCExpr }
  | '('')'                          { Expr UnitCExpr }
  | id                              { Expr (ConstExpr $1) }
  | id_constr                       { Expr (ConstConstrExpr $1) }
  | '(' Expr ')'                    { const $2 }
  | begin Expr end                  { Expr (BeginExpr $2) }
  | while Expr do Expr done         { Expr (WhileExpr $2 $4) }
  | for id '=' Expr to Expr do Expr done
                                    { Expr (ForExpr $2 $4 $6 $8) }
  | for id '=' Expr downto Expr do Expr done
                                    { Expr (ForDownExpr $2 $4 $6 $8) }
  | match Expr with Clauses end     { MatchExpr $2 (reverse $4) }

Clauses :: { [Clause AlexPosn] }
  : Clause                          { $1 : [] }
  | Clauses '|' Clause              { $3 : $1 }

Clause :: { Clause AlexPosn }
  : Pattern '->' Expr               { Match $1 $3 (tag $1) }

Pattern :: { Pattern AlexPosn }
  : P Pattern_                      { $2 $1 }
  | PatArg                          { $1 }

Pattern_ :: { AlexPosn -> Pattern AlexPosn }
  : id_constr PatArgs               { Pattern (ConstrPattern $1 (reverse $2)) }

PatArgs :: { [Pattern AlexPosn] }
  : PatArg                          { $1 : [] }
  | PatArgs PatArg                  { $2 : $1 }

PatArg :: { Pattern AlexPosn }
  : P PatArg_                       { $2 $1 }

PatArg_ :: { AlexPosn -> Pattern AlexPosn }
  : const_int                       { Pattern (IntConstPattern NoSign $1) }
  | '+' const_int                   { Pattern (IntConstPattern Plus $2) }
  | '-' const_int                   { Pattern (IntConstPattern Minus $2) }
  | const_float                     { Pattern (FloatConstPattern NoSign $1) }
  | '+.' const_float                { Pattern (FloatConstPattern Plus $2) }
  | '-.' const_float                { Pattern (FloatConstPattern Minus $2) }
  | const_char                      { Pattern (CharConstPattern $1) }
  | true                            { Pattern TruePattern }
  | false                           { Pattern FalsePattern }
  | id                              { Pattern (IdPattern $1) }
  | id_constr                       { Pattern (ConstrPattern $1 []) }
  | '(' Pattern ')'                 { const $2 }

{
-- Handle errors
parseError :: Token -> Parser a
parseError t = do
    posn <- getAlexPos
    throwAtPosn posn $ throwParsingError $ "Unable to process token " ++ show t
}
