{
module Parser.Parser (calc, test) where

import Lexer.Lexer
import Common.Token
import Common.AST
}

%name calc
%tokentype { Token }
%monad { Alex }
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
  '='                   { T_assign }
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
  '<>'                  { T_not_struct_eq_op }
  '<'                   { T_less_than }
  '>'                   { T_more_than }
  '<='                  { T_less_than_eq }
  '>='                  { T_more_than_eq }
  '=='                  { T_nat_eq_op }
  '!='                  { T_not_nat_eq_op }
  ':='                  { T_assign_mutable }
  '('                   { T_lparen }
  ')'                   { T_rparen }
  '['                   { T_lbracket }
  ']'                   { T_rbracket }
  ','                   { T_comma }
  ':'                   { T_colon }

%right in
%left ';'
%nonassoc then
%nonassoc else
%nonassoc ':='
%left '||'
%left '&&'
%nonassoc '=' '<>' '>' '<' '<=' '>=' '==' '!='
%left '+' '-' '+.' '-.'
%left '*' '/' '*.' '/.' mod
%right '**'

%%

Program :: { Program }
  : Program_                        { reverse $1 }
-- reversing the lists so that all definitions are in the correct order

Program_ :: { Program }
  : {- emtpy -}                     { [] }
  | Program_ LetDef                 { (Left $2) : $1 }
  | Program_ TypeDef                { (Right $2) : $1 }

LetDef :: { LetDef }
  : let Defs                        { Let (reverse $2) }
  | let rec Defs                    { LetRec (reverse $3) }

Defs :: { [Def] }
  : Def                             { $1 : [] }
  | Defs and Def                    { $3 : $1 }

Def :: { Def }
  : id Params '=' Expr              { FunDef $1 (reverse $2) $4 }
  | id Params ':' Type '=' Expr     { FunDefTyped $1 (reverse $2) $4 $6 }
  | mutable id                      { VarDef $2 }
  | mutable id ':' Type             { VarDefTyped $2 $4 }
  | mutable id '[' ExprsComma ']'   { ArrayDef $2 (reverse $4) }
  | mutable id '[' ExprsComma ']' ':' Type
                                    { ArrayDefTyped $2 (reverse $4) $7 }

Params :: { [Param] }
  : {- empty -}                     { [] }
  | Params Param                    { $2 : $1 }

ExprsComma :: { [Expr] }
  : Expr                            { $1 : [] }
  | ExprsComma ',' Expr             { $3 : $1 }

TypeDef :: { TypeDef }
  : type TDefs                      { Type (reverse $2) }

TDefs :: { [TDef] }
  : TDef                            { $1 : [] }
  | TDefs and TDef                  { $3 : $1 }

TDef :: { TDef }
  : id '=' Constrs                  { TDef $1 (reverse $3) }

Constrs :: { [Constr] }
  : Constr                          { $1 : [] }
  | Constrs '|' Constr              { $3 : $1 }

Constr :: { Constr }
  : id_constr                       { Constr $1 [] }
  | id_constr of Types              { Constr $1 (reverse $3) }

Types :: { [Type] }
  : Type                            { $1 : [] }
  | Types Type                      { $2 : $1 }

Param :: { Param }
  : id                              { Param $1 }
  | '(' id ':' Type ')'             { TypedParam $2 $4 }

Type :: { Type }
  : ArrayType '->' Type             { FunType $1 $3 }
  | ArrayType                       { $1 }

ArrayType :: { Type }
  : array Dims of ArrayType         { ArrayType $2 $4 }
  | RefType                         { $1 }

Dims :: { Int }
  : {- empty -}                     { 1 }
  | '[' Stars ']'                   { $2 }

Stars :: { Int }
  : '*'                             { 1 }
  | Stars ',' '*'                   { $1 + 1 }

RefType :: { Type }
  : RefType ref                     { RefType $1 }
  | BaseType                        { $1 }

BaseType :: { Type }
  : unit                            { UnitType }
  | int                             { IntType }
  | char                            { CharType }
  | bool                            { BoolType }
  | float                           { FloatType }
  | id                              { UserDefinedType $1 }
  | '(' Type ')'                    { $2 }

-- TODO: Fix dangling if
Expr :: { Expr }
  : LetDef in Expr                  { LetIn $1 $3 }
  | Expr ';' Expr                   { BinOpExpr SemicolonOp $1 $3 }
  | if Expr then Expr               { IfThenExpr $2 $4 }
  | if Expr then Expr else Expr     { IfThenElseExpr $2 $4 $6 }
  | LogicalExpr                     { $1 }

LogicalExpr :: { Expr }
  : LogicalExpr ':=' LogicalExpr    { BinOpExpr AssignMutableOp $1 $3 }
  | LogicalExpr '||' LogicalExpr    { BinOpExpr OrOp $1 $3 }
  | LogicalExpr '&&' LogicalExpr    { BinOpExpr AndOp $1 $3 }
  | LogicalExpr '='  LogicalExpr    { BinOpExpr AssignOp $1 $3 }
  | LogicalExpr '<>' LogicalExpr    { BinOpExpr NotStructEqOp $1 $3 }
  | LogicalExpr '<'  LogicalExpr    { BinOpExpr LTOp $1 $3 }
  | LogicalExpr '>'  LogicalExpr    { BinOpExpr GTOp $1 $3 }
  | LogicalExpr '<=' LogicalExpr    { BinOpExpr LEqOp $1 $3 }
  | LogicalExpr '>=' LogicalExpr    { BinOpExpr GEqOp $1 $3 }
  | LogicalExpr '==' LogicalExpr    { BinOpExpr NatEqOp $1 $3 }
  | LogicalExpr '!=' LogicalExpr    { BinOpExpr NotNatEqOp $1 $3 }
  | ArithmExpr                      { $1 }

ArithmExpr :: { Expr }
  : ArithmExpr '+'  ArithmExpr      { BinOpExpr PlusOp $1 $3 }
  | ArithmExpr '-'  ArithmExpr      { BinOpExpr MinusOp $1 $3 }
  | ArithmExpr '*'  ArithmExpr      { BinOpExpr TimesOp $1 $3 }
  | ArithmExpr '/'  ArithmExpr      { BinOpExpr DivOp $1 $3 }
  | ArithmExpr '+.' ArithmExpr      { BinOpExpr PlusFloatOp $1 $3 }
  | ArithmExpr '-.' ArithmExpr      { BinOpExpr MinusFloatOp $1 $3 }
  | ArithmExpr '*.' ArithmExpr      { BinOpExpr TimesFloatOp $1 $3 }
  | ArithmExpr '/.' ArithmExpr      { BinOpExpr DivFloatOp $1 $3 }
  | ArithmExpr mod  ArithmExpr      { BinOpExpr ModOp $1 $3 }
  | ArithmExpr '**' ArithmExpr      { BinOpExpr ExpOp $1 $3 }
  | UnOpExpr                        { $1 }

UnOpExpr :: { Expr }
  : '+' UnOpExpr                    { UnOpExpr PlusUnOp $2 }
  | '-' UnOpExpr                    { UnOpExpr MinusUnOp $2 }
  | '+.' UnOpExpr                   { UnOpExpr PlusFloatUnOp $2 }
  | '-.' UnOpExpr                   { UnOpExpr MinusFloatUnOp $2 }
  | not UnOpExpr                    { UnOpExpr NotOp $2 }
  | delete UnOpExpr                 { DeleteExpr $2 }
  | dim id                          { ArrayDim $2 1 }
  | dim const_int id                { ArrayDim $3 $2 }
  | FunAppExpr                      { $1 }

FunAppExpr :: { Expr }
  : id Args                         { FunAppExpr $1 (reverse $2) }
  | id_constr Args                  { ConstrAppExpr $1 (reverse $2) }
  | DerefExp                        { $1 }

Args :: { [Expr] }
  : DerefExp                        { $1 : [] }
  | Args DerefExp                   { $2 : $1 }

DerefExp :: { Expr }
  : '!' DerefExp                    { UnOpExpr BangOp $2 }
  | ArrayExpr                       { $1 }

ArrayExpr :: { Expr }
  : id '[' ExprsComma ']'           { ArrayAccess $1 (reverse $3) }
  | NewTExpr                        { $1 }

NewTExpr :: { Expr }
  : new Type                        { NewType $2 }
  | BaseExpr                        { $1 }

BaseExpr :: { Expr }
  : const_int                       { IntCExpr $1 }
  | const_float                     { FloatCExpr $1 }
  | const_char                      { CharCExpr $1 }
  | const_string                    { StringCExpr $1 }
  | true                            { TrueCExpr }
  | false                           { FalseCExpr }
  | '('')'                          { UnitCExpr }
  | id                              { FunAppExpr $1 [] }
  | id_constr                       { ConstrAppExpr $1 [] }
  | '(' Expr ')'                    { $2 }
  | begin Expr end                  { BeginExpr $2 }
  | while Expr do Expr done         { WhileExpr $2 $4 }
  | for id '=' Expr to Expr do Expr done
                                    { ForExpr $2 $4 $6 $8 }
  | for id '=' Expr downto Expr do Expr done
                                    { ForDownExpr $2 $4 $6 $8 }
  | match Expr with Clauses end     { MatchExpr $2 (reverse $4) }

Clauses :: { [Clause] }
  : Clause                          { $1 : [] }
  | Clauses '|' Clause              { $3 : $1 }

Clause :: { Clause }
  : Pattern '->' Expr               { Match $1 $3 }

Pattern :: { Pattern }
  : id_constr PatArgs               { ConstrPattern $1 (reverse $2) }
  | PatArg                          { $1 }

PatArgs :: { [Pattern] }
  : PatArg                          { $1 : [] }
  | PatArgs PatArg                  { $2 : $1 }

PatArg :: { Pattern }
  : const_int                       { IntConstPattern NoSign $1 }
  | '+' const_int                   { IntConstPattern Plus $2 }
  | '-' const_int                   { IntConstPattern Minus $2 }
  | const_float                     { FloatConstPattern NoSign $1 }
  | '+.' const_float                { FloatConstPattern Plus $2 }
  | '-.' const_float                { FloatConstPattern Minus $2 }
  | const_char                      { CharConstPattern $1 }
  | true                            { TruePattern }
  | false                           { FalsePattern }
  | id                              { IdPattern $1 }
  | id_constr                       { ConstrPattern $1 [] }
  | '(' Pattern ')'                 { $2 }

{
parseError :: Token -> Alex a
parseError t =
  let position p = "Parse error at line: " ++ (show $ getLineOfPosn p) ++
                   " and column: " ++ (show $ getColumnOfPosn p) ++ ". "
  in do
    (posn, _, _, _) <- alexGetInput
    alexError $ (position posn) ++ "Unable to process token " ++ (show t)

test :: String -> Either String Program
test s = runAlex s calc
}
