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

%%

Program :: { Program }
  : Program_                        { reverse $1 }
-- reversing the list so that all definitions are in the correct order

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
  : id Params '=' Expr              { Def $1 (reverse $2) $4}
  | id Params ':' Type '=' Expr     { TypedDef $1 (reverse $2) $4 $6 }
  | mutable id                      { MutDef $2 }
  | mutable id ':' Type             { MutTypedDef $2 $4}
  | mutable id '[' ExprsComma ']'   { ArrayDef $2 (reverse $4)}
  | mutable id '[' ExprsComma ']' ':' Type
                                    { ArrayTypedDef $2 (reverse $4) $7 }

Params :: { [Param] }
  : {- empty -}                     { [] }
  | Params Param                    { $2 : $1 }

ExprsComma :: { [Expr] }
  : Expr                            { $1 : [] }
  | ExprsComma ',' Expr             { $3 : $1 }

Exprs :: { [Expr] }
  : {- empty -}                     { [] }
  | Exprs Expr                      { $2 : $1 }

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
  : unit                            { UnitType }
  | int                             { IntType }
  | char                            { CharType }
  | bool                            { BoolType }
  | float                           { FloatType }
  | '(' Type ')'                    { NestedType $2 }
  | Type '->' Type                  { FunType $1 $3 }
  | Type ref                        { RefType $1 }
  | array Dims of Type              { ArrayType $2 $4 }
  | id                              { UserDefinedType $1 }

Dims :: { Int }
  : {- empty -}                     { 1 }
  | '[' Stars ']'                   { $2 }

Stars :: { Int }
  : '*'                             { 1 }
  | Stars ',' '*'                   { $1 + 1 }

Expr :: { Expr }
  : const_int                       { IntCExpr $1 }
  | const_float                     { FloatCExpr $1 }
  | const_char                      { CharCExpr $1 }
  | const_string                    { StringCExpr $1 }
  | true                            { TrueCExpr }
  | false                           { FalseCExpr }
  | '('')'                          { UnitCExpr }
  | '(' Expr ')'                    { NestedExpr $2 }
  | UnOp Expr                       { UnOpExpr $1 $2 }
  | Expr BinOp Expr                 { BinOpExpr $2 $1 $3 }
  | id Exprs                        { FunAppExpr $1 (reverse $2) }
  | id_constr Exprs                 { ConstrAppExpr $1 (reverse $2) }
  | id '[' ExprsComma ']'           { ArrayAccess $1 $3 }
  | dim id                          { ArrayDim $2 }
  | dim const_int id                { ArrayDimMult $3 $2 }
  | new Type                        { NewType $2 }
  | delete Expr                     { DeleteExpr $2 }
  | LetDef in Expr                  { LetIn $1 $3 }
  | begin Expr end                  { BeginExpr $2 }
  | if Expr then Expr               { IfThenExpr $2 $4 }
  | if Expr then Expr else Expr     { IfThenElseExpr $2 $4 $6 }
  | while Expr do Expr done         { WhileExpr $2 $4 }
  | for id '=' Expr to Expr do Expr done
                                    { ForExpr $2 $4 $6 $8 }
  | for id '=' Expr downto Expr do Expr done
                                    { ForDownExpr $2 $4 $6 $8 }
  | match Expr with Clauses end     { MatchExpr $2 (reverse $4) }

Clauses :: { [Clause] }
  : Clause                          { $1 : [] }
  | Clauses '|' Clause              { $3 : $1 }

UnOp :: { UnOp }
  : '+'                             { PlusUnOp }
  | '-'                             { MinusUnOp }
  | '+.'                            { PlusFloatUnOp }
  | '-.'                            { MinusFloatUnOp }
  | '!'                             { BangOp }
  | not                             { NotOp }

BinOp :: { BinOp }
  : '+'                             { PlusOp }
  | '-'                             { MinusOp }
  | '*'                             { TimesOp }
  | '/'                             { DivOp }
  | '+.'                            { PlusFloatOp }
  | '-.'                            { MinusFloatOp }
  | '*.'                            { TimesFloatOp }
  | '/.'                            { DivFloatOp }
  | mod                             { ModOp }
  | '**'                            { ExpOp }
  | '='                             { AssignOp }
  | '<>'                            { NotStructEqOp }
  | '<'                             { LTOp }
  | '>'                             { GTOp }
  | '<='                            { LEqOp }
  | '>='                            { GEqOp }
  | '=='                            { NatEqOp }
  | '!='                            { NotNatEqOp }
  | '&&'                            { AndOp }
  | '||'                            { OrOp }
  | ';'                             { SemicolonOp }
  | ':='                            { AssignMutableOp }

Clause :: { Clause }
  : Pattern '->' Expr               { Match $1 $3}

Pattern :: { Pattern }
  : const_int                       { IntConstPattern $1 }
  | '+' const_int                   { PlusIntConstPattern $2 }
  | '-' const_int                   { MinusIntConstPattern $2 }
  | const_float                     { FloatConstPattern $1 }
  | '+.' const_float                { PlusFloatConstPattern $2 }
  | '-.' const_float                { MinusFloatConstPattern $2 }
  | const_char                      { CharConstPattern $1 }
  | true                            { TruePattern }
  | false                           { FalsePattern }
  | id                              { IdPattern $1 }
  | '(' Pattern ')'                 { NestedPattern $2 }
  | id_constr Patterns              { ConstrPattern $1 (reverse $2) }

Patterns :: { [Pattern] }
  : {- empty -}                     { [] }
  | Patterns Pattern                { $2 : $1 }

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
