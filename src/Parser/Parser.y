{
module Parser.Parser (calc, calcRepl) where

import Lexer.Lexer (AlexPosn)
import Common.Token (Token(..), lexeme)
import Common.AST
import Parser.ParserM (Parser, lexerWrap, getPosn, throwAtPosn, throwParsingError)
}

%name calc AST
%name calcRepl REPL
%tokentype { Token }
%monad { Parser }
%lexer { lexerWrap } { EofT }
%error { parseError }

%token
  -- Keywords
  and                   { AndT }
  array                 { ArrayT }
  begin                 { BeginT }
  bool                  { BoolT }
  char                  { CharT }
  delete                { DeleteT }
  dim                   { DimT }
  do                    { DoT }
  done                  { DoneT }
  downto                { DowntoT }
  else                  { ElseT }
  end                   { EndT }
  false                 { FalseT }
  float                 { FloatT }
  for                   { ForT }
  if                    { IfT }
  in                    { InT }
  int                   { IntT }
  let                   { LetT }
  match                 { MatchT }
  mod                   { ModT }
  mutable               { MutableT }
  new                   { NewT }
  not                   { NotT }
  of                    { OfT }
  rec                   { RecT }
  ref                   { RefT }
  then                  { ThenT }
  to                    { ToT }
  true                  { TrueT }
  type                  { TypeT }
  unit                  { UnitT }
  while                 { WhileT }
  with                  { WithT }
  -- Identifiers
  id                    { IdT $$ }
  id_constr             { IdConstrT $$ }
  -- Constants
  const_int             { ConstIntT $$ }
  const_float           { ConstFloatT $$ }
  const_char            { ConstCharT $$ }
  const_string          { ConstStringT $$ }
  -- Symbols
  '->'                  { ArrowT }
  '='                   { EqualsT }
  '|'                   { BarT }
  '+'                   { PlusT }
  '-'                   { MinusT }
  '*'                   { TimesT }
  '/'                   { DivT }
  '+.'                  { PlusFloatT }
  '-.'                  { MinusFloatT }
  '*.'                  { TimesFloatT }
  '/.'                  { DivFloatT }
  '**'                  { ExpT }
  '!'                   { BangT }
  ';'                   { SemicolonT }
  '&&'                  { AndOpT }
  '||'                  { OrOpT }
  '<>'                  { NotEqualsT }
  '<'                   { LessThanT }
  '>'                   { GreaterThanT }
  '<='                  { LessThanEqT }
  '>='                  { GreaterThanEqT }
  '=='                  { NatEqOpT }
  '!='                  { NotNatEqOpT }
  ':='                  { AssignMutableT }
  '('                   { LParenT }
  ')'                   { RParenT }
  '['                   { LBracketT }
  ']'                   { RBracketT }
  ','                   { CommaT }
  ':'                   { ColonT }

%nonassoc in
%left ';'
%nonassoc IF_OP
%nonassoc ':='
%left '||'
%left '&&'
%nonassoc '=' '<>' '>' '<' '<=' '>=' '==' '!='
%left '+' '-' '+.' '-.'
%left '*' '/' '*.' '/.' mod
%right '**'
%nonassoc UN_OP
%nonassoc FUN_CALL
%nonassoc '!'
%nonassoc new

%expect 1 -- always choose shift in dangling else conflict

%%

AST :: { AST AlexPosn }
  : P AST_                          { AST (reverse $2) $1 }
  -- reversing the lists so that all definitions are in the correct order

AST_ :: { [Either (LetDef AlexPosn) (TypeDef AlexPosn)] }
  : {- emtpy -}                     { [] }
  | AST_ LetDef                     { (Left $2) : $1 }
  | AST_ TypeDef                    { (Right $2) : $1 }

P :: { AlexPosn }
  : {- empty -}                     {% getPosn }

REPL :: { ProgramOrExpr AlexPosn }
  : P REPL_AST_                     { Program (AST $2 $1) }
  | P Expr                          { Expression $2 }

REPL_AST_ :: { [Either (LetDef AlexPosn) (TypeDef AlexPosn)] }
  : {- emtpy -}                     { [] }
  | LetDef REPL_AST_                { (Left $1) : $2 }
  | TypeDef REPL_AST_               { (Right $1) : $2 }

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
  : id Params '=' Expr              { FunDef $1 (reverse $2) Nothing $4 }
  | id Params ':' Type '=' Expr     { FunDef $1 (reverse $2) (Just $4) $6 }
  | mutable id                      { VarDef $2 Nothing }
  | mutable id ':' Type             { VarDef $2 (Just $4) }
  | mutable id '[' ExprsComma ']'   { ArrayDef $2 (reverse $4) Nothing }
  | mutable id '[' ExprsComma ']' ':' Type
                                    { ArrayDef $2 (reverse $4) (Just $7) }

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
  : P ArrayType '->' Type           { Type (FunType ($2 $1) $4) $1 }
  | P ArrayType                     { $2 $1 }

ArrayType :: { AlexPosn -> Type AlexPosn }
  : array Dims of P ArrayType       { Type (ArrayType $2 ($5 $4)) }
  | RefType                         { $1 }

Dims :: { Int }
  : {- empty -}                     { 1 }
  | '[' Stars ']'                   { $2 }

Stars :: { Int }
  : '*'                             { 1 }
  | Stars ',' '*'                   { $1 + 1 }

RefType :: { AlexPosn -> Type AlexPosn }
  : RefType ref                     { \p -> Type (RefType ($1 p)) p }
  | BaseType_                       { $1 }

BaseType_ :: { AlexPosn -> Type AlexPosn }
  : unit                            { Type UnitType }
  | int                             { Type IntType }
  | char                            { Type CharType }
  | bool                            { Type BoolType }
  | float                           { Type FloatType }
  | id                              { Type (UserDefinedType $1) }
  | '(' Type ')'                    { const $2 }

Expr :: { Expr AlexPosn }
  : P Expr_                         { $2 $1 }

Expr_ :: { AlexPosn -> Expr AlexPosn }
  : LetDef_ in P Expr_              { \p -> LetIn ($1 p) ($4 $3) p }
  | Expr_ ';' P Expr_               { \p -> Expr (BinOpExpr SemicolonOp ($1 p) ($4 $3)) p }
  | if P Expr_ then P Expr_
                        %prec IF_OP { Expr (IfThenExpr ($3 $2) ($6 $5)) }
  | if P Expr_ then P Expr_ else P Expr_
                        %prec IF_OP { Expr (IfThenElseExpr ($3 $2) ($6 $5) ($9 $8) ) }
  | Expr_ ':=' P Expr_              { \p -> Expr (BinOpExpr AssignMutableOp ($1 p) ($4 $3)) p }
  | Expr_ '||' P Expr_              { \p -> Expr (BinOpExpr OrOp ($1 p) ($4 $3)) p }
  | Expr_ '&&' P Expr_              { \p -> Expr (BinOpExpr AndOp ($1 p) ($4 $3)) p }
  | Expr_ '='  P Expr_              { \p -> Expr (BinOpExpr EqOp ($1 p) ($4 $3)) p }
  | Expr_ '<>' P Expr_              { \p -> Expr (BinOpExpr NotEqOp ($1 p) ($4 $3)) p }
  | Expr_ '<'  P Expr_              { \p -> Expr (BinOpExpr LTOp ($1 p) ($4 $3)) p }
  | Expr_ '>'  P Expr_              { \p -> Expr (BinOpExpr GTOp ($1 p) ($4 $3)) p }
  | Expr_ '<=' P Expr_              { \p -> Expr (BinOpExpr LEqOp ($1 p) ($4 $3)) p }
  | Expr_ '>=' P Expr_              { \p -> Expr (BinOpExpr GEqOp ($1 p) ($4 $3)) p }
  | Expr_ '==' P Expr_              { \p -> Expr (BinOpExpr NatEqOp ($1 p) ($4 $3)) p }
  | Expr_ '!=' P Expr_              { \p -> Expr (BinOpExpr NotNatEqOp ($1 p) ($4 $3)) p }
  | Expr_ '+'  P Expr_              { \p -> Expr (BinOpExpr PlusOp ($1 p) ($4 $3)) p }
  | Expr_ '-'  P Expr_              { \p -> Expr (BinOpExpr MinusOp ($1 p) ($4 $3)) p }
  | Expr_ '*'  P Expr_              { \p -> Expr (BinOpExpr TimesOp ($1 p) ($4 $3)) p }
  | Expr_ '/'  P Expr_              { \p -> Expr (BinOpExpr DivOp ($1 p) ($4 $3)) p }
  | Expr_ '+.' P Expr_              { \p -> Expr (BinOpExpr PlusFloatOp ($1 p) ($4 $3)) p }
  | Expr_ '-.' P Expr_              { \p -> Expr (BinOpExpr MinusFloatOp ($1 p) ($4 $3)) p }
  | Expr_ '*.' P Expr_              { \p -> Expr (BinOpExpr TimesFloatOp ($1 p) ($4 $3)) p }
  | Expr_ '/.' P Expr_              { \p -> Expr (BinOpExpr DivFloatOp ($1 p) ($4 $3)) p }
  | Expr_ mod  P Expr_              { \p -> Expr (BinOpExpr ModOp ($1 p) ($4 $3)) p }
  | Expr_ '**' P Expr_              { \p -> Expr (BinOpExpr ExpOp ($1 p) ($4 $3)) p }
  | '+' P Expr_         %prec UN_OP { Expr (UnOpExpr PlusUnOp ($3 $2)) }
  | '-' P Expr_         %prec UN_OP { Expr (UnOpExpr MinusUnOp ($3 $2)) }
  | '+.' P Expr_        %prec UN_OP { Expr (UnOpExpr PlusFloatUnOp ($3 $2)) }
  | '-.' P Expr_        %prec UN_OP { Expr (UnOpExpr MinusFloatUnOp ($3 $2)) }
  | not P Expr_         %prec UN_OP { Expr (UnOpExpr NotOp ($3 $2)) }
  | delete P Expr_      %prec UN_OP { Expr (DeleteExpr ($3 $2)) }
  | dim id                          { Expr (ArrayDim $2 1) }
  | dim const_int id                { Expr (ArrayDim $3 $2) }
  | id Args          %prec FUN_CALL { Expr (FunAppExpr $1 (reverse $2)) }
  | id_constr Args   %prec FUN_CALL { Expr (ConstrAppExpr $1 (reverse $2)) }
  | '!' P Expr_                     { Expr (UnOpExpr BangOp ($3 $2)) }
  | id '[' ExprsComma ']'           { Expr (ArrayAccess $1 (reverse $3)) }
  | new Type                        { NewType $2 }
  | BaseExpr_                       { $1 }

Args :: { [Expr AlexPosn] }
  : Arg                             { $1 : [] }
  | Args Arg                        { $2 : $1 }

Arg :: { Expr AlexPosn }
  : P '!' Arg                       { Expr (UnOpExpr BangOp $3) $1 }
  | P id '[' ExprsComma ']'         { Expr (ArrayAccess $2 (reverse $4)) $1 }
  | P new Type                      { NewType $3 $1}
  | BaseExpr                        { $1 }

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
  : P Pattern_ '->' Expr            { Match ($2 $1) $4 $1 }

Pattern :: { Pattern AlexPosn }
  : P Pattern_                      { $2 $1 }

Pattern_ :: { AlexPosn -> Pattern AlexPosn }
  : id_constr PatArgs               { Pattern (ConstrPattern $1 (reverse $2)) }
  | PatArg_                         { $1 }

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
    posn <- getPosn
    throwAtPosn posn $ throwParsingError $ "Unable to process token " ++ lexeme t
}
