{
module Parser.Parser (calc, parse, parseFile) where

import Lexer.Lexer (getLineOfPosn, getColumnOfPosn, AlexPosn)
import Common.Token
    ( FloatConstant,
      Identifier,
      ConstrIdentifier,
      CharConstant,
      IntConstant,
      StringConstant,
      Token(..) )
import Common.AST
import Parser.ParserM (Parser, lexerWrap, runParser, getAlexPos,
                       ParserState, initParserState,
                       Error, throwParsingError)
}

%name calc
%tokentype { (Token, AlexPosn) }
%monad { Parser }
%lexer { lexerWrap } { (T_eof, _) }
%error { parseError }

%token
  -- Keywords
  and                   { (T_and, _) }
  array                 { (T_array, _) }
  begin                 { (T_begin, _) }
  bool                  { (T_bool, _) }
  char                  { (T_char, _) }
  delete                { (T_delete, _) }
  dim                   { (T_dim, _) }
  do                    { (T_do, _) }
  done                  { (T_done, _) }
  downto                { (T_downto, _) }
  else                  { (T_else, _) }
  end                   { (T_end, _) }
  false                 { (T_false, _) }
  float                 { (T_float, _) }
  for                   { (T_for, _) }
  if                    { (T_if, _) }
  in                    { (T_in, _) }
  int                   { (T_int, _) }
  let                   { (T_let, _) }
  match                 { (T_match, _) }
  mod                   { (T_mod, _) }
  mutable               { (T_mutable, _) }
  new                   { (T_new, _) }
  not                   { (T_not, _) }
  of                    { (T_of, _) }
  rec                   { (T_rec, _) }
  ref                   { (T_ref, _) }
  then                  { (T_then, _) }
  to                    { (T_to, _) }
  true                  { (T_true, _) }
  type                  { (T_type, _) }
  unit                  { (T_unit, _) }
  while                 { (T_while, _) }
  with                  { (T_with, _) }
  -- Identifiers
  id                    { (T_id _, _) }
  id_constr             { (T_id_constr _, _) }
  -- Constants
  const_int             { (T_const_int _, _) }
  const_float           { (T_const_float _, _) }
  const_char            { (T_const_char _, _) }
  const_string          { (T_const_string _, _) }
  -- Symbols
  '->'                  { (T_arrow, _) }
  '='                   { (T_equals, _) }
  '|'                   { (T_bar, _) }
  '+'                   { (T_plus, _) }
  '-'                   { (T_minus, _) }
  '*'                   { (T_times, _) }
  '/'                   { (T_div, _) }
  '+.'                  { (T_plus_float, _) }
  '-.'                  { (T_minus_float, _) }
  '*.'                  { (T_times_float, _) }
  '/.'                  { (T_div_float, _) }
  '**'                  { (T_exp, _) }
  '!'                   { (T_bang, _) }
  ';'                   { (T_semicolon, _) }
  '&&'                  { (T_and_op, _) }
  '||'                  { (T_or_op, _) }
  '<>'                  { (T_not_equals, _) }
  '<'                   { (T_less_than, _) }
  '>'                   { (T_greater_than, _) }
  '<='                  { (T_less_than_eq, _) }
  '>='                  { (T_greater_than_eq, _) }
  '=='                  { (T_nat_eq_op, _) }
  '!='                  { (T_not_nat_eq_op, _) }
  ':='                  { (T_assign_mutable, _) }
  '('                   { (T_lparen, _) }
  ')'                   { (T_rparen, _) }
  '['                   { (T_lbracket, _) }
  ']'                   { (T_rbracket, _) }
  ','                   { (T_comma, _) }
  ':'                   { (T_colon, _) }

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

Program :: { AST AlexPosn }
  : Program_                        { reverse $1 }
-- reversing the lists so that all definitions are in the correct order

Program_ :: { AST AlexPosn }
  : {- emtpy -}                     { [] }
  | Program_ LetDef                 { (Left $2) : $1 }
  | Program_ TypeDef                { (Right $2) : $1 }

LetDef :: { LetDefTag AlexPosn }
  : let Defs                        { (Let (reverse $2), pos $1) }
  | let rec Defs                    { (LetRec (reverse $3), pos $1) }

Defs :: { [DefTag AlexPosn] }
  : Def                             { $1 : [] }
  | Defs and Def                    { $3 : $1 }

Def :: { DefTag AlexPosn }
  : id Params '=' Expr              { (FunDef (tokId $1) (reverse $2) $4, pos $1) }
  | id Params ':' Type '=' Expr     { (FunDefTyped (tokId $1) (reverse $2) $4 $6, pos $1) }
  | mutable id                      { (VarDef (tokId $2), pos $1) }
  | mutable id ':' Type             { (VarDefTyped (tokId $2) $4, pos $1) }
  | mutable id '[' ExprsComma ']'   { (ArrayDef (tokId $2) (reverse $4), pos $1) }
  | mutable id '[' ExprsComma ']' ':' Type
                                    { (ArrayDefTyped (tokId $2) (reverse $4) $7, pos $1) }

Params :: { [ParamTag AlexPosn] }
  : {- empty -}                     { [] }
  | Params Param                    { $2 : $1 }

ExprsComma :: { [ExprTag AlexPosn] }
  : Expr                            { $1 : [] }
  | ExprsComma ',' Expr             { $3 : $1 }

TypeDef :: { TypeDefTag AlexPosn }
  : type TDefs                      { (TypeDef (reverse $2), pos $1) }

TDefs :: { [TDefTag AlexPosn] }
  : TDef                            { $1 : [] }
  | TDefs and TDef                  { $3 : $1 }

TDef :: { TDefTag AlexPosn }
  : id '=' Constrs                  { (TDef (tokId $1) (reverse $3), pos $1) }

Constrs :: { [ConstrTag AlexPosn] }
  : Constr                          { $1 : [] }
  | Constrs '|' Constr              { $3 : $1 }

Constr :: { ConstrTag AlexPosn }
  : id_constr                       { (Constr (tokIdConstr $1) [], pos $1) }
  | id_constr of Types              { (Constr (tokIdConstr $1) (reverse $3), pos $1) }

Types :: { [TypeTag AlexPosn] }
  : Type                            { $1 : [] }
  | Types Type                      { $2 : $1 }

Param :: { ParamTag AlexPosn }
  : id                              { (Param (tokId $1), pos $1) }
  | '(' id ':' Type ')'             { (TypedParam (tokId $2) $4, pos $1) }

Type :: { TypeTag AlexPosn }
  : ArrayType '->' Type             { (FunType $1 $3, snd $1) }
  | ArrayType                       { $1 }

ArrayType :: { TypeTag AlexPosn }
  : array Dims of ArrayType         { (ArrayType $2 $4, pos $1) }
  | RefType                         { $1 }

Dims :: { Int }
  : {- empty -}                     { 1 }
  | '[' Stars ']'                   { $2 }

Stars :: { Int }
  : '*'                             { 1 }
  | Stars ',' '*'                   { $1 + 1 }

RefType :: { TypeTag AlexPosn }
  : RefType ref                     { (RefType $1, snd $1) }
  | BaseType                        { $1 }

BaseType :: { TypeTag AlexPosn }
  : unit                            { (UnitType, pos $1) }
  | int                             { (IntType, pos $1) }
  | char                            { (CharType, pos $1) }
  | bool                            { (BoolType, pos $1) }
  | float                           { (FloatType, pos $1) }
  | id                              { (UserDefinedType (tokId $1), pos $1) }
  | '(' Type ')'                    { $2 }

Expr :: { ExprTag AlexPosn }
  : LetExpr                         { $1 }
  | SemicolonExpr                   { $1 }
  | SemicolonLetExpr                { $1 }

LetExpr :: { ExprTag AlexPosn }
  : LetDef in Expr                  { (LetIn $1 $3, snd $1) }

SemicolonExpr :: { ExprTag AlexPosn }
  : SemicolonExpr ';' SemicolonExpr { (BinOpExpr SemicolonOp $1 $3, snd $1) }
  | IfExpr                          { $1 }

SemicolonLetExpr :: { ExprTag AlexPosn } -- Cannot have let in lhs
  : SemicolonExpr ';' LetExpr       { (BinOpExpr SemicolonOp $1 $3, snd $1) }

LetIfExpr :: { ExprTag AlexPosn }
  : LetDef in IfExpr                { (LetIn $1 $3, snd $1) }
  | IfExpr                          { $1 }

IfExpr :: { ExprTag AlexPosn } -- Cannot have semicolon in last Expr
  : if Expr then LetIfExpr          { (IfThenExpr $2 $4, pos $1) }
  | if Expr then LetIfExpr else LetIfExpr
                                    { (IfThenElseExpr $2 $4 $6, pos $1) }
  | LogicalExpr                     { $1 }

LogicalExpr :: { ExprTag AlexPosn }
  : LogicalExpr ':=' LogicalExpr    { (BinOpExpr AssignMutableOp $1 $3, snd $1) }
  | LogicalExpr '||' LogicalExpr    { (BinOpExpr OrOp $1 $3, snd $1) }
  | LogicalExpr '&&' LogicalExpr    { (BinOpExpr AndOp $1 $3, snd $1) }
  | LogicalExpr '='  LogicalExpr    { (BinOpExpr EqOp $1 $3, snd $1) }
  | LogicalExpr '<>' LogicalExpr    { (BinOpExpr NotEqOp $1 $3, snd $1) }
  | LogicalExpr '<'  LogicalExpr    { (BinOpExpr LTOp $1 $3, snd $1) }
  | LogicalExpr '>'  LogicalExpr    { (BinOpExpr GTOp $1 $3, snd $1) }
  | LogicalExpr '<=' LogicalExpr    { (BinOpExpr LEqOp $1 $3, snd $1) }
  | LogicalExpr '>=' LogicalExpr    { (BinOpExpr GEqOp $1 $3, snd $1) }
  | LogicalExpr '==' LogicalExpr    { (BinOpExpr NatEqOp $1 $3, snd $1) }
  | LogicalExpr '!=' LogicalExpr    { (BinOpExpr NotNatEqOp $1 $3, snd $1) }
  | ArithmExpr                      { $1 }

ArithmExpr :: { ExprTag AlexPosn }
  : ArithmExpr '+'  ArithmExpr      { (BinOpExpr PlusOp $1 $3, snd $1) }
  | ArithmExpr '-'  ArithmExpr      { (BinOpExpr MinusOp $1 $3, snd $1) }
  | ArithmExpr '*'  ArithmExpr      { (BinOpExpr TimesOp $1 $3, snd $1) }
  | ArithmExpr '/'  ArithmExpr      { (BinOpExpr DivOp $1 $3, snd $1) }
  | ArithmExpr '+.' ArithmExpr      { (BinOpExpr PlusFloatOp $1 $3, snd $1) }
  | ArithmExpr '-.' ArithmExpr      { (BinOpExpr MinusFloatOp $1 $3, snd $1) }
  | ArithmExpr '*.' ArithmExpr      { (BinOpExpr TimesFloatOp $1 $3, snd $1) }
  | ArithmExpr '/.' ArithmExpr      { (BinOpExpr DivFloatOp $1 $3, snd $1) }
  | ArithmExpr mod  ArithmExpr      { (BinOpExpr ModOp $1 $3, snd $1) }
  | ArithmExpr '**' ArithmExpr      { (BinOpExpr ExpOp $1 $3, snd $1) }
  | UnOpExpr                        { $1 }

UnOpExpr :: { ExprTag AlexPosn }
  : '+' UnOpExpr                    { (UnOpExpr PlusUnOp $2, pos $1) }
  | '-' UnOpExpr                    { (UnOpExpr MinusUnOp $2, pos $1) }
  | '+.' UnOpExpr                   { (UnOpExpr PlusFloatUnOp $2, pos $1) }
  | '-.' UnOpExpr                   { (UnOpExpr MinusFloatUnOp $2, pos $1) }
  | not UnOpExpr                    { (UnOpExpr NotOp $2, pos $1) }
  | delete UnOpExpr                 { (DeleteExpr $2, pos $1) }
  | dim id                          { (ArrayDim (tokId $2) 1, pos $1) }
  | dim const_int id                { (ArrayDim (tokId $3) (tokInt $2), pos $1) }
  | FunAppExpr                      { $1 }

FunAppExpr :: { ExprTag AlexPosn }
  : id Args                         { (FunAppExpr (tokId $1) (reverse $2), pos $1) }
  | id_constr Args                  { (ConstrAppExpr (tokIdConstr $1) (reverse $2), pos $1) }
  | DerefExp                        { $1 }

Args :: { [ExprTag AlexPosn] }
  : DerefExp                        { $1 : [] }
  | Args DerefExp                   { $2 : $1 }

DerefExp :: { ExprTag AlexPosn }
  : '!' DerefExp                    { (UnOpExpr BangOp $2, pos $1) }
  | ArrayExpr                       { $1 }

ArrayExpr :: { ExprTag AlexPosn }
  : id '[' ExprsComma ']'           { (ArrayAccess (tokId $1) (reverse $3), pos $1) }
  | NewTExpr                        { $1 }

NewTExpr :: { ExprTag AlexPosn }
  : new Type                        { (NewType $2, pos $1) }
  | BaseExpr                        { $1 }

BaseExpr :: { ExprTag AlexPosn }
  : const_int                       { (IntCExpr (tokInt $1), pos $1) }
  | const_float                     { (FloatCExpr (tokFloat $1), pos $1) }
  | const_char                      { (CharCExpr (tokChar $1), pos $1) }
  | const_string                    { (StringCExpr (tokString $1), pos $1) }
  | true                            { (TrueCExpr, pos $1) }
  | false                           { (FalseCExpr, pos $1) }
  | '('')'                          { (UnitCExpr, pos $1) }
  | id                              { (FunAppExpr (tokId $1) [], pos $1) }
  | id_constr                       { (ConstrAppExpr (tokIdConstr $1) [], pos $1) }
  | '(' Expr ')'                    { $2 }
  | begin Expr end                  { (BeginExpr $2, pos $1) }
  | while Expr do Expr done         { (WhileExpr $2 $4, pos $1) }
  | for id '=' Expr to Expr do Expr done
                                    { (ForExpr (tokId $2) $4 $6 $8, pos $1) }
  | for id '=' Expr downto Expr do Expr done
                                    { (ForDownExpr (tokId $2) $4 $6 $8, pos $1) }
  | match Expr with Clauses end     { (MatchExpr $2 (reverse $4), pos $1) }

Clauses :: { [ClauseTag AlexPosn] }
  : Clause                          { $1 : [] }
  | Clauses '|' Clause              { $3 : $1 }

Clause :: { ClauseTag AlexPosn }
  : Pattern '->' Expr               { (Match $1 $3, snd $1) }

Pattern :: { PatternTag AlexPosn }
  : id_constr PatArgs               { (ConstrPattern (tokIdConstr $1) (reverse $2), pos $1) }
  | PatArg                          { $1 }

PatArgs :: { [PatternTag AlexPosn] }
  : PatArg                          { $1 : [] }
  | PatArgs PatArg                  { $2 : $1 }

PatArg :: { PatternTag AlexPosn }
  : const_int                       { (IntConstPattern NoSign (tokInt $1), pos $1) }
  | '+' const_int                   { (IntConstPattern Plus (tokInt $2), pos $1) }
  | '-' const_int                   { (IntConstPattern Minus (tokInt $2), pos $1) }
  | const_float                     { (FloatConstPattern NoSign (tokFloat $1), pos $1) }
  | '+.' const_float                { (FloatConstPattern Plus (tokFloat $2), pos $1) }
  | '-.' const_float                { (FloatConstPattern Minus (tokFloat $2), pos $1) }
  | const_char                      { (CharConstPattern (tokChar $1), pos $1) }
  | true                            { (TruePattern, pos $1) }
  | false                           { (FalsePattern, pos $1) }
  | id                              { (IdPattern (tokId $1), pos $1) }
  | id_constr                       { (ConstrPattern (tokIdConstr $1) [], pos $1) }
  | '(' Pattern ')'                 { $2 }

{
tokInt :: (Token, AlexPosn) -> IntConstant
tokInt (T_const_int i, _) = i

tokFloat :: (Token, AlexPosn) -> FloatConstant
tokFloat (T_const_float i, _) = i

tokChar :: (Token, AlexPosn) -> CharConstant
tokChar (T_const_char i, _) = i

tokString :: (Token, AlexPosn) -> StringConstant
tokString (T_const_string i, _) = i

tokId :: (Token, AlexPosn) -> Identifier
tokId (T_id i, _) = i

tokIdConstr :: (Token, AlexPosn) -> Identifier
tokIdConstr (T_id_constr i, _) = i

pos :: (Token, AlexPosn) -> AlexPosn
pos = snd

-- Handle errors
parseError :: (Token, AlexPosn) -> Parser a
parseError (t, posn) =
  let position p = "Error at line: " ++ show (getLineOfPosn p) ++
                   " and column: " ++ show (getColumnOfPosn p) ++ ". "
  in do
    -- posn <- getAlexPos
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
