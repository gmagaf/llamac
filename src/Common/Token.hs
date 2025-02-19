module Common.Token(Token(..),
                    Identifier,
                    ConstrIdentifier,
                    IntConstant,
                    FloatConstant,
                    CharConstant,
                    StringConstant) where

-- Mappings of Llama constants to Haskell values
type Identifier = String
type ConstrIdentifier = String
type IntConstant = Int
type FloatConstant = Float
type CharConstant = String
type StringConstant = String

data Token
  -- Keywords
  = T_and
  | T_array
  | T_begin
  | T_bool
  | T_char
  | T_delete
  | T_dim
  | T_do
  | T_done
  | T_downto
  | T_else
  | T_end
  | T_false
  | T_float
  | T_for
  | T_if
  | T_in
  | T_int
  | T_let
  | T_match
  | T_mod
  | T_mutable
  | T_new
  | T_not
  | T_of
  | T_rec
  | T_ref
  | T_then
  | T_to
  | T_true
  | T_type
  | T_unit
  | T_while
  | T_with
  -- Identifiers
  | T_id Identifier
  | T_id_constr ConstrIdentifier
  -- Constants
  | T_const_int IntConstant
  | T_const_float FloatConstant
  | T_const_char CharConstant
  | T_const_string StringConstant
  -- Symbols
  | T_arrow
  | T_assign
  | T_bar
  | T_plus
  | T_minus
  | T_times
  | T_div
  | T_plus_float
  | T_minus_float
  | T_times_float
  | T_div_float
  | T_exp
  | T_bang
  | T_semicolon
  | T_and_op
  | T_or_op
  | T_not_struct_eq_op
  | T_less_than
  | T_more_than
  | T_less_than_eq
  | T_more_than_eq
  | T_nat_eq_op
  | T_not_nat_eq_op
  | T_assign_mutable
  | T_lparen
  | T_rparen
  | T_lbracket
  | T_rbracket
  | T_comma
  | T_colon
  | T_eof
  deriving (Eq)

instance Show Token where
  show t = case t of
    -- Keywords
    T_and               -> "and"
    T_array             -> "array"
    T_begin             -> "begin"
    T_bool              -> "bool"
    T_char              -> "char"
    T_delete            -> "delete"
    T_dim               -> "dim"
    T_do                -> "do"
    T_done              -> "done"
    T_downto            -> "downto"
    T_else              -> "else"
    T_end               -> "end"
    T_false             -> "false"
    T_float             -> "float"
    T_for               -> "for"
    T_if                -> "if"
    T_in                -> "in"
    T_int               -> "int"
    T_let               -> "let"
    T_match             -> "match"
    T_mod               -> "mod"
    T_mutable           -> "mutable"
    T_new               -> "new"
    T_not               -> "not"
    T_of                -> "of"
    T_rec               -> "rec"
    T_ref               -> "ref"
    T_then              -> "then"
    T_to                -> "to"
    T_true              -> "true"
    T_type              -> "type"
    T_unit              -> "unit"
    T_while             -> "while"
    T_with              -> "with"
    -- Identifiers
    T_id v              -> "id:" ++ v
    T_id_constr v       -> "constrId:" ++ v
    -- Constants
    T_const_int v       -> "intC:" ++ (show v)
    T_const_float v     -> "floatC:" ++ (show v)
    T_const_char v      -> "charC:" ++ v
    T_const_string v    -> "stringC:" ++ v
    -- Symbols
    T_arrow             -> "'->'"
    T_assign            -> "'='"
    T_bar               -> "'|'"
    T_plus              -> "'+'"
    T_minus             -> "'-'"
    T_times             -> "'*'"
    T_div               -> "'/'"
    T_plus_float        -> "'+.'"
    T_minus_float       -> "'-.'"
    T_times_float       -> "'*.'"
    T_div_float         -> "'/.'"
    T_exp               -> "'**'"
    T_bang              -> "'!'"
    T_semicolon         -> "';'"
    T_and_op            -> "'&&'"
    T_or_op             -> "'||'"
    T_not_struct_eq_op  -> "'<>'"
    T_less_than         -> "'<'"
    T_more_than         -> "'>'"
    T_less_than_eq      -> "'<='"
    T_more_than_eq      -> "'>='"
    T_nat_eq_op         -> "'=='"
    T_not_nat_eq_op     -> "'!='"
    T_assign_mutable    -> "':='"
    T_lparen            -> "'('"
    T_rparen            -> "')'"
    T_lbracket          -> "'['"
    T_rbracket          -> "']'"
    T_comma             -> "','"
    T_colon             -> "':'"
    T_eof               -> "eof"
