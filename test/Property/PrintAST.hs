module Property.PrintAST where

import Common.AST
import Common.Token
import Debug.Trace (trace)

class Pretty a where
  prettyPrec :: Int -> a -> ShowS
  prettyPrec _ a s = pretty a ++ s
  pretty :: a -> String
  pretty a = prettyPrec 0 a ""
  showPretty :: a -> ShowS
  showPretty = showString . pretty

instance Pretty Token where
  pretty t = case t of
    T_id v           -> v
    T_id_constr v    -> v
    T_const_int v    -> show v
    T_const_float v  -> show v
    T_const_char v   -> "'" ++ v ++ "'"
    T_const_string v -> "\"" ++ v ++ "\""
    keyword          -> show keyword

prettyId :: Identifier -> ShowS
prettyId = showPretty . T_id

prettyConstrId :: ConstrIdentifier -> ShowS
prettyConstrId = showPretty . T_id_constr

prettyIntC :: IntConstant -> ShowS
prettyIntC = showPretty . T_const_int

prettyFloatC :: FloatConstant -> ShowS
prettyFloatC = showPretty . T_const_float

prettyCharC :: CharConstant -> ShowS
prettyCharC = showPretty . T_const_char

prettyStringC :: StringConstant -> ShowS
prettyStringC = showPretty . T_const_string

prettySepList :: Pretty a => String -> [a] -> String
prettySepList _ []     = ""
prettySepList _ (x:[]) = pretty x
prettySepList s (x:xs) = showPretty x . showString s $ prettySepList s xs

prettyPrecSepList :: Pretty a => Int -> String -> [a] -> ShowS
prettyPrecSepList _ _ []     = id
prettyPrecSepList d _ (x:[]) = prettyPrec d x
prettyPrecSepList d s (x:xs) = prettyPrec d x . showString s . prettyPrecSepList d s xs

-- False omits parentheses whenever possible
always :: Bool
always = False

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyPrec d (Left a)  = prettyPrec d a
  prettyPrec d (Right b) = prettyPrec d b

prettyProgram :: Program -> String
prettyProgram = prettySepList "\n\n"

instance Pretty TypeDef where
  pretty (Type tDefs) = showPretty T_type . showString " " $
    prettySepList ("\n" ++ pretty T_and ++ "  ") tDefs

instance Pretty TDef where
  pretty (TDef ide constrs) = prettyId ide . showString " " .
    showPretty T_assign . showString " " $
    prettySepList (" " ++ pretty T_bar ++ " ") constrs

instance Pretty Constr where
  pretty (Constr ide []) = prettyConstrId ide ""
  pretty (Constr ide ts) = prettyConstrId ide . showString " " .
    showPretty T_of . showString " " $ prettySepList " " ts

instance Pretty Type where
  prettyPrec d t =
    let
      ref_prec = 3
      array_prec = 2
      fun_prec = 1
      showsStars 1 = showPretty T_times
      showsStars s = showsStars (s - 1) .
        showPretty T_comma . showString " " . showPretty T_times
    in case t of
      UnitType -> showPretty T_unit
      IntType -> showPretty T_int
      CharType -> showPretty T_char
      BoolType -> showPretty T_bool
      FloatType -> showPretty T_float
      UserDefinedType ide -> prettyId ide
      RefType t -> showParen (always || d > ref_prec) $
            prettyPrec (ref_prec + 1) t .
            showString " " .
            showPretty T_ref
      ArrayType 1 t -> showParen (always || d > array_prec) $
            showPretty T_array . showString " " .
            showPretty T_of . showString " " .
            prettyPrec (array_prec + 1) t
      ArrayType n t -> showParen (always || d > array_prec) $
            showPretty T_array . showString " " .
            showPretty T_lbracket . showsStars n . showPretty T_rbracket .
            showString " " . showPretty T_of . showString " " .
            prettyPrec (array_prec + 1) t
      FunType u v -> showParen (always || d > fun_prec) $
            prettyPrec (fun_prec + 1) u .
            showString " " . showPretty T_arrow . showString " " .
            prettyPrec fun_prec v

instance Pretty LetDef where
  pretty (Let defs) = showPretty T_let . showString " " $
    prettySepList ("\n" ++ pretty T_and ++ " ") defs
  pretty (LetRec defs) = showPretty T_let . showString " " .
    showPretty T_rec . showString " " $
    prettySepList ("\n" ++ pretty T_and ++ " ") defs

instance Pretty Def where
  pretty def = case def of
    FunDef i ps e -> prettyId i . showString " " . prettyPrecSepList 0 " " ps .
      showString " " . showPretty T_assign . showString " " $ pretty e
    FunDefTyped i ps t e -> prettyId i . showString " " . prettyPrecSepList 0 " " ps .
      showString " " . showPretty T_colon . showString " " . showPretty t .
      showString " " . showPretty T_assign . showString " " $ pretty e
    VarDef i -> showPretty T_mutable . showString " " $ prettyId i ""
    VarDefTyped i t -> showPretty T_mutable . showString " " . prettyId i .
      showString " " . showPretty T_colon . showString " " $ showPretty t ""
    ArrayDef i es -> showPretty T_mutable . showString " " . prettyId i .
      showString " " . showPretty T_lbracket . prettyPrecSepList 0 ", " es $
      showPretty T_rbracket ""
    ArrayDefTyped i es t -> showPretty T_mutable . showString " " . prettyId i .
      showString " " . showPretty T_lbracket . prettyPrecSepList 0 ", " es .
      showPretty T_rbracket . showString " " . showPretty T_colon .
      showString " " $ showPretty t ""

instance Pretty Param where
  pretty (Param i) = prettyId i ""
  pretty (TypedParam i t) = showParen True param "" where
    param = prettyId i . showString " " . showPretty T_colon .
            showString " " . showPretty t

-- TODO
instance Pretty Expr where
  prettyPrec d e =
    let mut_assign_prec = 4
        else_prec = 3
        then_prec = 2
        if_prec = 2
        semicolon_prec = 1
        let_prec = 0
        cond = not always
    in case e of
      IntCExpr i -> prettyIntC i
      FloatCExpr f -> prettyFloatC f
      CharCExpr c -> prettyCharC c
      StringCExpr s -> prettyStringC s
      TrueCExpr -> showPretty T_true
      FalseCExpr -> showPretty T_false
      UnitCExpr -> showPretty T_lparen . showPretty T_rparen
      -- BinOpExpr SemicolonOp u v -> showParen (always || d > semicolon_prec) $
      --   prettyPrec semicolon_prec u . showPretty T_semicolon . showString " " .
      --   prettyPrec (semicolon_prec + 1) v
      IfThenExpr u v -> showParen (always || d > if_prec) $
        showPretty T_if . showString " " . prettyPrec (if_prec + 1) u . showString " " .
        showPretty T_then . showString " " . prettyPrec then_prec v
      IfThenElseExpr u v w -> showParen (always || d > if_prec) $
        showPretty T_if . showString " " . prettyPrec (if_prec + 1) u . showString " " .
        showPretty T_then . showString " " .
        prettyPrec (then_prec + 1) v . showString " " .
        showPretty T_else . showString " " . prettyPrec (else_prec + 1) w

instance Pretty Pattern where
  prettyPrec d p = case p of
    IntConstPattern NoSign i -> prettyIntC i
    IntConstPattern Plus i -> showPretty T_plus . prettyIntC i
    IntConstPattern Minus i -> showPretty T_minus . prettyIntC i
    FloatConstPattern NoSign f -> prettyFloatC f
    FloatConstPattern Plus f -> showPretty T_plus_float . prettyFloatC f
    FloatConstPattern Minus f -> showPretty T_minus_float . prettyFloatC f
    CharConstPattern c -> prettyCharC c
    TruePattern -> showPretty T_true
    FalsePattern -> showPretty T_false
    IdPattern i -> prettyId i
    ConstrPattern i ps -> showParen (always || (d > prec && ps /= [])) $
      prettyConstrId i . prettyPrecSepList (prec + 1) " " ps where
        prec = 1
