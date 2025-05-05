module Common.PrintAST (Pretty,
                        pretty,
                        prettyPrec,
                        showPretty,
                        prettyAST) where

import Common.AST
import Common.Token
     (FloatConstant,
      Identifier,
      ConstrIdentifier,
      CharConstant,
      IntConstant,
      StringConstant,
      Token(..))

-- Pretty printing utils

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
prettySepList _ [x]    = pretty x
prettySepList s (x:xs) = showPretty x . showString s $ prettySepList s xs

prettyPrecSepList :: Pretty a => Int -> String -> [a] -> ShowS
prettyPrecSepList _ _ []     = id
prettyPrecSepList d _ [x]    = prettyPrec d x
prettyPrecSepList d s (x:xs) = prettyPrec d x . showString s . prettyPrecSepList d s xs

-- False omits parentheses whenever possible
always :: Bool
always = False

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyPrec d (Left a)  = prettyPrec d a
  prettyPrec d (Right b) = prettyPrec d b

prettyAST :: AST b -> String
prettyAST = prettySepList "\n\n"

instance Pretty (TypeDef b) where
  pretty (TypeDef tDefs _) = showPretty T_type . showString " " $
    prettySepList ("\n" ++ pretty T_and ++ "  ") tDefs

instance Pretty (TDef b) where
  pretty (TDef ide constrs _) = prettyId ide . showString " " .
    showPretty T_equals . showString " " $
    prettySepList (" " ++ pretty T_bar ++ " ") constrs

instance Pretty (Constr b) where
  pretty (Constr ide [] _) = prettyConstrId ide ""
  pretty (Constr ide ts _) = prettyConstrId ide . showString " " .
    showPretty T_of . showString " " $ prettySepList " " ts

instance Pretty (Type b) where
  prettyPrec d (Type t _) = prettyPrec d t

instance Pretty t => Pretty (TypeF t) where
  prettyPrec d tf =
    let
      abs_prec = 4
      ref_prec = 3
      array_prec = 2
      fun_prec = 1
      showsStars 1 = showPretty T_times
      showsStars s = showsStars (s - 1) .
        showPretty T_comma . showString " " . showPretty T_times
    in case tf of
      UnitType -> showPretty T_unit
      IntType -> showPretty T_int
      CharType -> showPretty T_char
      BoolType -> showPretty T_bool
      FloatType -> showPretty T_float
      UserDefinedType ide -> prettyId ide
      RefType u -> showParen (always || d > ref_prec) $
            prettyPrec (ref_prec + 1) u .
            showString " " .
            showPretty T_ref
      ArrayType 1 u -> showParen (always || d > array_prec) $
            showPretty T_array . showString " " .
            showPretty T_of . showString " " .
            prettyPrec (array_prec + 1) u
      ArrayType n u -> showParen (always || d > array_prec) $
            showPretty T_array . showString " " .
            showPretty T_lbracket . showsStars n . showPretty T_rbracket .
            showString " " . showPretty T_of . showString " " .
            prettyPrec (array_prec + 1) u
      FunType u v -> showParen (always || d > fun_prec) $
            prettyPrec (fun_prec + 1) u .
            showString " " . showPretty T_arrow . showString " " .
            prettyPrec fun_prec v
      VarType i -> showString $ "@" ++ show i
      AbsType v u -> showParen (always || d > abs_prec) $
            showString ("forall @" ++ show v ++ ". ") .
            prettyPrec (abs_prec + 1) u

instance Pretty (LetDef b) where
  pretty (Let defs _) = showPretty T_let . showString " " $
    prettySepList ("\n" ++ pretty T_and ++ " ") defs
  pretty (LetRec defs _) = showPretty T_let . showString " " .
    showPretty T_rec . showString " " $
    prettySepList ("\n" ++ pretty T_and ++ " ") defs

instance Pretty (Def b) where
  pretty def = case def of
    FunDef i ps e _ -> prettyId i . showString sep . prettyPrecSepList 0 " " ps .
      showString " " . showPretty T_equals . showString " " $ pretty e where
        sep = if null ps then "" else " "
    FunDefTyped i ps t e _ -> prettyId i . showString sep . prettyPrecSepList 0 " " ps .
      showString " " . showPretty T_colon . showString " " . showPretty t .
      showString " " . showPretty T_equals . showString " " $ pretty e where
        sep = if null ps then "" else " "
    VarDef i _ -> showPretty T_mutable . showString " " $ prettyId i ""
    VarDefTyped i t _ -> showPretty T_mutable . showString " " . prettyId i .
      showString " " . showPretty T_colon . showString " " $ showPretty t ""
    ArrayDef i es _ -> showPretty T_mutable . showString " " . prettyId i .
      showString " " . showPretty T_lbracket . prettyPrecSepList 0 ", " es $
      showPretty T_rbracket ""
    ArrayDefTyped i es t _ -> showPretty T_mutable . showString " " . prettyId i .
      showString " " . showPretty T_lbracket . prettyPrecSepList 0 ", " es .
      showPretty T_rbracket . showString " " . showPretty T_colon .
      showString " " $ showPretty t ""

instance Pretty (Param b) where
  pretty (Param i _) = prettyId i ""
  pretty (TypedParam i t _) = showParen True param "" where
    param = prettyId i . showString " " . showPretty T_colon .
            showString " " . showPretty t

instance Pretty (Expr b) where
  prettyPrec d (Expr e _) =
    let new_prec = 15
        array_access_prec = 14
        bang_prec = 13
        app_prec = 12
        un_op_prec = 11
        else_prec = 3
        then_prec = 2
        if_prec = 2
        let_prec = 0
    in case e of
      IntCExpr i -> prettyIntC i
      FloatCExpr f -> prettyFloatC f
      CharCExpr c -> prettyCharC c
      StringCExpr s -> prettyStringC s
      TrueCExpr -> showPretty T_true
      FalseCExpr -> showPretty T_false
      UnitCExpr -> showPretty T_lparen . showPretty T_rparen
      ArrayDim i 1 -> showParen (always || d > un_op_prec) $
        showPretty T_dim . showString " " . prettyId i
      ArrayDim i n -> showParen (always || d > un_op_prec) $
        showPretty T_dim . showString " " . prettyIntC n .
        showString " " . prettyId i
      NewType t -> showParen (always || d > new_prec) $
        showPretty T_new . showString " " .
        showPretty t
      ArrayAccess i es -> showParen (always || d > array_access_prec) $
        prettyId i . showPretty T_lbracket .
        prettyPrecSepList 0 ", " es .
        showPretty T_rbracket
      FunAppExpr i ps -> showParen (always || (d > app_prec && not (null ps))) $
        prettyId i . showString sep .
        prettyPrecSepList (app_prec + 1) " " ps where
          sep = if null ps then "" else " "
      ConstrAppExpr i ps -> showParen (always || (d > app_prec && not (null ps))) $
        prettyConstrId i . showString sep .
        prettyPrecSepList (app_prec + 1) " " ps where
          sep = if null ps then "" else " "
      UnOpExpr BangOp u -> showParen (always || d > bang_prec) $
        showPretty T_bang . prettyPrec (bang_prec + 1) u
      UnOpExpr NotOp u -> showParen (always || d > un_op_prec) $
        showPretty T_not . showString " " . prettyPrec (un_op_prec + 1) u
      UnOpExpr op u -> showParen (always || d > un_op_prec) $
        showPretty (opToTok op) . prettyPrec (un_op_prec + 1) u
      DeleteExpr u -> showParen (always || d > un_op_prec) $
        showPretty T_delete . showString " " . prettyPrec (un_op_prec + 1) u
      BinOpExpr op u w ->
        let (assoc, p, tok) = binOpPrec op
        in prettyBinOpExp assoc p tok d u w
      IfThenExpr u v -> showParen (always || d > if_prec) $
        showPretty T_if . showString " " . prettyPrec (if_prec + 1) u . showString " " .
        showPretty T_then . showString " " . prettyPrec then_prec v
      IfThenElseExpr u v w -> showParen (always || d > if_prec) $
        showPretty T_if . showString " " . prettyPrec (if_prec + 1) u . showString " " .
        showPretty T_then . showString " " .
        prettyPrec (then_prec + 1) v . showString " " .
        showPretty T_else . showString " " . prettyPrec (else_prec + 1) w
      LetIn def u -> showParen (always || d > let_prec) $
        prettyPrec d def . showString " " . showPretty T_in .
        showString " " . prettyPrec (let_prec + 1) u
      BeginExpr u -> showParen always $
        showPretty T_begin . showString " " . showPretty u .
        showString " " . showPretty T_end
      WhileExpr u v -> showParen always $
        showPretty T_while . showString " " .
        showPretty u . showString " " . showPretty T_do .
        showString " " . showPretty v . showString " " . showPretty T_done
      ForExpr i u v w -> showParen always $
        showPretty T_for . showString " " . prettyId i .
        showString " " . showPretty T_equals . showString " " .
        showPretty u . showString " " . showPretty T_to . showString " " .
        showPretty v . showString " " . showPretty T_do . showString " " .
        showPretty w . showString " " . showPretty T_done
      ForDownExpr i u v w -> showParen always $
        showPretty T_for . showString " " . prettyId i .
        showString " " . showPretty T_equals . showString " " .
        showPretty u . showString " " . showPretty T_downto . showString " " .
        showPretty v . showString " " . showPretty T_do . showString " " .
        showPretty w . showString " " . showPretty T_done
      MatchExpr u cs -> showPretty T_match . showString " " .
        showPretty u . showString " " . showPretty T_with . showString "\n" .
        prettyPrecSepList 0 ("\n" ++ pretty T_bar ++ " ") cs . showString "\n" .
        showPretty T_end

data Assoc = L | R | Non
  deriving Eq

prettyBinOpExp :: Assoc -> Int -> Token -> Int -> Expr b -> Expr b -> ShowS
prettyBinOpExp L = prettyBinOpExpL
prettyBinOpExp R = prettyBinOpExpR
prettyBinOpExp Non = prettyBinOpExpNon

prettyBinOpExpL :: Int -> Token -> Int -> Expr b -> Expr b -> ShowS
prettyBinOpExpL prec tok d u w = showParen (always || d > prec) $
  prettyPrec prec u . showString " " .
  showPretty tok . showString " " .
  prettyPrec (prec + 1) w

prettyBinOpExpR :: Int -> Token -> Int -> Expr b -> Expr b -> ShowS
prettyBinOpExpR prec tok d u w = showParen (always || d > prec) $
  prettyPrec (prec + 1) u . showString " " .
  showPretty tok . showString " " .
  prettyPrec prec w

prettyBinOpExpNon :: Int -> Token -> Int -> Expr b -> Expr b -> ShowS
prettyBinOpExpNon prec tok d u w = showParen (always || d > prec) $
  prettyPrec (prec + 1) u . showString " " .
  showPretty tok . showString " " .
  prettyPrec (prec + 1) w

opToTok :: UnOp -> Token
opToTok PlusUnOp = T_plus
opToTok MinusUnOp = T_minus
opToTok PlusFloatUnOp = T_plus_float
opToTok MinusFloatUnOp = T_minus_float
opToTok BangOp = T_bang
opToTok NotOp = T_not

instance Pretty UnOp where
  pretty = pretty . opToTok

binOpPrec :: BinOp -> (Assoc, Int, Token)
binOpPrec ExpOp = (R, 10, T_exp)
binOpPrec TimesOp = (L, 9, T_times)
binOpPrec DivOp = (L, 9, T_div)
binOpPrec TimesFloatOp = (L, 9, T_times_float)
binOpPrec DivFloatOp = (L, 9, T_div_float)
binOpPrec ModOp = (L, 9, T_mod)
binOpPrec PlusOp = (L, 8, T_plus)
binOpPrec MinusOp = (L, 8, T_minus)
binOpPrec PlusFloatOp = (L, 8, T_plus_float)
binOpPrec MinusFloatOp = (L, 8, T_minus_float)
binOpPrec EqOp = (Non, 7, T_equals)
binOpPrec NotEqOp = (Non, 7, T_not_equals)
binOpPrec GTOp = (Non, 7, T_greater_than)
binOpPrec LTOp = (Non, 7, T_less_than)
binOpPrec GEqOp = (Non, 7, T_greater_than_eq)
binOpPrec LEqOp = (Non, 7, T_less_than_eq)
binOpPrec NatEqOp = (Non, 7, T_nat_eq_op)
binOpPrec NotNatEqOp = (Non, 7, T_not_nat_eq_op)
binOpPrec AndOp = (L, 6, T_and_op)
binOpPrec OrOp = (L, 5, T_or_op)
binOpPrec AssignMutableOp = (Non, 4, T_assign_mutable)
binOpPrec SemicolonOp = (L, 1, T_semicolon)

instance Pretty (Clause b) where
  pretty (Match p e _) = showPretty p . showString " " . showPretty T_arrow .
    showString " " . showPretty e $ ""

instance Pretty (Pattern b) where
  prettyPrec d (Pattern p _) = case p of
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
    ConstrPattern i ps -> showParen (always || (d > prec && not (null ps))) $
      prettyConstrId i . showString sep . prettyPrecSepList (prec + 1) " " ps where
        sep = if null ps then "" else " "
        prec = 1
