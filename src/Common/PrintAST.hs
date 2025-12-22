module Common.PrintAST (Pretty,
                        pretty,
                        prettyPrec,
                        showPretty,
                        prettyAST,
                        debugPrint,
                        prettyPrecSepList) where

import Text.Pretty.Simple (CheckColorTty(CheckColorTty),
                          OutputOptions(outputOptionsIndentAmount, outputOptionsStringStyle),
                          StringOutputStyle (Literal),
                          defaultOutputOptionsDarkBg,
                          pPrintOpt)

import Common.AST
import Common.Token
     (FloatConstant,
      Identifier,
      ConstrIdentifier,
      CharConstant,
      IntConstant,
      StringConstant,
      Token(..))

-- Debug printing utils
debugPrint :: Show a => a -> IO ()
debugPrint = let smallIndent = defaultOutputOptionsDarkBg {outputOptionsIndentAmount = 2, outputOptionsStringStyle = Literal}
             in pPrintOpt CheckColorTty smallIndent


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
    IdT v          -> v
    IdConstrT v    -> v
    ConstIntT v    -> show v
    ConstFloatT v  -> show v
    ConstCharT v   -> '\'' : v : "\'"
    ConstStringT v -> "\"" ++ v ++ "\""
    keyword         -> show keyword

prettyId :: Identifier -> ShowS
prettyId = showPretty . IdT

prettyConstrId :: ConstrIdentifier -> ShowS
prettyConstrId = showPretty . IdConstrT

prettyIntC :: IntConstant -> ShowS
prettyIntC = showPretty . ConstIntT

prettyFloatC :: FloatConstant -> ShowS
prettyFloatC = showPretty . ConstFloatT

prettyCharC :: CharConstant -> ShowS
prettyCharC = showPretty . ConstCharT

prettyStringC :: StringConstant -> ShowS
prettyStringC = showPretty . ConstStringT

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
  pretty (TypeDef tDefs _) = showPretty TypeT . showString " " $
    prettySepList ("\n" ++ pretty AndT ++ "  ") tDefs

instance Pretty (TDef b) where
  pretty (TDef i constrs _) = prettyId i . showString " " .
    showPretty EqualsT . showString " " $
    prettySepList (" " ++ pretty BarT ++ " ") constrs

instance Pretty (Constr b) where
  pretty (Constr i [] _) = prettyConstrId i ""
  pretty (Constr i ts _) = prettyConstrId i . showString " " .
    showPretty OfT . showString " " $ prettySepList " " ts

instance Pretty (Type b) where
  prettyPrec d (Type t _) = prettyPrec d t

instance Pretty t => Pretty (TypeF t) where
  prettyPrec d tf =
    let
      ref_prec = 3
      array_prec = 2
      fun_prec = 1
      showsStars 1 = showPretty TimesT
      showsStars s = showsStars (s - 1) .
        showPretty CommaT . showString " " . showPretty TimesT
    in case tf of
      UnitType  -> showPretty UnitT
      IntType   -> showPretty IntT
      CharType  -> showPretty CharT
      BoolType  -> showPretty BoolT
      FloatType -> showPretty FloatT
      UserDefinedType i -> prettyId i
      RefType u -> showParen (always || d > ref_prec) $
            prettyPrec (ref_prec + 1) u .
            showString " " .
            showPretty RefT
      ArrayType 1 u -> showParen (always || d > array_prec) $
            showPretty ArrayT . showString " " .
            showPretty OfT . showString " " .
            prettyPrec (array_prec + 1) u
      ArrayType n u -> showParen (always || d > array_prec) $
            showPretty ArrayT . showString " " .
            showPretty LBracketT . showsStars n . showPretty RBracketT .
            showString " " . showPretty OfT . showString " " .
            prettyPrec (array_prec + 1) u
      FunType u v -> showParen (always || d > fun_prec) $
            prettyPrec (fun_prec + 1) u .
            showString " " . showPretty ArrowT . showString " " .
            prettyPrec fun_prec v

instance Pretty (LetDef b) where
  pretty (Let defs _) = showPretty LetT . showString " " $
    prettySepList ("\n" ++ pretty AndT ++ " ") defs
  pretty (LetRec defs _) = showPretty LetT . showString " " .
    showPretty RecT . showString " " $
    prettySepList ("\n" ++ pretty AndT ++ " ") defs

instance Pretty (Def b) where
  pretty def = case def of
    FunDef i ps mt e _ ->
      prettyId i . showString sep . prettyPrecSepList 0 " " ps . showString " " .
      maybe id (\t -> showPretty ColonT . showString " " . showPretty t . showString " ") mt .
      showPretty EqualsT . showString " " $ pretty e where
        sep = if null ps then "" else " "
    VarDef i mt _ ->
      showPretty MutableT . showString " " . prettyId i $
      maybe "" (\t -> showString " " . showPretty ColonT . showString " " $ showPretty t "") mt
    ArrayDef i es mt _ ->
      showPretty MutableT . showString " " . prettyId i .
      showString " " . showPretty LBracketT . prettyPrecSepList 0 ", " es . showPretty RBracketT $
      maybe "" (\t -> showString " " . showPretty ColonT . showString " " $ showPretty t "") mt

instance Pretty (Param b) where
  pretty (Param i _) = prettyId i ""
  pretty (TypedParam i t _) = showParen True param "" where
    param = prettyId i . showString " " . showPretty ColonT .
            showString " " . showPretty t

instance Pretty (Expr b) where
  prettyPrec d expr =
    let new_prec = 15
        let_prec = 0
    in case expr of
      Expr e _ -> prettyPrec d e
      NewType t _ -> showParen (always || d > new_prec) $
        showPretty NewT . showString " " .
        showPretty t
      LetIn def u _ -> showParen (always || d > let_prec) $
        prettyPrec d def . showString " " . showPretty InT .
        showString " " . prettyPrec (let_prec + 1) u
      MatchExpr u cs _ -> showPretty MatchT . showString " " .
        showPretty u . showString " " . showPretty WithT . showString "\n" .
        prettyPrecSepList 0 ("\n" ++ pretty BarT ++ " ") cs . showString "\n" .
        showPretty EndT

instance Pretty e => Pretty (ExprF e) where
  prettyPrec d e =
    let array_access_prec = 14
        bang_prec = 13
        app_prec = 12
        un_op_prec = 11
        else_prec = 3
        then_prec = 2
        if_prec = 2
    in case e of
      IntCExpr i -> prettyIntC i
      FloatCExpr f -> prettyFloatC f
      CharCExpr c -> prettyCharC c
      StringCExpr s -> prettyStringC s
      TrueCExpr -> showPretty TrueT
      FalseCExpr -> showPretty FalseT
      UnitCExpr -> showPretty LParenT . showPretty RParenT
      ConstExpr i -> prettyId i
      ConstConstrExpr i -> prettyConstrId i
      ArrayDim i 1 -> showParen (always || d > un_op_prec) $
        showPretty DimT . showString " " . prettyId i
      ArrayDim i n -> showParen (always || d > un_op_prec) $
        showPretty DimT . showString " " . prettyIntC n .
        showString " " . prettyId i
      ArrayAccess i es -> showParen (always || d > array_access_prec) $
        prettyId i . showPretty LBracketT .
        prettyPrecSepList 0 ", " es .
        showPretty RBracketT
      FunAppExpr i ps -> showParen (always || (d > app_prec && not (null ps))) $
        prettyId i . showString sep .
        prettyPrecSepList (app_prec + 1) " " ps where
          sep = if null ps then "" else " "
      ConstrAppExpr i ps -> showParen (always || (d > app_prec && not (null ps))) $
        prettyConstrId i . showString sep .
        prettyPrecSepList (app_prec + 1) " " ps where
          sep = if null ps then "" else " "
      UnOpExpr BangOp u -> showParen (always || d > bang_prec) $
        showPretty BangT . prettyPrec (bang_prec + 1) u
      UnOpExpr NotOp u -> showParen (always || d > un_op_prec) $
        showPretty NotT . showString " " . prettyPrec (un_op_prec + 1) u
      UnOpExpr op u -> showParen (always || d > un_op_prec) $
        showPretty (opToTok op) . prettyPrec (un_op_prec + 1) u
      DeleteExpr u -> showParen (always || d > un_op_prec) $
        showPretty DeleteT . showString " " . prettyPrec (un_op_prec + 1) u
      BinOpExpr op u w ->
        let (assoc, p, tok) = binOpPrec op
        in prettyBinOpExp assoc p tok d u w
      IfThenExpr u v -> showParen (always || d > if_prec) $
        showPretty IfT . showString " " . prettyPrec (if_prec + 1) u . showString " " .
        showPretty ThenT . showString " " . prettyPrec then_prec v
      IfThenElseExpr u v w -> showParen (always || d > if_prec) $
        showPretty IfT . showString " " . prettyPrec (if_prec + 1) u . showString " " .
        showPretty ThenT . showString " " .
        prettyPrec (then_prec + 1) v . showString " " .
        showPretty ElseT . showString " " . prettyPrec (else_prec + 1) w
      BeginExpr u -> showParen always $
        showPretty BeginT . showString " " . showPretty u .
        showString " " . showPretty EndT
      WhileExpr u v -> showParen always $
        showPretty WhileT . showString " " .
        showPretty u . showString " " . showPretty DoT .
        showString " " . showPretty v . showString " " . showPretty DoneT
      ForExpr i u v w -> showParen always $
        showPretty ForT . showString " " . prettyId i .
        showString " " . showPretty EqualsT . showString " " .
        showPretty u . showString " " . showPretty ToT . showString " " .
        showPretty v . showString " " . showPretty DoT . showString " " .
        showPretty w . showString " " . showPretty DoneT
      ForDownExpr i u v w -> showParen always $
        showPretty ForT . showString " " . prettyId i .
        showString " " . showPretty EqualsT . showString " " .
        showPretty u . showString " " . showPretty DowntoT . showString " " .
        showPretty v . showString " " . showPretty DoT . showString " " .
        showPretty w . showString " " . showPretty DoneT

data Assoc = L | R | Non
  deriving Eq

prettyBinOpExp :: Pretty e => Assoc -> Int -> Token -> Int -> e -> e -> ShowS
prettyBinOpExp L = prettyBinOpExpL
prettyBinOpExp R = prettyBinOpExpR
prettyBinOpExp Non = prettyBinOpExpNon

prettyBinOpExpL :: Pretty e => Int -> Token -> Int -> e -> e -> ShowS
prettyBinOpExpL prec tok d u w = showParen (always || d > prec) $
  prettyPrec prec u . showString " " .
  showPretty tok . showString " " .
  prettyPrec (prec + 1) w

prettyBinOpExpR :: Pretty e => Int -> Token -> Int -> e -> e -> ShowS
prettyBinOpExpR prec tok d u w = showParen (always || d > prec) $
  prettyPrec (prec + 1) u . showString " " .
  showPretty tok . showString " " .
  prettyPrec prec w

prettyBinOpExpNon :: Pretty e => Int -> Token -> Int -> e -> e -> ShowS
prettyBinOpExpNon prec tok d u w = showParen (always || d > prec) $
  prettyPrec (prec + 1) u . showString " " .
  showPretty tok . showString " " .
  prettyPrec (prec + 1) w

opToTok :: UnOp -> Token
opToTok PlusUnOp = PlusT
opToTok MinusUnOp = MinusT
opToTok PlusFloatUnOp = PlusFloatT
opToTok MinusFloatUnOp = MinusFloatT
opToTok BangOp = BangT
opToTok NotOp = NotT

instance Pretty UnOp where
  pretty = pretty . opToTok

binOpPrec :: BinOp -> (Assoc, Int, Token)
binOpPrec ExpOp = (R, 10, ExpT)
binOpPrec TimesOp = (L, 9, TimesT)
binOpPrec DivOp = (L, 9, DivT)
binOpPrec TimesFloatOp = (L, 9, TimesFloatT)
binOpPrec DivFloatOp = (L, 9, DivFloatT)
binOpPrec ModOp = (L, 9, ModT)
binOpPrec PlusOp = (L, 8, PlusT)
binOpPrec MinusOp = (L, 8, MinusT)
binOpPrec PlusFloatOp = (L, 8, PlusFloatT)
binOpPrec MinusFloatOp = (L, 8, MinusFloatT)
binOpPrec EqOp = (Non, 7, EqualsT)
binOpPrec NotEqOp = (Non, 7, NotEqualsT)
binOpPrec GTOp = (Non, 7, GreaterThanT)
binOpPrec LTOp = (Non, 7, LessThanT)
binOpPrec GEqOp = (Non, 7, GreaterThanEqT)
binOpPrec LEqOp = (Non, 7, LessThanEqT)
binOpPrec NatEqOp = (Non, 7, NatEqOpT)
binOpPrec NotNatEqOp = (Non, 7, NotNatEqOpT)
binOpPrec AndOp = (L, 6, AndOpT)
binOpPrec OrOp = (L, 5, OrOpT)
binOpPrec AssignMutableOp = (Non, 4, AssignMutableT)
binOpPrec SemicolonOp = (L, 1, SemicolonT)

instance Pretty BinOp where
  pretty = pretty . (\(_, _, t) -> t) . binOpPrec

instance Pretty (Clause b) where
  pretty (Match p e _) = showPretty p . showString " " . showPretty ArrowT .
    showString " " . showPretty e $ ""

instance Pretty (Pattern b) where
  prettyPrec d (Pattern p _) = prettyPrec d p

instance Pretty p => Pretty (PatternF p) where
  prettyPrec d pf = case pf of
    IntConstPattern NoSign i -> prettyIntC i
    IntConstPattern Plus i -> showPretty PlusT . prettyIntC i
    IntConstPattern Minus i -> showPretty MinusT . prettyIntC i
    FloatConstPattern NoSign f -> prettyFloatC f
    FloatConstPattern Plus f -> showPretty PlusFloatT . prettyFloatC f
    FloatConstPattern Minus f -> showPretty MinusFloatT . prettyFloatC f
    CharConstPattern c -> prettyCharC c
    TruePattern -> showPretty TrueT
    FalsePattern -> showPretty FalseT
    IdPattern i -> prettyId i
    ConstrPattern i ps -> showParen (always || (d > prec && not (null ps))) $
      prettyConstrId i . showString sep . prettyPrecSepList (prec + 1) " " ps where
        sep = if null ps then "" else " "
        prec = 1
