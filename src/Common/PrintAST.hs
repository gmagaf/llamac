{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Common.PrintAST (Pretty,
                        pretty,
                        prettyP,
                        prettyPrec,
                        prettyPPrec,
                        showPretty,
                        prettyPSepList,
                        prettyPPrecSepList) where

import Common.AST
import Common.Token
     (FloatConstant,
      Identifier,
      ConstrIdentifier,
      CharConstant,
      IntConstant,
      StringConstant,
      Token(..),
      lexeme)

-- Pretty printing utils

class Pretty a where
  -- False omits parentheses whenever possible
  prettyPPrec :: Bool -> Int -> a -> ShowS
  prettyPPrec p _ a s = prettyP p a ++ s
  prettyPrec :: Int -> a -> ShowS
  prettyPrec = prettyPPrec False
  prettyP :: Bool -> a -> String
  prettyP p a = prettyPPrec p 0 a ""
  pretty :: a -> String
  pretty = prettyP False
  showPretty :: a -> ShowS
  showPretty = showString . pretty

-- Pretty printing tokens

instance Pretty Token where
  pretty = lexeme

prettyId :: Identifier -> ShowS
prettyId = showPretty . IdT

instance Pretty Identifier where
  showPretty = prettyId

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

-- Utils for pretty printing lists

prettyPSepList :: Pretty a => Bool -> String -> [a] -> ShowS
prettyPSepList _ _ []          = id
prettyPSepList always _ [x]    = prettyPPrec always 0 x
prettyPSepList always s (x:xs) = prettyPPrec always 0 x . showString s . prettyPSepList always s xs

prettyPPrecSepList :: Pretty a => Bool -> Int -> String -> [a] -> ShowS
prettyPPrecSepList _ _ _ []          = id
prettyPPrecSepList always d _ [x]    = prettyPPrec always d x
prettyPPrecSepList always d s (x:xs) = prettyPPrec always d x . showString s . prettyPPrecSepList always d s xs

-- Pretty printing ast

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyPPrec always d (Left a)  = prettyPPrec always d a
  prettyPPrec always d (Right b) = prettyPPrec always d b

instance Pretty (AST b) where
  prettyPPrec always _ (AST ast _) = prettyPSepList always "\n\n" ast

instance Pretty (TypeDef b) where
  prettyPPrec always _ (TypeDef tDefs _) = showPretty TypeT . showString " " .
    prettyPSepList always ("\n" ++ pretty AndT ++ "  ") tDefs

instance Pretty (TDef b) where
  prettyPPrec always _ (TDef i constrs _) = prettyId i . showString " " .
    showPretty EqualsT . showString " " .
    prettyPSepList always (" " ++ pretty BarT ++ " ") constrs

instance Pretty (Constr b) where
  prettyPPrec _      _ (Constr i [] _) = prettyConstrId i
  prettyPPrec always _ (Constr i ts _) = prettyConstrId i . showString " " .
    showPretty OfT . showString " " . prettyPSepList always " " ts

instance Pretty (Type b) where
  prettyPPrec always d (Type t _) = prettyPPrec always d t

instance (Pretty i, Pretty t) => Pretty (TypeF i t) where
  prettyPPrec always d tf =
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
      UserDefinedType i -> showPretty i
      RefType u -> showParen (always || d > ref_prec) $
            prettyPPrec always (ref_prec + 1) u .
            showString " " .
            showPretty RefT
      ArrayType 1 u -> showParen (always || d > array_prec) $
            showPretty ArrayT . showString " " .
            showPretty OfT . showString " " .
            prettyPPrec always (array_prec + 1) u
      ArrayType n u -> showParen (always || d > array_prec) $
            showPretty ArrayT . showString " " .
            showPretty LBracketT . showsStars n . showPretty RBracketT .
            showString " " . showPretty OfT . showString " " .
            prettyPPrec always (array_prec + 1) u
      FunType u v -> showParen (always || d > fun_prec) $
            prettyPPrec always (fun_prec + 1) u .
            showString " " . showPretty ArrowT . showString " " .
            prettyPPrec always fun_prec v

instance Pretty (LetDef b) where
  prettyPPrec always _ (Let defs _)    = showPretty LetT . showString " " .
    prettyPSepList always ("\n" ++ pretty AndT ++ " ") defs
  prettyPPrec always _ (LetRec defs _) = showPretty LetT . showString " " .
    showPretty RecT . showString " " .
    prettyPSepList always ("\n" ++ pretty AndT ++ " ") defs

instance Pretty (Def b) where
  prettyPPrec always _ def = case def of
    FunDef i ps mt e _ ->
      prettyId i . showString sep . prettyPSepList always " " ps . showString " " .
      maybe id (\t -> showPretty ColonT . showString " " . prettyPPrec always 0 t . showString " ") mt .
      showPretty EqualsT . showString " " . prettyPPrec always 0 e where
        sep = if null ps then "" else " "
    VarDef i mt _ ->
      showPretty MutableT . showString " " . prettyId i .
      maybe id (\t -> showString " " . showPretty ColonT . showString " " . prettyPPrec always 0 t) mt
    ArrayDef i es mt _ ->
      showPretty MutableT . showString " " . prettyId i .
      showString " " . showPretty LBracketT . prettyPPrecSepList always 0 ", " es . showPretty RBracketT .
      maybe id (\t -> showString " " . showPretty ColonT . showString " " . prettyPPrec always 0 t) mt

instance Pretty (Param b) where
  prettyPPrec _ _ (Param i _)             = prettyId i
  prettyPPrec always _ (TypedParam i t _) = showParen True param where
    param = prettyId i . showString " " . showPretty ColonT .
            showString " " . prettyPPrec always 0 t

-- Pretty printing expressions
-- This is a bit more complicated

-- Precedence of operators
new_prec, array_access_prec, bang_prec, app_prec, un_op_prec :: Int
exp_prec, mult_prec, add_prec, comp_prec, and_prec, or_prec :: Int
assign_prec, if_then_else_prec, else_prec, if_then_prec, semicolon_prec, let_prec :: Int
new_prec          = 15
array_access_prec = 14
bang_prec         = 13
app_prec          = 12
un_op_prec        = 11
exp_prec          = 10
mult_prec         = 9
add_prec          = 8
comp_prec         = 7
and_prec          = 6
or_prec           = 5
assign_prec       = 4
if_then_else_prec = 3
else_prec         = 2
if_then_prec      = 2
semicolon_prec    = 1
let_prec          = 0

data ParenInfo = ParenInfo
              { isAlways :: Bool
              , isRightMostLet :: Bool
              , isRightMostIf :: Bool
              , elseFollows :: Bool
              }

prettyPPrecSepListExpr :: ParenInfo -> Int -> String -> [Expr a] -> ShowS
prettyPPrecSepListExpr _ _ _ []         = id
prettyPPrecSepListExpr pInfo d _ [x]    = prettyPPrecExpr pInfo d x
prettyPPrecSepListExpr pInfo d s (x:xs) = prettyPPrecExpr pInfo d x .
  showString s . prettyPPrecSepListExpr pInfo d s xs

showParenOp :: ParenInfo -> Int -> Int -> Bool
showParenOp pInfo d prec = isAlways pInfo || d > prec

showParenLet :: ParenInfo -> Int -> Bool
showParenLet pInfo d = isAlways pInfo || (not (isRightMostLet pInfo) && d > let_prec)

showParenIfThenElse :: ParenInfo -> Int -> Bool
showParenIfThenElse pInfo d = isAlways pInfo || (not (isRightMostIf pInfo) && d > if_then_else_prec)

showParenIfThen :: ParenInfo -> Int -> Bool
showParenIfThen pInfo d = always || left || elseF where
  always = isAlways pInfo
  left = not (isRightMostIf pInfo) && d > if_then_prec
  elseF = isRightMostIf pInfo && elseFollows pInfo

instance Pretty (Expr b) where
  prettyPPrec always = prettyPPrecExpr
    ParenInfo { isAlways = always
              , isRightMostLet = True
              , isRightMostIf = True
              , elseFollows = False
              }

prettyPPrecExpr :: ParenInfo -> Int -> Expr b -> ShowS
prettyPPrecExpr pInfo d expr =
    let always = isAlways pInfo
    in case expr of
      Expr e _         -> prettyPPrecExprF pInfo d e
      NewType t _      -> showParen (showParenOp pInfo d new_prec) $
        showPretty NewT . showString " " .
        prettyPPrec always 0 t
      LetIn def u _    -> showParen paren $
        prettyPPrec always d def . showString " " . showPretty InT .
        showString " " . prettyPPrecExpr pInfo' let_prec u where
          paren = showParenLet pInfo d
          pInfo' = pInfo{ isRightMostLet = True
                        , isRightMostIf = True
                        , elseFollows = elseFollows pInfo && not paren }
      MatchExpr u cs _ -> showPretty MatchT . showString " " .
        prettyPPrec always 0 u . showString " " . showPretty WithT . showString "\n" .
        prettyPPrecSepList (isAlways pInfo) 0 ("\n" ++ pretty BarT ++ " ") cs . showString "\n" .
        showPretty EndT

instance Pretty (ExprF (Expr b)) where
  prettyPPrec always = prettyPPrecExprF
    ParenInfo { isAlways = always
              , isRightMostLet = True
              , isRightMostIf = True
              , elseFollows = False }

prettyPPrecExprF :: ParenInfo -> Int -> ExprF (Expr b) -> ShowS
prettyPPrecExprF pInfo d e = let
    always = isAlways pInfo
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
      ArrayDim i 1 -> showParen (showParenOp pInfo d un_op_prec) $
        showPretty DimT . showString " " . prettyId i
      ArrayDim i n -> showParen (showParenOp pInfo d un_op_prec) $
        showPretty DimT . showString " " . prettyIntC n .
        showString " " . prettyId i
      ArrayAccess i es -> showParen (showParenOp pInfo d array_access_prec) $
        prettyId i . showPretty LBracketT .
        prettyPPrecSepList always 0 ", " es .
        showPretty RBracketT
      FunAppExpr i ps -> showParen (always || (d > app_prec && not (null ps))) $
        prettyId i . showString sep .
        prettyPPrecSepListExpr pInfo' (app_prec + 1) " " ps where
          sep = if null ps then "" else " "
          pInfo' = pInfo{ isRightMostLet = False, isRightMostIf = False }
      ConstrAppExpr i ps -> showParen (always || (d > app_prec && not (null ps))) $
        prettyConstrId i . showString sep .
        prettyPPrecSepListExpr pInfo' (app_prec + 1) " " ps where
          sep = if null ps then "" else " "
          pInfo' = pInfo{ isRightMostLet = False, isRightMostIf = False }
      UnOpExpr op u ->
        let (p, tok) = opToTok op
        in prettyUnOpExp p tok pInfo d u
      DeleteExpr u -> showParen parens $
        showPretty DeleteT . showString " " . prettyPPrecExpr pInfo' (un_op_prec + 1) u where
          parens = showParenOp pInfo d un_op_prec
          pInfo' = pInfo{ isRightMostLet = isRightMostLet pInfo || parens
                        , isRightMostIf = isRightMostIf pInfo || parens
                        , elseFollows = not parens && elseFollows pInfo }
      BinOpExpr op u w ->
        let (assoc, p, tok) = binOpPrec op
        in prettyBinOpExp assoc p tok pInfo d u w
      IfThenExpr u v -> showParen parens $
        showPretty IfT . showString " " . prettyPPrec always 0 u . showString " " .
        showPretty ThenT . showString " " . prettyPPrecExpr pInfoThen if_then_prec v where
          parens = showParenIfThen pInfo d
          pInfoThen = pInfo{ isRightMostLet = isRightMostLet pInfo || parens
                           , isRightMostIf = isRightMostIf pInfo || parens
                           , elseFollows = not parens && elseFollows pInfo
                           }
      IfThenElseExpr u v w -> showParen parens $
        showPretty IfT . showString " " . prettyPPrec always 0 u . showString " " .
        showPretty ThenT . showString " " .
        prettyPPrecExpr pInfoThen if_then_else_prec v . showString " " .
        showPretty ElseT . showString " " . prettyPPrecExpr pInfoElse else_prec w where
          parens = showParenIfThenElse pInfo d
          pInfoThen = pInfo{ isRightMostLet = True, isRightMostIf = True, elseFollows = True }
          pInfoElse = pInfo{ isRightMostLet = isRightMostLet pInfo || parens
                           , isRightMostIf = isRightMostIf pInfo || parens
                           , elseFollows = not parens && elseFollows pInfo
                           }
      BeginExpr u -> showParen always $
        showPretty BeginT . showString " " . prettyPPrec always 0 u .
        showString " " . showPretty EndT
      WhileExpr u v -> showParen always $
        showPretty WhileT . showString " " .
        prettyPPrec always 0 u . showString " " . showPretty DoT .
        showString " " . prettyPPrec always 0 v . showString " " . showPretty DoneT
      ForExpr i u v w -> showParen always $
        showPretty ForT . showString " " . prettyId i .
        showString " " . showPretty EqualsT . showString " " .
        prettyPPrec always 0 u . showString " " . showPretty ToT . showString " " .
        prettyPPrec always 0 v . showString " " . showPretty DoT . showString " " .
        prettyPPrec always 0 w . showString " " . showPretty DoneT
      ForDownExpr i u v w -> showParen always $
        showPretty ForT . showString " " . prettyId i .
        showString " " . showPretty EqualsT . showString " " .
        prettyPPrec always 0 u . showString " " . showPretty DowntoT . showString " " .
        prettyPPrec always 0 v . showString " " . showPretty DoT . showString " " .
        prettyPPrec always 0 w . showString " " . showPretty DoneT

opToTok :: UnOp -> (Int, Token)
opToTok op = case op of
  PlusUnOp ->       (un_op_prec, PlusT)
  MinusUnOp ->      (un_op_prec, MinusT)
  PlusFloatUnOp ->  (un_op_prec, PlusFloatT)
  MinusFloatUnOp -> (un_op_prec, MinusFloatT)
  BangOp ->         (bang_prec, BangT)
  NotOp ->          (un_op_prec, NotT)

instance Pretty UnOp where
  pretty = pretty . snd . opToTok

prettyUnOpExp :: Int -> Token -> ParenInfo -> Int -> Expr b -> ShowS
prettyUnOpExp prec tok pInfo d u =
  let parens = showParenOp pInfo d prec
      pInfo' = pInfo{ isRightMostLet = isRightMostLet pInfo || parens
                    , isRightMostIf = isRightMostIf pInfo || parens
                    , elseFollows = not parens && elseFollows pInfo }
      sep = if tok == NotT then " " else ""
  in showParen parens $
     showPretty tok . showString sep . prettyPPrecExpr pInfo' (prec + 1) u

data Assoc = L | R | Non
  deriving Eq

binOpPrec :: BinOp -> (Assoc, Int, Token)
binOpPrec op = case op of
  ExpOp ->            (R, exp_prec, ExpT)
  TimesOp ->          (L, mult_prec, TimesT)
  DivOp ->            (L, mult_prec, DivT)
  TimesFloatOp ->     (L, mult_prec, TimesFloatT)
  DivFloatOp ->       (L, mult_prec, DivFloatT)
  ModOp ->            (L, mult_prec, ModT)
  PlusOp ->           (L, add_prec, PlusT)
  MinusOp ->          (L, add_prec, MinusT)
  PlusFloatOp ->      (L, add_prec, PlusFloatT)
  MinusFloatOp ->     (L, add_prec, MinusFloatT)
  EqOp ->             (Non, comp_prec, EqualsT)
  NotEqOp ->          (Non, comp_prec, NotEqualsT)
  GTOp ->             (Non, comp_prec, GreaterThanT)
  LTOp ->             (Non, comp_prec, LessThanT)
  GEqOp ->            (Non, comp_prec, GreaterThanEqT)
  LEqOp ->            (Non, comp_prec, LessThanEqT)
  NatEqOp ->          (Non, comp_prec, NatEqOpT)
  NotNatEqOp ->       (Non, comp_prec, NotNatEqOpT)
  AndOp ->            (L, and_prec, AndOpT)
  OrOp ->             (L, or_prec, OrOpT)
  AssignMutableOp ->  (Non, assign_prec, AssignMutableT)
  SemicolonOp ->      (L, semicolon_prec, SemicolonT)

instance Pretty BinOp where
  pretty = pretty . (\(_, _, t) -> t) . binOpPrec

prettyBinOpExp :: Assoc -> Int -> Token -> ParenInfo -> Int -> Expr b -> Expr b -> ShowS
prettyBinOpExp a prec tok pInfo d u w = showParen parens $
  prettyPPrecExpr pInfoL precL u . showString " " .
  showPretty tok . showString " " .
  prettyPPrecExpr pInfoR precR w where
    parens = showParenOp pInfo d prec
    (precL, precR) = case a of
      L   -> (prec, prec + 1)
      R   -> (prec + 1, prec)
      Non -> (prec + 1, prec + 1)
    pInfoL = pInfo{ isRightMostLet = False, isRightMostIf = tok == SemicolonT, elseFollows = False }
    pInfoR = pInfo{ isRightMostLet = isRightMostLet pInfo || parens
                  , isRightMostIf = isRightMostIf pInfo || parens
                  , elseFollows = not parens && elseFollows pInfo
                  }

-- Pretty printing of Clauses and Patterns

instance Pretty (Clause b) where
  prettyPPrec always _ (Match p e _) = prettyPPrec always 0 p . showString " " . showPretty ArrowT .
    showString " " . prettyPPrec always 0 e

instance Pretty (Pattern b) where
  prettyPPrec always d (Pattern p _) = prettyPPrec always d p

instance Pretty p => Pretty (PatternF p) where
  prettyPPrec always d pf = case pf of
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
      prettyConstrId i . showString sep . prettyPPrecSepList always (prec + 1) " " ps where
        sep = if null ps then "" else " "
        prec = 1
