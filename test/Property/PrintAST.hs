module Property.PrintAST where

import Data.List (intercalate)
import Common.AST

class Pretty a where
  prettyPrec :: Int -> a -> ShowS
  prettyPrec _ a _ = pretty a
  pretty :: a -> String
  pretty a = prettyPrec 0 a ""

-- False omits parentheses whenever possible
giberish :: Bool
giberish = False

prettyProgram :: Program -> String
prettyProgram ps = foldr aux "" ps where
  aux (Left _) acc  = acc
  aux (Right ts) acc = (pretty ts) ++ "\n" ++ acc

instance Pretty TypeDef where
  pretty (Type tDefs) =
    showString "type " $
    intercalate "\nand  " (map pretty tDefs)

instance Pretty TDef where
  pretty (TDef ide constrs) =
    showString ide .
    showString " = " $
    intercalate " | " (map pretty constrs)

instance Pretty Constr where
  pretty (Constr ide ts) =
      showString ide .
      showString " of " $
      intercalate " " (map pretty ts)

-- infixr 1 ->
-- unary 2 array
-- unary 3 ref
instance Pretty Type where
  prettyPrec _ UnitType = showString "unit"
  prettyPrec _ IntType = showString "int"
  prettyPrec _ CharType = showString "char"
  prettyPrec _ BoolType = showString "bool"
  prettyPrec _ FloatType = showString "float"
  prettyPrec _ (UserDefinedType ide) = showString ide
  prettyPrec d (RefType t) = showParen (giberish || d > ref_prec) $
            prettyPrec (ref_prec + 1) t .
            showString " ref" where
    ref_prec = 3
  prettyPrec d (ArrayType 1 t) = showParen (giberish || d > array_prec) $
            showString "array of " .
            prettyPrec (array_prec + 1) t where
    array_prec = 2
  prettyPrec d (ArrayType n t) = showParen (giberish || d > array_prec) $
            showString "array [" .
            showsStars n .
            showString "] of " .
            prettyPrec (array_prec + 1) t where
    array_prec = 2
    showsStars 1 = showString "*"
    showsStars s = showsStars (s - 1) . showString ", *"
  prettyPrec d (FunType u v) = showParen (giberish || d > fun_prec) $
            prettyPrec (fun_prec + 1) u .
            showString " -> " .
            prettyPrec fun_prec v where
    fun_prec = 1
