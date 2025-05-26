module RunTimeLib.LibHeaders (module RunTimeLib.LibHeaders) where

import Common.Token (Identifier)
import Common.AST (TypeF(FunType, RefType))
import Common.SymbolType
     (ConstType(..),
      stringConstType,
      intConstType,
      floatConstType,
      boolConstType,
      charConstType,
      unitConstType)

-- This module contains the signatures of the run-time library

type RunTimeLibSib = (Identifier, ConstType, [Identifier])

libSigs :: [RunTimeLibSib]
libSigs = ioLibSigs ++ mathLibSigs ++ incrLibSigs ++ convertLibSigs ++ stringLibSigs

-- Input/Output
ioLibSigs :: [RunTimeLibSib]
ioLibSigs =
    [ ("print_int",    ConstType (FunType intConstType unitConstType), ["arg0"])
    , ("print_bool",   ConstType (FunType boolConstType unitConstType), ["arg0"])
    , ("print_char",   ConstType (FunType charConstType unitConstType), ["arg0"])
    , ("print_float",  ConstType (FunType floatConstType unitConstType), ["arg0"])
    , ("print_string", ConstType (FunType stringConstType unitConstType), ["arg0"])
    , ("read_int",     ConstType (FunType unitConstType intConstType), ["arg0"])
    , ("read_bool",    ConstType (FunType unitConstType boolConstType), ["arg0"])
    , ("read_char",    ConstType (FunType unitConstType charConstType), ["arg0"])
    , ("read_float",   ConstType (FunType unitConstType floatConstType), ["arg0"])
    , ("read_string",  ConstType (FunType stringConstType unitConstType), ["arg0"])
    ]

-- Math
mathLibSigs :: [RunTimeLibSib]
mathLibSigs =
    [ ("abs",  ConstType (FunType intConstType intConstType), ["arg0"])
    , ("fabs", ConstType (FunType floatConstType floatConstType), ["arg0"])
    , ("sqrt", ConstType (FunType floatConstType floatConstType), ["arg0"])
    , ("sin",  ConstType (FunType floatConstType floatConstType), ["arg0"])
    , ("cos",  ConstType (FunType floatConstType floatConstType), ["arg0"])
    , ("tan",  ConstType (FunType floatConstType floatConstType), ["arg0"])
    , ("atan", ConstType (FunType floatConstType floatConstType), ["arg0"])
    , ("exp",  ConstType (FunType floatConstType floatConstType), ["arg0"])
    , ("ln",   ConstType (FunType floatConstType floatConstType), ["arg0"])
    , ("pi",   ConstType (FunType unitConstType floatConstType), ["arg0"])
    ]

-- Incr/Decr
incrLibSigs :: [RunTimeLibSib]
incrLibSigs =
    [ ("incr", ConstType (FunType (ConstType (RefType intConstType)) unitConstType), ["arg0"])
    , ("decr", ConstType (FunType (ConstType (RefType intConstType)) unitConstType), ["arg0"])
    ]
    
-- Incr/Decr
convertLibSigs :: [RunTimeLibSib]
convertLibSigs = 
    [ ("float_of_int", ConstType (FunType intConstType floatConstType), ["arg0"])
    , ("int_of_float", ConstType (FunType floatConstType intConstType), ["arg0"])
    , ("round",        ConstType (FunType floatConstType intConstType), ["arg0"])
    , ("int_of_char",  ConstType (FunType charConstType intConstType), ["arg0"])
    , ("char_of_int",  ConstType (FunType intConstType charConstType), ["arg0"])
    ]

-- Strings
stringLibSigs :: [RunTimeLibSib]
stringLibSigs =
    [ ("strlen", ConstType (FunType stringConstType intConstType), ["arg0"])
    , ("strcmp", ConstType (FunType stringConstType (ConstType (FunType stringConstType intConstType))), ["arg0", "arg1"])
    , ("strcpy", ConstType (FunType stringConstType (ConstType (FunType stringConstType unitConstType))), ["arg0", "arg1"])
    , ("strcat", ConstType (FunType stringConstType (ConstType (FunType stringConstType unitConstType))), ["arg0", "arg1"])
    ]