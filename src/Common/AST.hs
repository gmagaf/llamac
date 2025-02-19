module Common.AST (module Common.AST) where

import Common.Token (Identifier,
                     ConstrIdentifier,
                     IntConstant,
                     FloatConstant,
                     CharConstant,
                     StringConstant)

type Program = [Either LetDef TypeDef]

data LetDef = Let [Def]
            | LetRec [Def]
  deriving Show

data Def = Def Identifier [Param] Expr
         | TypedDef Identifier [Param] Type Expr
         | MutDef Identifier
         | MutTypedDef Identifier Type
         | ArrayDef Identifier [Expr]
         | ArrayTypedDef Identifier [Expr] Type
  deriving Show

data TypeDef = Type [TDef]
  deriving Show

data TDef = TDef Identifier [Constr]
  deriving Show

data Constr = Constr ConstrIdentifier [Type]
  deriving Show

data Param = Param Identifier
           | TypedParam Identifier Type
  deriving Show

data Type = UnitType | IntType | CharType | BoolType | FloatType
          | NestedType Type | FunType Type Type | RefType Type
          | ArrayType Int Type | UserDefinedType Identifier
  deriving Show

data Expr = IntCExpr IntConstant
          | FloatCExpr FloatConstant
          | CharCExpr CharConstant
          | StringCExpr StringConstant
          | TrueCExpr
          | FalseCExpr
          | UnitCExpr
          | NestedExpr Expr
          | UnOpExpr UnOp Expr
          | BinOpExpr BinOp Expr Expr
          | FunAppExpr Identifier [Expr]
          | ConstrAppExpr ConstrIdentifier [Expr]
          | ArrayAccess Identifier [Expr]
          | ArrayDim Identifier
          | ArrayDimMult Identifier Int
          | NewType Type
          | DeleteExpr Expr
          | LetIn LetDef Expr
          | BeginExpr Expr
          | IfThenExpr Expr Expr
          | IfThenElseExpr Expr Expr Expr
          | WhileExpr Expr Expr
          | ForExpr Identifier Expr Expr Expr
          | ForDownExpr Identifier Expr Expr Expr
          | MatchExpr Expr [Clause]
  deriving Show


data UnOp = PlusUnOp | MinusUnOp | PlusFloatUnOp
          | MinusFloatUnOp | BangOp | NotOp
  deriving Show

data BinOp = PlusOp | MinusOp | TimesOp | DivOp
           | PlusFloatOp | MinusFloatOp | TimesFloatOp | DivFloatOp
           | ModOp | ExpOp | AssignOp | NotStructEqOp
           | LTOp | GTOp | LEqOp | GEqOp
           | NatEqOp | NotNatEqOp | AndOp | OrOp
           | SemicolonOp | AssignMutableOp
  deriving Show

data Clause = Match Pattern Expr
  deriving Show

data Pattern = IntConstPattern IntConstant
             | PlusIntConstPattern IntConstant | MinusIntConstPattern IntConstant
             | FloatConstPattern FloatConstant
             | PlusFloatConstPattern FloatConstant | MinusFloatConstPattern FloatConstant
             | CharConstPattern CharConstant
             | TruePattern | FalsePattern
             | IdPattern Identifier
             | NestedPattern Pattern
             | ConstrPattern ConstrIdentifier [Pattern]
  deriving Show
