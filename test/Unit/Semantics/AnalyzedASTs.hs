module Unit.Semantics.AnalyzedASTs (module Unit.Semantics.AnalyzedASTs) where

import Common.AST
import Common.SymbolType (SymbolType(..), TypeScheme(..))
import Lexer.Lexer (AlexPosn(..))
import Semantics.Utils (SemanticTag(..), TypeInfo(..))

helloWorldSemAST :: AST SemanticTag
helloWorldSemAST =
    [Left (Let [FunDef "main" [] (Expr (FunAppExpr "print_string" [Expr (StringCExpr "Hello world!\\n") (SemTag {posn = AlexPn 24 1 25, typeInfo = NodeType (SymType (ArrayType 1 (SymType CharType)))})]) (SemTag {posn = AlexPn 11 1 12, typeInfo = NodeType (SymType UnitType)})) (SemTag {posn = AlexPn 4 1 5, typeInfo = DefType (MonoType (SymType UnitType))})] (SemTag {posn = AlexPn 0 1 1, typeInfo = NotTypable}))]