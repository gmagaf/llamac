module Unit.Parser.HelloWorld (helloWorldAST) where

import Common.AST
import Lexer.Lexer (AlexPosn(AlexPn))

helloWorldAST :: AST AlexPosn
helloWorldAST = AST
  [ Left
    ( Let
      [ FunDef "main" [] Nothing
        ( Expr
          ( FunAppExpr "print_string"
            [ Expr ( StringCExpr "Hello world!\n" ) ( AlexPn 24 1 25 ) ]
          )
          ( AlexPn 11 1 12 )
        )
        ( AlexPn 4 1 5 )
      ]
      ( AlexPn 0 1 1 )
    )
  ]
  ( AlexPn 0 1 1 )
