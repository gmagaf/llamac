module Unit.Parser.Reverse (reverseAST) where

import Common.AST
import Lexer.Lexer (AlexPosn(AlexPn))

reverseAST :: AST AlexPosn
reverseAST = AST
  [ Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ FunDef "reverse"
              [ Param "s"
                ( AlexPn 25 2 15 )
              , Param "r"
                ( AlexPn 27 2 17 )
              ] Nothing
              ( LetIn
                ( Let
                  [ FunDef "l" [] Nothing
                    ( Expr
                      ( FunAppExpr "strlen"
                        [ Expr ( ConstExpr "s" ) ( AlexPn 50 3 20 ) ]
                      )
                      ( AlexPn 43 3 13 )
                    )
                    ( AlexPn 39 3 9 )
                  ]
                  ( AlexPn 35 3 5 )
                )
                ( Expr
                  ( BinOpExpr SemicolonOp
                    ( Expr
                      ( ForExpr "i"
                        ( Expr ( IntCExpr 0 ) ( AlexPn 67 4 13 ) )
                        ( Expr
                          ( BinOpExpr MinusOp
                            ( Expr ( ConstExpr "l" ) ( AlexPn 72 4 18 ) )
                            ( Expr ( IntCExpr 1 ) ( AlexPn 74 4 20 ) )
                          )
                          ( AlexPn 72 4 18 )
                        )
                        ( Expr
                          ( BinOpExpr AssignMutableOp
                            ( Expr
                              ( ArrayAccess "r"
                                [ Expr ( ConstExpr "i" ) ( AlexPn 87 5 9 ) ]
                              )
                              ( AlexPn 85 5 7 )
                            )
                            ( Expr
                              ( UnOpExpr BangOp
                                ( Expr
                                  ( ArrayAccess "s"
                                    [ Expr
                                      ( BinOpExpr MinusOp
                                        ( Expr
                                          ( BinOpExpr MinusOp
                                            ( Expr
                                              ( ConstExpr "l" )
                                              ( AlexPn 96 5 18 )
                                            )
                                            ( Expr
                                              ( ConstExpr "i" )
                                              ( AlexPn 98 5 20 )
                                            )
                                          )
                                          ( AlexPn 96 5 18 )
                                        )
                                        ( Expr
                                          ( IntCExpr 1 )
                                          ( AlexPn 100 5 22 )
                                        )
                                      )
                                      ( AlexPn 96 5 18 )
                                    ]
                                  )
                                  ( AlexPn 94 5 16 )
                                )
                              )
                              ( AlexPn 93 5 15 )
                            )
                          )
                          ( AlexPn 85 5 7 )
                        )
                      )
                      ( AlexPn 59 4 5 )
                    )
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr
                          ( ArrayAccess "r"
                            [ Expr ( ConstExpr "l" ) ( AlexPn 119 7 7 ) ]
                          )
                          ( AlexPn 117 7 5 )
                        )
                        ( Expr ( CharCExpr '\0' ) ( AlexPn 125 7 13 ) )
                      )
                      ( AlexPn 117 7 5 )
                    )
                  )
                  ( AlexPn 59 4 5 )
                )
                ( AlexPn 35 3 5 )
              )
              ( AlexPn 17 2 7 )
            ]
            ( AlexPn 13 2 3 )
          )
          ( LetIn
            ( Let
              [ ArrayDef "p"
                [ Expr ( IntCExpr 20 ) ( AlexPn 151 9 18 ) ] Nothing
                ( AlexPn 140 9 7 )
              ]
              ( AlexPn 136 9 3 )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( FunAppExpr "reverse"
                    [ Expr
                      ( StringCExpr "\n!dlrow olleH" )
                      ( AlexPn 169 11 11 )
                    , Expr
                      ( ConstExpr "p" )
                      ( AlexPn 186 11 28 )
                    ]
                  )
                  ( AlexPn 161 11 3 )
                )
                ( Expr
                  ( FunAppExpr "print_string"
                    [ Expr ( ConstExpr "p" ) ( AlexPn 204 12 16 ) ]
                  )
                  ( AlexPn 191 12 3 )
                )
              )
              ( AlexPn 161 11 3 )
            )
            ( AlexPn 136 9 3 )
          )
          ( AlexPn 13 2 3 )
        )
        ( AlexPn 4 1 5 )
      ]
      ( AlexPn 0 1 1 )
    )
  ]
  ( AlexPn 0 1 1 )
