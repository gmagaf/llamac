module Unit.Parser.Primes (primesAST) where

import Common.AST
import Lexer.Lexer (AlexPosn(AlexPn))

primesAST :: AST AlexPosn
primesAST = AST
  [ Left
    ( LetRec
      [ FunDef "prime"
        [ Param "n" ( AlexPn 14 1 15 ) ] Nothing
        ( Expr
          ( IfThenElseExpr
            ( Expr
              ( BinOpExpr LTOp
                ( Expr ( ConstExpr "n" ) ( AlexPn 28 2 11 ) )
                ( Expr ( IntCExpr 0 ) ( AlexPn 32 2 15 ) )
              )
              ( AlexPn 28 2 11 )
            )
            ( Expr
              ( FunAppExpr "prime"
                [ Expr
                  ( UnOpExpr MinusUnOp
                    ( Expr ( ConstExpr "n" ) ( AlexPn 53 2 36 ) )
                  )
                  ( AlexPn 52 2 35 )
                ]
              )
              ( AlexPn 45 2 28 )
            )
            ( Expr
              ( IfThenElseExpr
                ( Expr
                  ( BinOpExpr LTOp
                    ( Expr ( ConstExpr "n" ) ( AlexPn 66 3 11 ) )
                    ( Expr ( IntCExpr 2 ) ( AlexPn 70 3 15 ) )
                  )
                  ( AlexPn 66 3 11 )
                )
                ( Expr FalseCExpr ( AlexPn 83 3 28 ) )
                ( Expr
                  ( IfThenElseExpr
                    ( Expr
                      ( BinOpExpr EqOp
                        ( Expr ( ConstExpr "n" ) ( AlexPn 99 4 11 ) )
                        ( Expr ( IntCExpr 2 ) ( AlexPn 103 4 15 ) )
                      )
                      ( AlexPn 99 4 11 )
                    )
                    ( Expr TrueCExpr ( AlexPn 116 4 28 ) )
                    ( Expr
                      ( IfThenElseExpr
                        ( Expr
                          ( BinOpExpr EqOp
                            ( Expr
                              ( BinOpExpr ModOp
                                ( Expr ( ConstExpr "n" ) ( AlexPn 131 5 11 ) )
                                ( Expr ( IntCExpr 2 ) ( AlexPn 137 5 17 ) )
                              )
                              ( AlexPn 131 5 11 )
                            )
                            ( Expr ( IntCExpr 0 ) ( AlexPn 141 5 21 ) )
                          )
                          ( AlexPn 131 5 11 )
                        )
                        ( Expr FalseCExpr ( AlexPn 148 5 28 ) )
                        ( LetIn
                          ( LetRec
                            [ FunDef "loop"
                              [ Param "i" ( AlexPn 174 6 21 ) ] Nothing
                              ( Expr
                                ( IfThenElseExpr
                                  ( Expr
                                    ( BinOpExpr LEqOp
                                      ( Expr
                                        ( ConstExpr "i" )
                                        ( AlexPn 185 7 8 )
                                      )
                                      ( Expr
                                        ( BinOpExpr DivOp
                                          ( Expr
                                            ( ConstExpr "n" )
                                            ( AlexPn 190 7 13 )
                                          )
                                          ( Expr
                                            ( IntCExpr 2 )
                                            ( AlexPn 194 7 17 )
                                          )
                                        )
                                        ( AlexPn 190 7 13 )
                                      )
                                    )
                                    ( AlexPn 185 7 8 )
                                  )
                                  ( Expr
                                    ( IfThenElseExpr
                                      ( Expr
                                        ( BinOpExpr EqOp
                                          ( Expr
                                            ( BinOpExpr ModOp
                                              ( Expr
                                                ( ConstExpr "n" )
                                                ( AlexPn 210 8 10 )
                                              )
                                              ( Expr
                                                ( ConstExpr "i" )
                                                ( AlexPn 216 8 16 )
                                              )
                                            )
                                            ( AlexPn 210 8 10 )
                                          )
                                          ( Expr
                                            ( IntCExpr 0 )
                                            ( AlexPn 220 8 20 )
                                          )
                                        )
                                        ( AlexPn 210 8 10 )
                                      )
                                      ( Expr FalseCExpr ( AlexPn 227 8 27 ) )
                                      ( Expr
                                        ( FunAppExpr "loop"
                                          [ Expr
                                            ( BinOpExpr PlusOp
                                              ( Expr
                                                ( ConstExpr "i" )
                                                ( AlexPn 265 9 33 )
                                              )
                                              ( Expr
                                                ( IntCExpr 2 )
                                                ( AlexPn 267 9 35 )
                                              )
                                            )
                                            ( AlexPn 265 9 33 )
                                          ]
                                        )
                                        ( AlexPn 259 9 27 )
                                      )
                                    )
                                    ( AlexPn 207 8 7 )
                                  )
                                  ( Expr TrueCExpr ( AlexPn 289 11 9 ) )
                                )
                                ( AlexPn 182 7 5 )
                              )
                              ( AlexPn 169 6 16 )
                            ]
                            ( AlexPn 161 6 8 )
                          )
                          ( Expr
                            ( FunAppExpr "loop"
                              [ Expr ( IntCExpr 3 ) ( AlexPn 306 12 10 ) ]
                            )
                            ( AlexPn 301 12 5 )
                          )
                          ( AlexPn 161 6 8 )
                        )
                      )
                      ( AlexPn 128 5 8 )
                    )
                  )
                  ( AlexPn 96 4 8 )
                )
              )
              ( AlexPn 63 3 8 )
            )
          )
          ( AlexPn 25 2 8 )
        )
        ( AlexPn 8 1 9 )
      ]
      ( AlexPn 0 1 1 )
    )
  , Left
    ( Let
      [ FunDef "main" [] Nothing
        ( Expr
          ( BinOpExpr SemicolonOp
            ( Expr
              ( FunAppExpr "print_string"
                [ Expr
                  ( StringCExpr "Please, give the upper limit: " )
                  ( AlexPn 335 15 16 )
                ]
              )
              ( AlexPn 322 15 3 )
            )
            ( LetIn
              ( Let
                [ FunDef "limit" [] Nothing
                  ( Expr
                    ( FunAppExpr "read_int"
                      [ Expr UnitCExpr ( AlexPn 392 16 24 ) ]
                    )
                    ( AlexPn 383 16 15 )
                  )
                  ( AlexPn 375 16 7 )
                ]
                ( AlexPn 371 16 3 )
              )
              ( Expr
                ( BinOpExpr SemicolonOp
                  ( Expr
                    ( BinOpExpr SemicolonOp
                      ( Expr
                        ( BinOpExpr SemicolonOp
                          ( Expr
                            ( FunAppExpr "print_string"
                              [ Expr
                                ( StringCExpr "Prime numbers between 0 and " )
                                ( AlexPn 413 17 16 )
                              ]
                            )
                            ( AlexPn 400 17 3 )
                          )
                          ( Expr
                            ( FunAppExpr "print_int"
                              [ Expr
                                ( ConstExpr "limit" )
                                ( AlexPn 457 18 13 )
                              ]
                            )
                            ( AlexPn 447 18 3 )
                          )
                        )
                        ( AlexPn 400 17 3 )
                      )
                      ( Expr
                        ( FunAppExpr "print_string"
                          [ Expr ( StringCExpr "\n\n" ) ( AlexPn 479 19 16 ) ]
                        )
                        ( AlexPn 466 19 3 )
                      )
                    )
                    ( AlexPn 400 17 3 )
                  )
                  ( LetIn
                    ( Let
                      [ VarDef "counter" Nothing ( AlexPn 493 20 7 ) ]
                      ( AlexPn 489 20 3 )
                    )
                    ( Expr
                      ( BinOpExpr SemicolonOp
                        ( Expr
                          ( BinOpExpr SemicolonOp
                            ( Expr
                              ( BinOpExpr SemicolonOp
                                ( Expr
                                  ( BinOpExpr AssignMutableOp
                                    ( Expr
                                      ( ConstExpr "counter" )
                                      ( AlexPn 514 21 3 )
                                    )
                                    ( Expr ( IntCExpr 0 ) ( AlexPn 525 21 14 ) )
                                  )
                                  ( AlexPn 514 21 3 )
                                )
                                ( Expr
                                  ( IfThenExpr
                                    ( Expr
                                      ( BinOpExpr GEqOp
                                        ( Expr
                                          ( ConstExpr "limit" )
                                          ( AlexPn 533 22 6 )
                                        )
                                        ( Expr
                                          ( IntCExpr 2 )
                                          ( AlexPn 542 22 15 )
                                        )
                                      )
                                      ( AlexPn 533 22 6 )
                                    )
                                    ( Expr
                                      ( BinOpExpr SemicolonOp
                                        ( Expr
                                          ( FunAppExpr "incr"
                                            [ Expr
                                              ( ConstExpr "counter" )
                                              ( AlexPn 555 22 28 )
                                            ]
                                          )
                                          ( AlexPn 550 22 23 )
                                        )
                                        ( Expr
                                          ( FunAppExpr "print_string"
                                            [ Expr
                                              ( StringCExpr "2\n" )
                                              ( AlexPn 577 22 50 )
                                            ]
                                          )
                                          ( AlexPn 564 22 37 )
                                        )
                                      )
                                      ( AlexPn 550 22 23 )
                                    )
                                  )
                                  ( AlexPn 530 22 3 )
                                )
                              )
                              ( AlexPn 514 21 3 )
                            )
                            ( Expr
                              ( IfThenExpr
                                ( Expr
                                  ( BinOpExpr GEqOp
                                    ( Expr
                                      ( ConstExpr "limit" )
                                      ( AlexPn 590 23 6 )
                                    )
                                    ( Expr ( IntCExpr 3 ) ( AlexPn 599 23 15 ) )
                                  )
                                  ( AlexPn 590 23 6 )
                                )
                                ( Expr
                                  ( BinOpExpr SemicolonOp
                                    ( Expr
                                      ( FunAppExpr "incr"
                                        [ Expr
                                          ( ConstExpr "counter" )
                                          ( AlexPn 612 23 28 )
                                        ]
                                      )
                                      ( AlexPn 607 23 23 )
                                    )
                                    ( Expr
                                      ( FunAppExpr "print_string"
                                        [ Expr
                                          ( StringCExpr "3\n" )
                                          ( AlexPn 634 23 50 )
                                        ]
                                      )
                                      ( AlexPn 621 23 37 )
                                    )
                                  )
                                  ( AlexPn 607 23 23 )
                                )
                              )
                              ( AlexPn 587 23 3 )
                            )
                          )
                          ( AlexPn 514 21 3 )
                        )
                        ( LetIn
                          ( LetRec
                            [ FunDef "loop"
                              [ Param "number" ( AlexPn 657 24 16 ) ] Nothing
                              ( Expr
                                ( IfThenExpr
                                  ( Expr
                                    ( BinOpExpr LEqOp
                                      ( Expr
                                        ( ConstExpr "number" )
                                        ( AlexPn 673 25 8 )
                                      )
                                      ( Expr
                                        ( ConstExpr "limit" )
                                        ( AlexPn 683 25 18 )
                                      )
                                    )
                                    ( AlexPn 673 25 8 )
                                  )
                                  ( Expr
                                    ( BeginExpr
                                      ( Expr
                                        ( BinOpExpr SemicolonOp
                                          ( Expr
                                            ( BinOpExpr SemicolonOp
                                              ( Expr
                                                ( IfThenExpr
                                                  ( Expr
                                                    ( FunAppExpr "prime"
                                                      [ Expr
                                                        ( BinOpExpr MinusOp
                                                          ( Expr
                                                            ( ConstExpr "number" )
                                                            ( AlexPn 720 27 17 )
                                                          )
                                                          ( Expr
                                                            ( IntCExpr 1 )
                                                            ( AlexPn 729 27 26 )
                                                          )
                                                        )
                                                        ( AlexPn 720 27 17 )
                                                      ]
                                                    )
                                                    ( AlexPn 713 27 10 )
                                                  )
                                                  ( Expr
                                                    ( BeginExpr
                                                      ( Expr
                                                        ( BinOpExpr SemicolonOp
                                                          ( Expr
                                                            ( BinOpExpr SemicolonOp
                                                              ( Expr
                                                                ( FunAppExpr "incr"
                                                                  [ Expr
                                                                    ( ConstExpr "counter" )
                                                                    ( AlexPn 762 29 14 )
                                                                  ]
                                                                )
                                                                ( AlexPn 757 29 9 )
                                                              )
                                                              ( Expr
                                                                ( FunAppExpr "print_int"
                                                                  [ Expr
                                                                    ( BinOpExpr MinusOp
                                                                      ( Expr
                                                                        ( ConstExpr "number" )
                                                                        ( AlexPn 790 30 20 )
                                                                      )
                                                                      ( Expr
                                                                        ( IntCExpr 1 )
                                                                        ( AlexPn 799 30 29 )
                                                                      )
                                                                    )
                                                                    ( AlexPn 790 30 20 )
                                                                  ]
                                                                )
                                                                ( AlexPn 779 30 9 )
                                                              )
                                                            )
                                                            ( AlexPn 757 29 9 )
                                                          )
                                                          ( Expr
                                                            ( FunAppExpr "print_string"
                                                              [ Expr
                                                                ( StringCExpr "\n" )
                                                                ( AlexPn 824 31 22 )
                                                              ]
                                                            )
                                                            ( AlexPn 811 31 9 )
                                                          )
                                                        )
                                                        ( AlexPn 757 29 9 )
                                                      )
                                                    )
                                                    ( AlexPn 743 28 7 )
                                                  )
                                                )
                                                ( AlexPn 710 27 7 )
                                              )
                                              ( Expr
                                                ( IfThenExpr
                                                  ( Expr
                                                    ( BinOpExpr AndOp
                                                      ( Expr
                                                        ( BinOpExpr NotEqOp
                                                          ( Expr
                                                            ( ConstExpr "number" )
                                                            ( AlexPn 849 33 10 )
                                                          )
                                                          ( Expr
                                                            ( ConstExpr "limit" )
                                                            ( AlexPn 859 33 20 )
                                                          )
                                                        )
                                                        ( AlexPn 849 33 10 )
                                                      )
                                                      ( Expr
                                                        ( FunAppExpr "prime"
                                                          [ Expr
                                                            ( BinOpExpr PlusOp
                                                              ( Expr
                                                                ( ConstExpr "number" )
                                                                ( AlexPn 875 33 36 )
                                                              )
                                                              ( Expr
                                                                ( IntCExpr 1 )
                                                                ( AlexPn 884 33 45 )
                                                              )
                                                            )
                                                            ( AlexPn 875 33 36 )
                                                          ]
                                                        )
                                                        ( AlexPn 868 33 29 )
                                                      )
                                                    )
                                                    ( AlexPn 849 33 10 )
                                                  )
                                                  ( Expr
                                                    ( BeginExpr
                                                      ( Expr
                                                        ( BinOpExpr SemicolonOp
                                                          ( Expr
                                                            ( BinOpExpr SemicolonOp
                                                              ( Expr
                                                                ( FunAppExpr "incr"
                                                                  [ Expr
                                                                    ( ConstExpr "counter" )
                                                                    ( AlexPn 917 35 14 )
                                                                  ]
                                                                )
                                                                ( AlexPn 912 35 9 )
                                                              )
                                                              ( Expr
                                                                ( FunAppExpr "print_int"
                                                                  [ Expr
                                                                    ( BinOpExpr PlusOp
                                                                      ( Expr
                                                                        ( ConstExpr "number" )
                                                                        ( AlexPn 945 36 20 )
                                                                      )
                                                                      ( Expr
                                                                        ( IntCExpr 1 )
                                                                        ( AlexPn 954 36 29 )
                                                                      )
                                                                    )
                                                                    ( AlexPn 945 36 20 )
                                                                  ]
                                                                )
                                                                ( AlexPn 934 36 9 )
                                                              )
                                                            )
                                                            ( AlexPn 912 35 9 )
                                                          )
                                                          ( Expr
                                                            ( FunAppExpr "print_string"
                                                              [ Expr
                                                                ( StringCExpr "\n" )
                                                                ( AlexPn 979 37 22 )
                                                              ]
                                                            )
                                                            ( AlexPn 966 37 9 )
                                                          )
                                                        )
                                                        ( AlexPn 912 35 9 )
                                                      )
                                                    )
                                                    ( AlexPn 898 34 7 )
                                                  )
                                                )
                                                ( AlexPn 846 33 7 )
                                              )
                                            )
                                            ( AlexPn 710 27 7 )
                                          )
                                          ( Expr
                                            ( FunAppExpr "loop"
                                              [ Expr
                                                ( BinOpExpr PlusOp
                                                  ( Expr
                                                    ( ConstExpr "number" )
                                                    ( AlexPn 1007 39 13 )
                                                  )
                                                  ( Expr
                                                    ( IntCExpr 6 )
                                                    ( AlexPn 1016 39 22 )
                                                  )
                                                )
                                                ( AlexPn 1007 39 13 )
                                              ]
                                            )
                                            ( AlexPn 1001 39 7 )
                                          )
                                        )
                                        ( AlexPn 710 27 7 )
                                      )
                                    )
                                    ( AlexPn 698 26 5 )
                                  )
                                )
                                ( AlexPn 670 25 5 )
                              )
                              ( AlexPn 652 24 11 )
                            ]
                            ( AlexPn 644 24 3 )
                          )
                          ( Expr
                            ( BinOpExpr SemicolonOp
                              ( Expr
                                ( BinOpExpr SemicolonOp
                                  ( Expr
                                    ( BinOpExpr SemicolonOp
                                      ( Expr
                                        ( FunAppExpr "loop"
                                          [ Expr
                                            ( IntCExpr 6 )
                                            ( AlexPn 1037 41 8 )
                                          ]
                                        )
                                        ( AlexPn 1032 41 3 )
                                      )
                                      ( Expr
                                        ( FunAppExpr "print_string"
                                          [ Expr
                                            ( StringCExpr "\n" )
                                            ( AlexPn 1055 42 16 )
                                          ]
                                        )
                                        ( AlexPn 1042 42 3 )
                                      )
                                    )
                                    ( AlexPn 1032 41 3 )
                                  )
                                  ( Expr
                                    ( FunAppExpr "print_int"
                                      [ Expr
                                        ( UnOpExpr BangOp
                                          ( Expr
                                            ( ConstExpr "counter" )
                                            ( AlexPn 1074 43 14 )
                                          )
                                        )
                                        ( AlexPn 1073 43 13 )
                                      ]
                                    )
                                    ( AlexPn 1063 43 3 )
                                  )
                                )
                                ( AlexPn 1032 41 3 )
                              )
                              ( Expr
                                ( FunAppExpr "print_string"
                                  [ Expr
                                    ( StringCExpr " prime number(s) were found.\n" )
                                    ( AlexPn 1098 44 16 )
                                  ]
                                )
                                ( AlexPn 1085 44 3 )
                              )
                            )
                            ( AlexPn 1032 41 3 )
                          )
                          ( AlexPn 644 24 3 )
                        )
                      )
                      ( AlexPn 514 21 3 )
                    )
                    ( AlexPn 489 20 3 )
                  )
                )
                ( AlexPn 400 17 3 )
              )
              ( AlexPn 371 16 3 )
            )
          )
          ( AlexPn 322 15 3 )
        )
        ( AlexPn 313 14 5 )
      ]
      ( AlexPn 309 14 1 )
    )
  ]
  ( AlexPn 0 1 1 )
