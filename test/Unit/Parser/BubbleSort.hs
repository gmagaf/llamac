module Unit.Parser.BubbleSort (bubbleSortAST) where

import Common.AST
import Lexer.Lexer (AlexPosn(AlexPn))

bubbleSortAST :: AST AlexPosn
bubbleSortAST = AST
  [ Left
    ( Let
      [ FunDef "bsort"
        [ Param "x" ( AlexPn 10 1 11 ) ] Nothing
        ( LetIn
          ( Let
            [ FunDef "swap"
              [ Param "x"
                ( AlexPn 25 2 12 )
              , Param "y"
                ( AlexPn 27 2 14 )
              ] Nothing
              ( LetIn
                ( Let
                  [ FunDef "t" [] Nothing
                    ( Expr
                      ( UnOpExpr BangOp
                        ( Expr ( ConstExpr "x" ) ( AlexPn 44 3 14 ) )
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
                      ( BinOpExpr AssignMutableOp
                        ( Expr ( ConstExpr "x" ) ( AlexPn 49 3 19 ) )
                        ( Expr
                          ( UnOpExpr BangOp
                            ( Expr ( ConstExpr "y" ) ( AlexPn 55 3 25 ) )
                          )
                          ( AlexPn 54 3 24 )
                        )
                      )
                      ( AlexPn 49 3 19 )
                    )
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr ( ConstExpr "y" ) ( AlexPn 58 3 28 ) )
                        ( Expr ( ConstExpr "t" ) ( AlexPn 63 3 33 ) )
                      )
                      ( AlexPn 58 3 28 )
                    )
                  )
                  ( AlexPn 49 3 19 )
                )
                ( AlexPn 35 3 5 )
              )
              ( AlexPn 20 2 7 )
            ]
            ( AlexPn 16 2 3 )
          )
          ( LetIn
            ( Let
              [ VarDef "changed" Nothing ( AlexPn 74 4 7 ) ]
              ( AlexPn 70 4 3 )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( BinOpExpr AssignMutableOp
                    ( Expr ( ConstExpr "changed" ) ( AlexPn 95 5 3 ) )
                    ( Expr TrueCExpr ( AlexPn 106 5 14 ) )
                  )
                  ( AlexPn 95 5 3 )
                )
                ( Expr
                  ( WhileExpr
                    ( Expr
                      ( UnOpExpr BangOp
                        ( Expr ( ConstExpr "changed" ) ( AlexPn 121 6 10 ) )
                      )
                      ( AlexPn 120 6 9 )
                    )
                    ( Expr
                      ( BinOpExpr SemicolonOp
                        ( Expr
                          ( BinOpExpr AssignMutableOp
                            ( Expr ( ConstExpr "changed" ) ( AlexPn 136 7 5 ) )
                            ( Expr FalseCExpr ( AlexPn 147 7 16 ) )
                          )
                          ( AlexPn 136 7 5 )
                        )
                        ( Expr
                          ( ForExpr "i"
                            ( Expr ( IntCExpr 0 ) ( AlexPn 166 8 13 ) )
                            ( Expr
                              ( BinOpExpr MinusOp
                                ( Expr ( ArrayDim "x" 1 ) ( AlexPn 171 8 18 ) )
                                ( Expr ( IntCExpr 2 ) ( AlexPn 179 8 26 ) )
                              )
                              ( AlexPn 171 8 18 )
                            )
                            ( Expr
                              ( IfThenExpr
                                ( Expr
                                  ( BinOpExpr GTOp
                                    ( Expr
                                      ( UnOpExpr BangOp
                                        ( Expr
                                          ( ArrayAccess "x"
                                            [ Expr
                                              ( ConstExpr "i" )
                                              ( AlexPn 196 9 13 )
                                            ]
                                          )
                                          ( AlexPn 194 9 11 )
                                        )
                                      )
                                      ( AlexPn 193 9 10 )
                                    )
                                    ( Expr
                                      ( UnOpExpr BangOp
                                        ( Expr
                                          ( ArrayAccess "x"
                                            [ Expr
                                              ( BinOpExpr PlusOp
                                                ( Expr
                                                  ( ConstExpr "i" )
                                                  ( AlexPn 204 9 21 )
                                                )
                                                ( Expr
                                                  ( IntCExpr 1 )
                                                  ( AlexPn 206 9 23 )
                                                )
                                              )
                                              ( AlexPn 204 9 21 )
                                            ]
                                          )
                                          ( AlexPn 202 9 19 )
                                        )
                                      )
                                      ( AlexPn 201 9 18 )
                                    )
                                  )
                                  ( AlexPn 193 9 10 )
                                )
                                ( Expr
                                  ( BeginExpr
                                    ( Expr
                                      ( BinOpExpr SemicolonOp
                                        ( Expr
                                          ( FunAppExpr "swap"
                                            [ Expr
                                              ( ArrayAccess "x"
                                                [ Expr
                                                  ( ConstExpr "i" )
                                                  ( AlexPn 241 11 16 )
                                                ]
                                              )
                                              ( AlexPn 239 11 14 )
                                            , Expr
                                              ( ArrayAccess "x"
                                                [ Expr
                                                  ( BinOpExpr PlusOp
                                                    ( Expr
                                                      ( ConstExpr "i" )
                                                      ( AlexPn 246 11 21 )
                                                    )
                                                    ( Expr
                                                      ( IntCExpr 1 )
                                                      ( AlexPn 248 11 23 )
                                                    )
                                                  )
                                                  ( AlexPn 246 11 21 )
                                                ]
                                              )
                                              ( AlexPn 244 11 19 )
                                            ]
                                          )
                                          ( AlexPn 234 11 9 )
                                        )
                                        ( Expr
                                          ( BinOpExpr AssignMutableOp
                                            ( Expr
                                              ( ConstExpr "changed" )
                                              ( AlexPn 260 12 9 )
                                            )
                                            ( Expr TrueCExpr
                                              ( AlexPn 271 12 20 )
                                            )
                                          )
                                          ( AlexPn 260 12 9 )
                                        )
                                      )
                                      ( AlexPn 234 11 9 )
                                    )
                                  )
                                  ( AlexPn 220 10 7 )
                                )
                              )
                              ( AlexPn 190 9 7 )
                            )
                          )
                          ( AlexPn 158 8 5 )
                        )
                      )
                      ( AlexPn 136 7 5 )
                    )
                  )
                  ( AlexPn 114 6 3 )
                )
              )
              ( AlexPn 95 5 3 )
            )
            ( AlexPn 70 4 3 )
          )
          ( AlexPn 16 2 3 )
        )
        ( AlexPn 4 1 5 )
      ]
      ( AlexPn 0 1 1 )
    )
  , Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ FunDef "print_array"
              [ Param "msg"
                ( AlexPn 332 18 19 )
              , Param "x"
                ( AlexPn 336 18 23 )
              ] Nothing
              ( Expr
                ( BinOpExpr SemicolonOp
                  ( Expr
                    ( BinOpExpr SemicolonOp
                      ( Expr
                        ( FunAppExpr "print_string"
                          [ Expr ( ConstExpr "msg" ) ( AlexPn 357 19 18 ) ]
                        )
                        ( AlexPn 344 19 5 )
                      )
                      ( Expr
                        ( ForExpr "i"
                          ( Expr ( IntCExpr 0 ) ( AlexPn 374 20 13 ) )
                          ( Expr
                            ( BinOpExpr MinusOp
                              ( Expr ( ArrayDim "x" 1 ) ( AlexPn 379 20 18 ) )
                              ( Expr ( IntCExpr 1 ) ( AlexPn 387 20 26 ) )
                            )
                            ( AlexPn 379 20 18 )
                          )
                          ( Expr
                            ( BinOpExpr SemicolonOp
                              ( Expr
                                ( IfThenExpr
                                  ( Expr
                                    ( BinOpExpr GTOp
                                      ( Expr
                                        ( ConstExpr "i" )
                                        ( AlexPn 401 21 10 )
                                      )
                                      ( Expr
                                        ( IntCExpr 0 )
                                        ( AlexPn 405 21 14 )
                                      )
                                    )
                                    ( AlexPn 401 21 10 )
                                  )
                                  ( Expr
                                    ( FunAppExpr "print_string"
                                      [ Expr
                                        ( StringCExpr ", " )
                                        ( AlexPn 425 21 34 )
                                      ]
                                    )
                                    ( AlexPn 412 21 21 )
                                  )
                                )
                                ( AlexPn 398 21 7 )
                              )
                              ( Expr
                                ( FunAppExpr "print_int"
                                  [ Expr
                                    ( UnOpExpr BangOp
                                      ( Expr
                                        ( ArrayAccess "x"
                                          [ Expr
                                            ( ConstExpr "i" )
                                            ( AlexPn 450 22 20 )
                                          ]
                                        )
                                        ( AlexPn 448 22 18 )
                                      )
                                    )
                                    ( AlexPn 447 22 17 )
                                  ]
                                )
                                ( AlexPn 437 22 7 )
                              )
                            )
                            ( AlexPn 398 21 7 )
                          )
                        )
                        ( AlexPn 366 20 5 )
                      )
                    )
                    ( AlexPn 344 19 5 )
                  )
                  ( Expr
                    ( FunAppExpr "print_string"
                      [ Expr ( StringCExpr "\n" ) ( AlexPn 480 24 18 ) ]
                    )
                    ( AlexPn 467 24 5 )
                  )
                )
                ( AlexPn 344 19 5 )
              )
              ( AlexPn 320 18 7 )
            ]
            ( AlexPn 316 18 3 )
          )
          ( LetIn
            ( Let
              [ VarDef "seed" Nothing
                ( AlexPn 495 26 7 )
              , ArrayDef "x"
                [ Expr ( IntCExpr 16 ) ( AlexPn 524 27 17 ) ] Nothing
                ( AlexPn 514 27 7 )
              ]
              ( AlexPn 491 26 3 )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( BinOpExpr SemicolonOp
                    ( Expr
                      ( BinOpExpr SemicolonOp
                        ( Expr
                          ( BinOpExpr SemicolonOp
                            ( Expr
                              ( BinOpExpr AssignMutableOp
                                ( Expr
                                  ( ConstExpr "seed" )
                                  ( AlexPn 533 28 3 )
                                )
                                ( Expr ( IntCExpr 65 ) ( AlexPn 541 28 11 ) )
                              )
                              ( AlexPn 533 28 3 )
                            )
                            ( Expr
                              ( ForExpr "i"
                                ( Expr ( IntCExpr 0 ) ( AlexPn 555 29 11 ) )
                                ( Expr ( IntCExpr 15 ) ( AlexPn 560 29 16 ) )
                                ( Expr
                                  ( BinOpExpr SemicolonOp
                                    ( Expr
                                      ( BinOpExpr AssignMutableOp
                                        ( Expr
                                          ( ConstExpr "seed" )
                                          ( AlexPn 570 30 5 )
                                        )
                                        ( Expr
                                          ( BinOpExpr ModOp
                                            ( Expr
                                              ( BinOpExpr PlusOp
                                                ( Expr
                                                  ( BinOpExpr PlusOp
                                                    ( Expr
                                                      ( BinOpExpr TimesOp
                                                        ( Expr
                                                          ( UnOpExpr BangOp
                                                            ( Expr
                                                              ( ConstExpr "seed" )
                                                              ( AlexPn 580 30 15 )
                                                            )
                                                          )
                                                          ( AlexPn 579 30 14 )
                                                        )
                                                        ( Expr
                                                          ( IntCExpr 137 )
                                                          ( AlexPn 587 30 22 )
                                                        )
                                                      )
                                                      ( AlexPn 579 30 14 )
                                                    )
                                                    ( Expr
                                                      ( IntCExpr 220 )
                                                      ( AlexPn 593 30 28 )
                                                    )
                                                  )
                                                  ( AlexPn 579 30 14 )
                                                )
                                                ( Expr
                                                  ( ConstExpr "i" )
                                                  ( AlexPn 599 30 34 )
                                                )
                                              )
                                              ( AlexPn 579 30 14 )
                                            )
                                            ( Expr
                                              ( IntCExpr 101 )
                                              ( AlexPn 606 30 41 )
                                            )
                                          )
                                          ( AlexPn 578 30 13 )
                                        )
                                      )
                                      ( AlexPn 570 30 5 )
                                    )
                                    ( Expr
                                      ( BinOpExpr AssignMutableOp
                                        ( Expr
                                          ( ArrayAccess "x"
                                            [ Expr
                                              ( ConstExpr "i" )
                                              ( AlexPn 617 31 7 )
                                            ]
                                          )
                                          ( AlexPn 615 31 5 )
                                        )
                                        ( Expr
                                          ( UnOpExpr BangOp
                                            ( Expr
                                              ( ConstExpr "seed" )
                                              ( AlexPn 624 31 14 )
                                            )
                                          )
                                          ( AlexPn 623 31 13 )
                                        )
                                      )
                                      ( AlexPn 615 31 5 )
                                    )
                                  )
                                  ( AlexPn 570 30 5 )
                                )
                              )
                              ( AlexPn 547 29 3 )
                            )
                          )
                          ( AlexPn 533 28 3 )
                        )
                        ( Expr
                          ( FunAppExpr "print_array"
                            [ Expr
                              ( StringCExpr "Initial array: " )
                              ( AlexPn 651 33 15 )
                            , Expr
                              ( ConstExpr "x" )
                              ( AlexPn 669 33 33 )
                            ]
                          )
                          ( AlexPn 639 33 3 )
                        )
                      )
                      ( AlexPn 533 28 3 )
                    )
                    ( Expr
                      ( FunAppExpr "bsort"
                        [ Expr ( ConstExpr "x" ) ( AlexPn 680 34 9 ) ]
                      )
                      ( AlexPn 674 34 3 )
                    )
                  )
                  ( AlexPn 533 28 3 )
                )
                ( Expr
                  ( FunAppExpr "print_array"
                    [ Expr
                      ( StringCExpr "Sorted array: " )
                      ( AlexPn 697 35 15 )
                    , Expr
                      ( ConstExpr "x" )
                      ( AlexPn 714 35 32 )
                    ]
                  )
                  ( AlexPn 685 35 3 )
                )
              )
              ( AlexPn 533 28 3 )
            )
            ( AlexPn 491 26 3 )
          )
          ( AlexPn 316 18 3 )
        )
        ( AlexPn 307 17 5 )
      ]
      ( AlexPn 303 17 1 )
    )
  ]
  ( AlexPn 0 1 1 )
