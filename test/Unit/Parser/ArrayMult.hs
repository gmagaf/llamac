module Unit.Parser.ArrayMult (arrayMultAST) where

import Common.AST
import Lexer.Lexer (AlexPosn(AlexPn))

arrayMultAST :: AST AlexPosn
arrayMultAST = AST
  [ Left
    ( Let
      [ FunDef "mmult"
        [ Param "a"
          ( AlexPn 10 1 11 )
        , Param "b"
          ( AlexPn 12 1 13 )
        , Param "c"
          ( AlexPn 14 1 15 )
        ] Nothing
        ( Expr
          ( IfThenExpr
            ( Expr
              ( BinOpExpr AndOp
                ( Expr
                  ( BinOpExpr AndOp
                    ( Expr
                      ( BinOpExpr EqOp
                        ( Expr ( ArrayDim "a" 2 ) ( AlexPn 23 2 6 ) )
                        ( Expr ( ArrayDim "b" 1 ) ( AlexPn 33 2 16 ) )
                      )
                      ( AlexPn 23 2 6 )
                    )
                    ( Expr
                      ( BinOpExpr EqOp
                        ( Expr ( ArrayDim "c" 1 ) ( AlexPn 44 2 27 ) )
                        ( Expr ( ArrayDim "a" 1 ) ( AlexPn 54 2 37 ) )
                      )
                      ( AlexPn 44 2 27 )
                    )
                  )
                  ( AlexPn 23 2 6 )
                )
                ( Expr
                  ( BinOpExpr EqOp
                    ( Expr ( ArrayDim "c" 2 ) ( AlexPn 65 2 48 ) )
                    ( Expr ( ArrayDim "b" 2 ) ( AlexPn 75 2 58 ) )
                  )
                  ( AlexPn 65 2 48 )
                )
              )
              ( AlexPn 23 2 6 )
            )
            ( Expr
              ( BeginExpr
                ( Expr
                  ( ForExpr "i"
                    ( Expr ( IntCExpr 0 ) ( AlexPn 108 4 13 ) )
                    ( Expr
                      ( BinOpExpr MinusOp
                        ( Expr ( ArrayDim "c" 1 ) ( AlexPn 113 4 18 ) )
                        ( Expr ( IntCExpr 1 ) ( AlexPn 123 4 28 ) )
                      )
                      ( AlexPn 113 4 18 )
                    )
                    ( Expr
                      ( ForExpr "j"
                        ( Expr ( IntCExpr 0 ) ( AlexPn 142 5 15 ) )
                        ( Expr
                          ( BinOpExpr MinusOp
                            ( Expr ( ArrayDim "c" 2 ) ( AlexPn 147 5 20 ) )
                            ( Expr ( IntCExpr 1 ) ( AlexPn 157 5 30 ) )
                          )
                          ( AlexPn 147 5 20 )
                        )
                        ( Expr
                          ( BinOpExpr SemicolonOp
                            ( Expr
                              ( BinOpExpr AssignMutableOp
                                ( Expr
                                  ( ArrayAccess "c"
                                    [ Expr
                                      ( ConstExpr "i" )
                                      ( AlexPn 172 6 11 )
                                    , Expr
                                      ( ConstExpr "j" )
                                      ( AlexPn 175 6 14 )
                                    ]
                                  )
                                  ( AlexPn 170 6 9 )
                                )
                                ( Expr ( IntCExpr 0 ) ( AlexPn 181 6 20 ) )
                              )
                              ( AlexPn 170 6 9 )
                            )
                            ( Expr
                              ( ForExpr "k"
                                ( Expr ( IntCExpr 0 ) ( AlexPn 200 7 17 ) )
                                ( Expr
                                  ( BinOpExpr MinusOp
                                    ( Expr
                                      ( ArrayDim "a" 2 )
                                      ( AlexPn 205 7 22 )
                                    )
                                    ( Expr ( IntCExpr 1 ) ( AlexPn 215 7 32 ) )
                                  )
                                  ( AlexPn 205 7 22 )
                                )
                                ( Expr
                                  ( BinOpExpr AssignMutableOp
                                    ( Expr
                                      ( ArrayAccess "c"
                                        [ Expr
                                          ( ConstExpr "i" )
                                          ( AlexPn 232 8 13 )
                                        , Expr
                                          ( ConstExpr "j" )
                                          ( AlexPn 235 8 16 )
                                        ]
                                      )
                                      ( AlexPn 230 8 11 )
                                    )
                                    ( Expr
                                      ( BinOpExpr PlusOp
                                        ( Expr
                                          ( UnOpExpr BangOp
                                            ( Expr
                                              ( ArrayAccess "c"
                                                [ Expr
                                                  ( ConstExpr "i" )
                                                  ( AlexPn 244 8 25 )
                                                , Expr
                                                  ( ConstExpr "j" )
                                                  ( AlexPn 247 8 28 )
                                                ]
                                              )
                                              ( AlexPn 242 8 23 )
                                            )
                                          )
                                          ( AlexPn 241 8 22 )
                                        )
                                        ( Expr
                                          ( BinOpExpr TimesOp
                                            ( Expr
                                              ( UnOpExpr BangOp
                                                ( Expr
                                                  ( ArrayAccess "a"
                                                    [ Expr
                                                      ( ConstExpr "i" )
                                                      ( AlexPn 255 8 36 )
                                                    , Expr
                                                      ( ConstExpr "k" )
                                                      ( AlexPn 258 8 39 )
                                                    ]
                                                  )
                                                  ( AlexPn 253 8 34 )
                                                )
                                              )
                                              ( AlexPn 252 8 33 )
                                            )
                                            ( Expr
                                              ( UnOpExpr BangOp
                                                ( Expr
                                                  ( ArrayAccess "b"
                                                    [ Expr
                                                      ( ConstExpr "k" )
                                                      ( AlexPn 266 8 47 )
                                                    , Expr
                                                      ( ConstExpr "j" )
                                                      ( AlexPn 269 8 50 )
                                                    ]
                                                  )
                                                  ( AlexPn 264 8 45 )
                                                )
                                              )
                                              ( AlexPn 263 8 44 )
                                            )
                                          )
                                          ( AlexPn 252 8 33 )
                                        )
                                      )
                                      ( AlexPn 241 8 22 )
                                    )
                                  )
                                  ( AlexPn 230 8 11 )
                                )
                              )
                              ( AlexPn 192 7 9 )
                            )
                          )
                          ( AlexPn 170 6 9 )
                        )
                      )
                      ( AlexPn 134 5 7 )
                    )
                  )
                  ( AlexPn 100 4 5 )
                )
              )
              ( AlexPn 90 3 3 )
            )
          )
          ( AlexPn 20 2 3 )
        )
        ( AlexPn 4 1 5 )
      ]
      ( AlexPn 0 1 1 )
    )
  , Left
    ( Let [ VarDef "seed" Nothing ( AlexPn 316 14 5 ) ] ( AlexPn 312 14 1 ) )
  , Left
    ( Let
      [ FunDef "init" [] Nothing
        ( Expr
          ( BinOpExpr AssignMutableOp
            ( Expr ( ConstExpr "seed" ) ( AlexPn 341 16 12 ) )
            ( Expr ( IntCExpr 65 ) ( AlexPn 349 16 20 ) )
          )
          ( AlexPn 341 16 12 )
        )
        ( AlexPn 334 16 5 )
      ]
      ( AlexPn 330 16 1 )
    )
  , Left
    ( Let
      [ FunDef "minit"
        [ Param "m" ( AlexPn 363 18 11 ) ] Nothing
        ( Expr
          ( ForExpr "i"
            ( Expr ( IntCExpr 0 ) ( AlexPn 377 19 11 ) )
            ( Expr
              ( BinOpExpr MinusOp
                ( Expr ( ArrayDim "m" 1 ) ( AlexPn 382 19 16 ) )
                ( Expr ( IntCExpr 1 ) ( AlexPn 392 19 26 ) )
              )
              ( AlexPn 382 19 16 )
            )
            ( Expr
              ( ForExpr "j"
                ( Expr ( IntCExpr 0 ) ( AlexPn 409 20 13 ) )
                ( Expr
                  ( BinOpExpr MinusOp
                    ( Expr ( ArrayDim "m" 2 ) ( AlexPn 414 20 18 ) )
                    ( Expr ( IntCExpr 1 ) ( AlexPn 424 20 28 ) )
                  )
                  ( AlexPn 414 20 18 )
                )
                ( Expr
                  ( BinOpExpr SemicolonOp
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr ( ConstExpr "seed" ) ( AlexPn 435 21 7 ) )
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
                                              ( AlexPn 445 21 17 )
                                            )
                                          )
                                          ( AlexPn 444 21 16 )
                                        )
                                        ( Expr
                                          ( IntCExpr 137 )
                                          ( AlexPn 452 21 24 )
                                        )
                                      )
                                      ( AlexPn 444 21 16 )
                                    )
                                    ( Expr
                                      ( BinOpExpr TimesOp
                                        ( Expr
                                          ( IntCExpr 2 )
                                          ( AlexPn 458 21 30 )
                                        )
                                        ( Expr
                                          ( ConstExpr "i" )
                                          ( AlexPn 460 21 32 )
                                        )
                                      )
                                      ( AlexPn 458 21 30 )
                                    )
                                  )
                                  ( AlexPn 444 21 16 )
                                )
                                ( Expr ( ConstExpr "j" ) ( AlexPn 464 21 36 ) )
                              )
                              ( AlexPn 444 21 16 )
                            )
                            ( Expr ( IntCExpr 101 ) ( AlexPn 471 21 43 ) )
                          )
                          ( AlexPn 443 21 15 )
                        )
                      )
                      ( AlexPn 435 21 7 )
                    )
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr
                          ( ArrayAccess "m"
                            [ Expr
                              ( ConstExpr "i" )
                              ( AlexPn 484 22 9 )
                            , Expr
                              ( ConstExpr "j" )
                              ( AlexPn 487 22 12 )
                            ]
                          )
                          ( AlexPn 482 22 7 )
                        )
                        ( Expr
                          ( UnOpExpr BangOp
                            ( Expr ( ConstExpr "seed" ) ( AlexPn 494 22 19 ) )
                          )
                          ( AlexPn 493 22 18 )
                        )
                      )
                      ( AlexPn 482 22 7 )
                    )
                  )
                  ( AlexPn 435 21 7 )
                )
              )
              ( AlexPn 401 20 5 )
            )
          )
          ( AlexPn 369 19 3 )
        )
        ( AlexPn 357 18 5 )
      ]
      ( AlexPn 353 18 1 )
    )
  , Left
    ( Let
      [ FunDef "mprint"
        [ Param "m" ( AlexPn 527 26 12 ) ] Nothing
        ( Expr
          ( ForExpr "i"
            ( Expr ( IntCExpr 0 ) ( AlexPn 541 27 11 ) )
            ( Expr
              ( BinOpExpr MinusOp
                ( Expr ( ArrayDim "m" 1 ) ( AlexPn 546 27 16 ) )
                ( Expr ( IntCExpr 1 ) ( AlexPn 556 27 26 ) )
              )
              ( AlexPn 546 27 16 )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( ForExpr "j"
                    ( Expr ( IntCExpr 0 ) ( AlexPn 573 28 13 ) )
                    ( Expr
                      ( BinOpExpr MinusOp
                        ( Expr ( ArrayDim "m" 2 ) ( AlexPn 578 28 18 ) )
                        ( Expr ( IntCExpr 1 ) ( AlexPn 588 28 28 ) )
                      )
                      ( AlexPn 578 28 18 )
                    )
                    ( Expr
                      ( BinOpExpr SemicolonOp
                        ( Expr
                          ( FunAppExpr "print_int"
                            [ Expr
                              ( UnOpExpr BangOp
                                ( Expr
                                  ( ArrayAccess "m"
                                    [ Expr
                                      ( ConstExpr "i" )
                                      ( AlexPn 612 29 20 )
                                    , Expr
                                      ( ConstExpr "j" )
                                      ( AlexPn 615 29 23 )
                                    ]
                                  )
                                  ( AlexPn 610 29 18 )
                                )
                              )
                              ( AlexPn 609 29 17 )
                            ]
                          )
                          ( AlexPn 599 29 7 )
                        )
                        ( Expr
                          ( FunAppExpr "print_string"
                            [ Expr ( StringCExpr " " ) ( AlexPn 638 30 20 ) ]
                          )
                          ( AlexPn 625 30 7 )
                        )
                      )
                      ( AlexPn 599 29 7 )
                    )
                  )
                  ( AlexPn 565 28 5 )
                )
                ( Expr
                  ( FunAppExpr "print_string"
                    [ Expr ( StringCExpr "\n" ) ( AlexPn 669 32 18 ) ]
                  )
                  ( AlexPn 656 32 5 )
                )
              )
              ( AlexPn 565 28 5 )
            )
          )
          ( AlexPn 533 27 3 )
        )
        ( AlexPn 520 26 5 )
      ]
      ( AlexPn 516 26 1 )
    )
  , Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ ArrayDef "x"
              [ Expr
                ( IntCExpr 3 )
                ( AlexPn 709 36 17 )
              , Expr
                ( IntCExpr 4 )
                ( AlexPn 711 36 19 )
              ] Nothing
              ( AlexPn 699 36 7 )
            , ArrayDef "y"
              [ Expr
                ( IntCExpr 4 )
                ( AlexPn 730 37 17 )
              , Expr
                ( IntCExpr 5 )
                ( AlexPn 732 37 19 )
              ] Nothing
              ( AlexPn 720 37 7 )
            , ArrayDef "z"
              [ Expr
                ( IntCExpr 3 )
                ( AlexPn 751 38 17 )
              , Expr
                ( IntCExpr 5 )
                ( AlexPn 753 38 19 )
              ] Nothing
              ( AlexPn 741 38 7 )
            ]
            ( AlexPn 695 36 3 )
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
                            ( BinOpExpr SemicolonOp
                              ( Expr
                                ( BinOpExpr SemicolonOp
                                  ( Expr
                                    ( BinOpExpr SemicolonOp
                                      ( Expr
                                        ( FunAppExpr "minit"
                                          [ Expr
                                            ( ConstExpr "x" )
                                            ( AlexPn 768 40 9 )
                                          ]
                                        )
                                        ( AlexPn 762 40 3 )
                                      )
                                      ( Expr
                                        ( FunAppExpr "minit"
                                          [ Expr
                                            ( ConstExpr "y" )
                                            ( AlexPn 779 41 9 )
                                          ]
                                        )
                                        ( AlexPn 773 41 3 )
                                      )
                                    )
                                    ( AlexPn 762 40 3 )
                                  )
                                  ( Expr
                                    ( FunAppExpr "mprint"
                                      [ Expr
                                        ( ConstExpr "x" )
                                        ( AlexPn 792 43 10 )
                                      ]
                                    )
                                    ( AlexPn 785 43 3 )
                                  )
                                )
                                ( AlexPn 762 40 3 )
                              )
                              ( Expr
                                ( FunAppExpr "print_string"
                                  [ Expr
                                    ( StringCExpr "\ntimes\n\n" )
                                    ( AlexPn 810 44 16 )
                                  ]
                                )
                                ( AlexPn 797 44 3 )
                              )
                            )
                            ( AlexPn 762 40 3 )
                          )
                          ( Expr
                            ( FunAppExpr "mprint"
                              [ Expr ( ConstExpr "y" ) ( AlexPn 834 45 10 ) ]
                            )
                            ( AlexPn 827 45 3 )
                          )
                        )
                        ( AlexPn 762 40 3 )
                      )
                      ( Expr
                        ( FunAppExpr "print_string"
                          [ Expr
                            ( StringCExpr "\nmakes\n\n" )
                            ( AlexPn 852 46 16 )
                          ]
                        )
                        ( AlexPn 839 46 3 )
                      )
                    )
                    ( AlexPn 762 40 3 )
                  )
                  ( Expr
                    ( FunAppExpr "mmult"
                      [ Expr
                        ( ConstExpr "x" )
                        ( AlexPn 875 47 9 )
                      , Expr
                        ( ConstExpr "y" )
                        ( AlexPn 877 47 11 )
                      , Expr
                        ( ConstExpr "z" )
                        ( AlexPn 879 47 13 )
                      ]
                    )
                    ( AlexPn 869 47 3 )
                  )
                )
                ( AlexPn 762 40 3 )
              )
              ( Expr
                ( FunAppExpr "mprint"
                  [ Expr ( ConstExpr "z" ) ( AlexPn 891 48 10 ) ]
                )
                ( AlexPn 884 48 3 )
              )
            )
            ( AlexPn 762 40 3 )
          )
          ( AlexPn 695 36 3 )
        )
        ( AlexPn 686 35 5 )
      ]
      ( AlexPn 682 35 1 )
    )
  ]
  ( AlexPn 0 1 1 )
