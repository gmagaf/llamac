module Unit.Parser.Hanoi (hanoiAST) where

import Common.AST
import Lexer.Lexer (AlexPosn(AlexPn))

hanoiAST :: AST AlexPosn
hanoiAST = AST
  [ Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ FunDef "move"
              [ Param "source"
                ( AlexPn 22 2 12 )
              , Param "target"
                ( AlexPn 29 2 19 )
              ] Nothing
              ( Expr
                ( BinOpExpr SemicolonOp
                  ( Expr
                    ( BinOpExpr SemicolonOp
                      ( Expr
                        ( BinOpExpr SemicolonOp
                          ( Expr
                            ( BinOpExpr SemicolonOp
                              ( Expr
                                ( FunAppExpr "print_string"
                                  [ Expr
                                    ( StringCExpr "Moving from: " )
                                    ( AlexPn 55 3 18 )
                                  ]
                                )
                                ( AlexPn 42 3 5 )
                              )
                              ( Expr
                                ( FunAppExpr "print_string"
                                  [ Expr
                                    ( ConstExpr "source" )
                                    ( AlexPn 89 4 18 )
                                  ]
                                )
                                ( AlexPn 76 4 5 )
                              )
                            )
                            ( AlexPn 42 3 5 )
                          )
                          ( Expr
                            ( FunAppExpr "print_string"
                              [ Expr
                                ( StringCExpr " to " )
                                ( AlexPn 114 5 18 )
                              ]
                            )
                            ( AlexPn 101 5 5 )
                          )
                        )
                        ( AlexPn 42 3 5 )
                      )
                      ( Expr
                        ( FunAppExpr "print_string"
                          [ Expr ( ConstExpr "target" ) ( AlexPn 139 6 18 ) ]
                        )
                        ( AlexPn 126 6 5 )
                      )
                    )
                    ( AlexPn 42 3 5 )
                  )
                  ( Expr
                    ( FunAppExpr "print_string"
                      [ Expr ( StringCExpr "\n" ) ( AlexPn 164 7 18 ) ]
                    )
                    ( AlexPn 151 7 5 )
                  )
                )
                ( AlexPn 42 3 5 )
              )
              ( AlexPn 17 2 7 )
            ]
            ( AlexPn 13 2 3 )
          )
          ( LetIn
            ( LetRec
              [ FunDef "hanoi"
                [ Param "rings"
                  ( AlexPn 188 8 17 )
                , Param "source"
                  ( AlexPn 194 8 23 )
                , Param "target"
                  ( AlexPn 201 8 30 )
                , Param "auxil"
                  ( AlexPn 208 8 37 )
                ] Nothing
                ( Expr
                  ( IfThenExpr
                    ( Expr
                      ( BinOpExpr GTOp
                        ( Expr ( ConstExpr "rings" ) ( AlexPn 223 9 8 ) )
                        ( Expr ( IntCExpr 0 ) ( AlexPn 231 9 16 ) )
                      )
                      ( AlexPn 223 9 8 )
                    )
                    ( Expr
                      ( BeginExpr
                        ( Expr
                          ( BinOpExpr SemicolonOp
                            ( Expr
                              ( BinOpExpr SemicolonOp
                                ( Expr
                                  ( FunAppExpr "hanoi"
                                    [ Expr
                                      ( BinOpExpr MinusOp
                                        ( Expr
                                          ( ConstExpr "rings" )
                                          ( AlexPn 261 11 14 )
                                        )
                                        ( Expr
                                          ( IntCExpr 1 )
                                          ( AlexPn 267 11 20 )
                                        )
                                      )
                                      ( AlexPn 261 11 14 )
                                    , Expr
                                      ( ConstExpr "source" )
                                      ( AlexPn 270 11 23 )
                                    , Expr
                                      ( ConstExpr "auxil" )
                                      ( AlexPn 277 11 30 )
                                    , Expr
                                      ( ConstExpr "target" )
                                      ( AlexPn 283 11 36 )
                                    ]
                                  )
                                  ( AlexPn 254 11 7 )
                                )
                                ( Expr
                                  ( FunAppExpr "move"
                                    [ Expr
                                      ( ConstExpr "source" )
                                      ( AlexPn 302 12 12 )
                                    , Expr
                                      ( ConstExpr "target" )
                                      ( AlexPn 309 12 19 )
                                    ]
                                  )
                                  ( AlexPn 297 12 7 )
                                )
                              )
                              ( AlexPn 254 11 7 )
                            )
                            ( Expr
                              ( FunAppExpr "hanoi"
                                [ Expr
                                  ( BinOpExpr MinusOp
                                    ( Expr
                                      ( ConstExpr "rings" )
                                      ( AlexPn 330 13 14 )
                                    )
                                    ( Expr ( IntCExpr 1 ) ( AlexPn 336 13 20 ) )
                                  )
                                  ( AlexPn 330 13 14 )
                                , Expr
                                  ( ConstExpr "auxil" )
                                  ( AlexPn 339 13 23 )
                                , Expr
                                  ( ConstExpr "target" )
                                  ( AlexPn 345 13 29 )
                                , Expr
                                  ( ConstExpr "source" )
                                  ( AlexPn 352 13 36 )
                                ]
                              )
                              ( AlexPn 323 13 7 )
                            )
                          )
                          ( AlexPn 254 11 7 )
                        )
                      )
                      ( AlexPn 242 10 5 )
                    )
                  )
                  ( AlexPn 220 9 5 )
                )
                ( AlexPn 182 8 11 )
              ]
              ( AlexPn 174 8 3 )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( FunAppExpr "print_string"
                    [ Expr
                      ( StringCExpr "Please, give the number of rings: " )
                      ( AlexPn 385 15 16 )
                    ]
                  )
                  ( AlexPn 372 15 3 )
                )
                ( LetIn
                  ( Let
                    [ FunDef "n" [] Nothing
                      ( Expr
                        ( FunAppExpr "read_int"
                          [ Expr UnitCExpr ( AlexPn 442 16 20 ) ]
                        )
                        ( AlexPn 433 16 11 )
                      )
                      ( AlexPn 429 16 7 )
                    ]
                    ( AlexPn 425 16 3 )
                  )
                  ( Expr
                    ( FunAppExpr "hanoi"
                      [ Expr
                        ( ConstExpr "n" )
                        ( AlexPn 456 17 9 )
                      , Expr
                        ( StringCExpr "left" )
                        ( AlexPn 458 17 11 )
                      , Expr
                        ( StringCExpr "right" )
                        ( AlexPn 465 17 18 )
                      , Expr
                        ( StringCExpr "middle" )
                        ( AlexPn 473 17 26 )
                      ]
                    )
                    ( AlexPn 450 17 3 )
                  )
                  ( AlexPn 425 16 3 )
                )
              )
              ( AlexPn 372 15 3 )
            )
            ( AlexPn 174 8 3 )
          )
          ( AlexPn 13 2 3 )
        )
        ( AlexPn 4 1 5 )
      ]
      ( AlexPn 0 1 1 )
    )
  ]
  ( AlexPn 0 1 1 )
