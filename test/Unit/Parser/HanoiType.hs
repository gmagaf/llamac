module Unit.Parser.HanoiType (hanoiTypeAST) where

import Common.AST
import Lexer.Lexer (AlexPosn(AlexPn))

hanoiTypeAST :: AST AlexPosn
hanoiTypeAST = AST
  [ Right
    ( TypeDef
      [ TDef "pile"
        [ Constr "Left" []
          ( AlexPn 12 1 13 )
        , Constr "Middle" []
          ( AlexPn 19 1 20 )
        , Constr "Right" []
          ( AlexPn 28 1 29 )
        ]
        ( AlexPn 5 1 6 )
      ]
      ( AlexPn 0 1 1 )
    )
  , Left
    ( Let
      [ FunDef "print_pile"
        [ Param "pile" ( AlexPn 50 3 16 ) ] Nothing
        ( MatchExpr
          ( Expr ( ConstExpr "pile" ) ( AlexPn 65 4 9 ) )
          [ Match
            ( Pattern ( ConstrPattern "Left" [] ) ( AlexPn 79 5 5 ) )
            ( Expr
              ( FunAppExpr "print_string"
                [ Expr ( StringCExpr "left" ) ( AlexPn 102 5 28 ) ]
              )
              ( AlexPn 89 5 15 )
            )
            ( AlexPn 79 5 5 )
          , Match
            ( Pattern ( ConstrPattern "Middle" [] ) ( AlexPn 113 6 5 ) )
            ( Expr
              ( FunAppExpr "print_string"
                [ Expr ( StringCExpr "middle" ) ( AlexPn 136 6 28 ) ]
              )
              ( AlexPn 123 6 15 )
            )
            ( AlexPn 113 6 5 )
          , Match
            ( Pattern ( ConstrPattern "Right" [] ) ( AlexPn 149 7 5 ) )
            ( Expr
              ( FunAppExpr "print_string"
                [ Expr ( StringCExpr "right" ) ( AlexPn 172 7 28 ) ]
              )
              ( AlexPn 159 7 15 )
            )
            ( AlexPn 149 7 5 )
          ]
          ( AlexPn 59 4 3 )
        )
        ( AlexPn 39 3 5 )
      ]
      ( AlexPn 35 3 1 )
    )
  , Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ FunDef "move"
              [ Param "source"
                ( AlexPn 209 11 12 )
              , Param "target"
                ( AlexPn 216 11 19 )
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
                                    ( AlexPn 242 12 18 )
                                  ]
                                )
                                ( AlexPn 229 12 5 )
                              )
                              ( Expr
                                ( FunAppExpr "print_pile"
                                  [ Expr
                                    ( ConstExpr "source" )
                                    ( AlexPn 274 13 16 )
                                  ]
                                )
                                ( AlexPn 263 13 5 )
                              )
                            )
                            ( AlexPn 229 12 5 )
                          )
                          ( Expr
                            ( FunAppExpr "print_string"
                              [ Expr
                                ( StringCExpr " to " )
                                ( AlexPn 299 14 18 )
                              ]
                            )
                            ( AlexPn 286 14 5 )
                          )
                        )
                        ( AlexPn 229 12 5 )
                      )
                      ( Expr
                        ( FunAppExpr "print_pile"
                          [ Expr ( ConstExpr "target" ) ( AlexPn 322 15 16 ) ]
                        )
                        ( AlexPn 311 15 5 )
                      )
                    )
                    ( AlexPn 229 12 5 )
                  )
                  ( Expr
                    ( FunAppExpr "print_string"
                      [ Expr ( StringCExpr "\n" ) ( AlexPn 347 16 18 ) ]
                    )
                    ( AlexPn 334 16 5 )
                  )
                )
                ( AlexPn 229 12 5 )
              )
              ( AlexPn 204 11 7 )
            ]
            ( AlexPn 200 11 3 )
          )
          ( LetIn
            ( LetRec
              [ FunDef "hanoi"
                [ Param "rings"
                  ( AlexPn 371 17 17 )
                , Param "source"
                  ( AlexPn 377 17 23 )
                , Param "target"
                  ( AlexPn 384 17 30 )
                , Param "auxil"
                  ( AlexPn 391 17 37 )
                ] Nothing
                ( Expr
                  ( IfThenExpr
                    ( Expr
                      ( BinOpExpr GTOp
                        ( Expr ( ConstExpr "rings" ) ( AlexPn 406 18 8 ) )
                        ( Expr ( IntCExpr 0 ) ( AlexPn 414 18 16 ) )
                      )
                      ( AlexPn 406 18 8 )
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
                                          ( AlexPn 444 20 14 )
                                        )
                                        ( Expr
                                          ( IntCExpr 1 )
                                          ( AlexPn 450 20 20 )
                                        )
                                      )
                                      ( AlexPn 444 20 14 )
                                    , Expr
                                      ( ConstExpr "source" )
                                      ( AlexPn 453 20 23 )
                                    , Expr
                                      ( ConstExpr "auxil" )
                                      ( AlexPn 460 20 30 )
                                    , Expr
                                      ( ConstExpr "target" )
                                      ( AlexPn 466 20 36 )
                                    ]
                                  )
                                  ( AlexPn 437 20 7 )
                                )
                                ( Expr
                                  ( FunAppExpr "move"
                                    [ Expr
                                      ( ConstExpr "source" )
                                      ( AlexPn 485 21 12 )
                                    , Expr
                                      ( ConstExpr "target" )
                                      ( AlexPn 492 21 19 )
                                    ]
                                  )
                                  ( AlexPn 480 21 7 )
                                )
                              )
                              ( AlexPn 437 20 7 )
                            )
                            ( Expr
                              ( FunAppExpr "hanoi"
                                [ Expr
                                  ( BinOpExpr MinusOp
                                    ( Expr
                                      ( ConstExpr "rings" )
                                      ( AlexPn 513 22 14 )
                                    )
                                    ( Expr ( IntCExpr 1 ) ( AlexPn 519 22 20 ) )
                                  )
                                  ( AlexPn 513 22 14 )
                                , Expr
                                  ( ConstExpr "auxil" )
                                  ( AlexPn 522 22 23 )
                                , Expr
                                  ( ConstExpr "target" )
                                  ( AlexPn 528 22 29 )
                                , Expr
                                  ( ConstExpr "source" )
                                  ( AlexPn 535 22 36 )
                                ]
                              )
                              ( AlexPn 506 22 7 )
                            )
                          )
                          ( AlexPn 437 20 7 )
                        )
                      )
                      ( AlexPn 425 19 5 )
                    )
                  )
                  ( AlexPn 403 18 5 )
                )
                ( AlexPn 365 17 11 )
              ]
              ( AlexPn 357 17 3 )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( FunAppExpr "print_string"
                    [ Expr
                      ( StringCExpr "Please, give the number of rings: " )
                      ( AlexPn 568 24 16 )
                    ]
                  )
                  ( AlexPn 555 24 3 )
                )
                ( LetIn
                  ( Let
                    [ FunDef "n" [] Nothing
                      ( Expr
                        ( FunAppExpr "read_int"
                          [ Expr UnitCExpr ( AlexPn 625 25 20 ) ]
                        )
                        ( AlexPn 616 25 11 )
                      )
                      ( AlexPn 612 25 7 )
                    ]
                    ( AlexPn 608 25 3 )
                  )
                  ( Expr
                    ( FunAppExpr "hanoi"
                      [ Expr
                        ( ConstExpr "n" )
                        ( AlexPn 639 26 9 )
                      , Expr
                        ( ConstConstrExpr "Left" )
                        ( AlexPn 641 26 11 )
                      , Expr
                        ( ConstConstrExpr "Right" )
                        ( AlexPn 646 26 16 )
                      , Expr
                        ( ConstConstrExpr "Middle" )
                        ( AlexPn 652 26 22 )
                      ]
                    )
                    ( AlexPn 633 26 3 )
                  )
                  ( AlexPn 608 25 3 )
                )
              )
              ( AlexPn 555 24 3 )
            )
            ( AlexPn 357 17 3 )
          )
          ( AlexPn 200 11 3 )
        )
        ( AlexPn 191 10 5 )
      ]
      ( AlexPn 187 10 1 )
    )
  ]
  ( AlexPn 0 1 1 )
