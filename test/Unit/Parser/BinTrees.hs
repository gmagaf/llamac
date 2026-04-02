module Unit.Parser.BinTrees (binTreesAST) where

import Common.AST
import Lexer.Lexer (AlexPosn(AlexPn))

binTreesAST :: AST AlexPosn
binTreesAST = AST
  [ Right
    ( TypeDef
      [ TDef "tree"
        [ Constr "Nil" []
          ( AlexPn 12 1 13 )
        , Constr "Node"
          [ Type IntType
            ( AlexPn 26 1 27 )
          , Type
            ( UserDefinedType "tree" )
            ( AlexPn 30 1 31 )
          , Type
            ( UserDefinedType "tree" )
            ( AlexPn 35 1 36 )
          ]
          ( AlexPn 18 1 19 )
        ]
        ( AlexPn 5 1 6 )
      ]
      ( AlexPn 0 1 1 )
    )
  , Left
    ( LetRec
      [ FunDef "treeInsert"
        [ Param "t" ( AlexPn 60 3 20 ), Param "n" ( AlexPn 62 3 22 ) ] Nothing
        ( MatchExpr
          ( Expr ( ConstExpr "t" ) ( AlexPn 74 4 9 ) )
          [ Match
            ( Pattern ( ConstrPattern "Nil" [] ) ( AlexPn 87 5 7 ) )
            ( Expr
              ( ConstrAppExpr "Node"
                [ Expr
                  ( ConstExpr "n" )
                  ( AlexPn 108 5 28 )
                , Expr
                  ( ConstConstrExpr "Nil" )
                  ( AlexPn 110 5 30 )
                , Expr
                  ( ConstConstrExpr "Nil" )
                  ( AlexPn 114 5 34 )
                ]
              )
              ( AlexPn 103 5 23 )
            )
            ( AlexPn 87 5 7 )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "m" )
                  ( AlexPn 129 6 12 )
                , Pattern
                  ( IdPattern "t1" )
                  ( AlexPn 131 6 14 )
                , Pattern
                  ( IdPattern "t2" )
                  ( AlexPn 134 6 17 )
                ]
              )
              ( AlexPn 124 6 7 )
            )
            ( Expr
              ( IfThenElseExpr
                ( Expr
                  ( BinOpExpr LTOp
                    ( Expr ( ConstExpr "n" ) ( AlexPn 148 6 31 ) )
                    ( Expr ( ConstExpr "m" ) ( AlexPn 152 6 35 ) )
                  )
                  ( AlexPn 148 6 31 )
                )
                ( Expr
                  ( ConstrAppExpr "Node"
                    [ Expr
                      ( ConstExpr "m" )
                      ( AlexPn 164 6 47 )
                    , Expr
                      ( FunAppExpr "treeInsert"
                        [ Expr
                          ( ConstExpr "t1" )
                          ( AlexPn 178 6 61 )
                        , Expr
                          ( ConstExpr "n" )
                          ( AlexPn 181 6 64 )
                        ]
                      )
                      ( AlexPn 167 6 50 )
                    , Expr
                      ( ConstExpr "t2" )
                      ( AlexPn 184 6 67 )
                    ]
                  )
                  ( AlexPn 159 6 42 )
                )
                ( Expr
                  ( IfThenElseExpr
                    ( Expr
                      ( BinOpExpr GTOp
                        ( Expr ( ConstExpr "n" ) ( AlexPn 217 7 31 ) )
                        ( Expr ( ConstExpr "m" ) ( AlexPn 221 7 35 ) )
                      )
                      ( AlexPn 217 7 31 )
                    )
                    ( Expr
                      ( ConstrAppExpr "Node"
                        [ Expr
                          ( ConstExpr "m" )
                          ( AlexPn 233 7 47 )
                        , Expr
                          ( ConstExpr "t1" )
                          ( AlexPn 235 7 49 )
                        , Expr
                          ( FunAppExpr "treeInsert"
                            [ Expr
                              ( ConstExpr "t2" )
                              ( AlexPn 250 7 64 )
                            , Expr
                              ( ConstExpr "n" )
                              ( AlexPn 253 7 67 )
                            ]
                          )
                          ( AlexPn 239 7 53 )
                        ]
                      )
                      ( AlexPn 228 7 42 )
                    )
                    ( Expr ( ConstExpr "t" ) ( AlexPn 283 8 28 ) )
                  )
                  ( AlexPn 214 7 28 )
                )
              )
              ( AlexPn 145 6 28 )
            )
            ( AlexPn 124 6 7 )
          ]
          ( AlexPn 68 4 3 )
        )
        ( AlexPn 49 3 9 )
      ]
      ( AlexPn 41 3 1 )
    )
  , Left
    ( LetRec
      [ FunDef "treeMerge"
        [ Param "t1"
          ( AlexPn 310 11 19 )
        , Param "t2"
          ( AlexPn 313 11 22 )
        ] Nothing
        ( MatchExpr
          ( Expr ( ConstExpr "t1" ) ( AlexPn 326 12 9 ) )
          [ Match
            ( Pattern ( ConstrPattern "Nil" [] ) ( AlexPn 340 13 7 ) )
            ( Expr ( ConstExpr "t2" ) ( AlexPn 348 13 15 ) )
            ( AlexPn 340 13 7 )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "n" )
                  ( AlexPn 362 14 12 )
                , Pattern
                  ( IdPattern "t11" )
                  ( AlexPn 364 14 14 )
                , Pattern
                  ( IdPattern "t12" )
                  ( AlexPn 368 14 18 )
                ]
              )
              ( AlexPn 357 14 7 )
            )
            ( Expr
              ( ConstrAppExpr "Node"
                [ Expr
                  ( ConstExpr "n" )
                  ( AlexPn 380 14 30 )
                , Expr
                  ( ConstExpr "t11" )
                  ( AlexPn 382 14 32 )
                , Expr
                  ( FunAppExpr "treeMerge"
                    [ Expr
                      ( ConstExpr "t12" )
                      ( AlexPn 397 14 47 )
                    , Expr
                      ( ConstExpr "t2" )
                      ( AlexPn 401 14 51 )
                    ]
                  )
                  ( AlexPn 387 14 37 )
                ]
              )
              ( AlexPn 375 14 25 )
            )
            ( AlexPn 357 14 7 )
          ]
          ( AlexPn 320 12 3 )
        )
        ( AlexPn 300 11 9 )
      ]
      ( AlexPn 292 11 1 )
    )
  , Left
    ( LetRec
      [ FunDef "treeDelete"
        [ Param "t"
          ( AlexPn 431 17 20 )
        , Param "n"
          ( AlexPn 433 17 22 )
        ] Nothing
        ( MatchExpr
          ( Expr ( ConstExpr "t" ) ( AlexPn 445 18 9 ) )
          [ Match
            ( Pattern ( ConstrPattern "Nil" [] ) ( AlexPn 458 19 7 ) )
            ( Expr ( ConstExpr "t" ) ( AlexPn 468 19 17 ) )
            ( AlexPn 458 19 7 )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "m" )
                  ( AlexPn 481 20 12 )
                , Pattern
                  ( IdPattern "t1" )
                  ( AlexPn 483 20 14 )
                , Pattern
                  ( IdPattern "t2" )
                  ( AlexPn 486 20 17 )
                ]
              )
              ( AlexPn 476 20 7 )
            )
            ( Expr
              ( IfThenElseExpr
                ( Expr
                  ( BinOpExpr LTOp
                    ( Expr ( ConstExpr "n" ) ( AlexPn 495 20 26 ) )
                    ( Expr ( ConstExpr "m" ) ( AlexPn 499 20 30 ) )
                  )
                  ( AlexPn 495 20 26 )
                )
                ( Expr
                  ( ConstrAppExpr "Node"
                    [ Expr
                      ( ConstExpr "m" )
                      ( AlexPn 535 21 30 )
                    , Expr
                      ( FunAppExpr "treeDelete"
                        [ Expr
                          ( ConstExpr "t1" )
                          ( AlexPn 549 21 44 )
                        , Expr
                          ( ConstExpr "n" )
                          ( AlexPn 552 21 47 )
                        ]
                      )
                      ( AlexPn 538 21 33 )
                    , Expr
                      ( ConstExpr "t2" )
                      ( AlexPn 555 21 50 )
                    ]
                  )
                  ( AlexPn 530 21 25 )
                )
                ( Expr
                  ( IfThenElseExpr
                    ( Expr
                      ( BinOpExpr GTOp
                        ( Expr ( ConstExpr "n" ) ( AlexPn 588 22 31 ) )
                        ( Expr ( ConstExpr "m" ) ( AlexPn 592 22 35 ) )
                      )
                      ( AlexPn 588 22 31 )
                    )
                    ( Expr
                      ( ConstrAppExpr "Node"
                        [ Expr
                          ( ConstExpr "m" )
                          ( AlexPn 628 23 30 )
                        , Expr
                          ( ConstExpr "t1" )
                          ( AlexPn 630 23 32 )
                        , Expr
                          ( FunAppExpr "treeDelete"
                            [ Expr
                              ( ConstExpr "t2" )
                              ( AlexPn 645 23 47 )
                            , Expr
                              ( ConstExpr "n" )
                              ( AlexPn 648 23 50 )
                            ]
                          )
                          ( AlexPn 634 23 36 )
                        ]
                      )
                      ( AlexPn 623 23 25 )
                    )
                    ( Expr
                      ( FunAppExpr "treeMerge"
                        [ Expr
                          ( ConstExpr "t1" )
                          ( AlexPn 712 25 35 )
                        , Expr
                          ( ConstExpr "t2" )
                          ( AlexPn 715 25 38 )
                        ]
                      )
                      ( AlexPn 702 25 25 )
                    )
                  )
                  ( AlexPn 585 22 28 )
                )
              )
              ( AlexPn 492 20 23 )
            )
            ( AlexPn 476 20 7 )
          ]
          ( AlexPn 439 18 3 )
        )
        ( AlexPn 420 17 9 )
      ]
      ( AlexPn 412 17 1 )
    )
  , Left
    ( LetRec
      [ FunDef "treePrint"
        [ Param "t" ( AlexPn 743 28 19 ) ] Nothing
        ( MatchExpr
          ( Expr ( ConstExpr "t" ) ( AlexPn 755 29 9 ) )
          [ Match
            ( Pattern ( ConstrPattern "Nil" [] ) ( AlexPn 768 30 7 ) )
            ( Expr UnitCExpr ( AlexPn 776 30 15 ) )
            ( AlexPn 768 30 7 )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "n" )
                  ( AlexPn 790 31 12 )
                , Pattern
                  ( IdPattern "t1" )
                  ( AlexPn 792 31 14 )
                , Pattern
                  ( IdPattern "t2" )
                  ( AlexPn 795 31 17 )
                ]
              )
              ( AlexPn 785 31 7 )
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
                                  ( FunAppExpr "print_int"
                                    [ Expr
                                      ( ConstExpr "n" )
                                      ( AlexPn 811 31 33 )
                                    ]
                                  )
                                  ( AlexPn 801 31 23 )
                                )
                                ( Expr
                                  ( FunAppExpr "print_string"
                                    [ Expr
                                      ( StringCExpr "(" )
                                      ( AlexPn 849 32 36 )
                                    ]
                                  )
                                  ( AlexPn 836 32 23 )
                                )
                              )
                              ( AlexPn 801 31 23 )
                            )
                            ( Expr
                              ( FunAppExpr "treePrint"
                                [ Expr ( ConstExpr "t1" ) ( AlexPn 886 33 33 ) ]
                              )
                              ( AlexPn 876 33 23 )
                            )
                          )
                          ( AlexPn 801 31 23 )
                        )
                        ( Expr
                          ( FunAppExpr "print_string"
                            [ Expr ( StringCExpr "|" ) ( AlexPn 925 34 36 ) ]
                          )
                          ( AlexPn 912 34 23 )
                        )
                      )
                      ( AlexPn 801 31 23 )
                    )
                    ( Expr
                      ( FunAppExpr "treePrint"
                        [ Expr ( ConstExpr "t2" ) ( AlexPn 962 35 33 ) ]
                      )
                      ( AlexPn 952 35 23 )
                    )
                  )
                  ( AlexPn 801 31 23 )
                )
                ( Expr
                  ( FunAppExpr "print_string"
                    [ Expr ( StringCExpr ")" ) ( AlexPn 1001 36 36 ) ]
                  )
                  ( AlexPn 988 36 23 )
                )
              )
              ( AlexPn 801 31 23 )
            )
            ( AlexPn 785 31 7 )
          ]
          ( AlexPn 749 29 3 )
        )
        ( AlexPn 733 28 9 )
      ]
      ( AlexPn 725 28 1 )
    )
  , Left
    ( LetRec
      [ FunDef "treeCount"
        [ Param "t" ( AlexPn 1030 39 19 ) ] Nothing
        ( MatchExpr
          ( Expr ( ConstExpr "t" ) ( AlexPn 1042 40 9 ) )
          [ Match
            ( Pattern ( ConstrPattern "Nil" [] ) ( AlexPn 1055 41 7 ) )
            ( Expr ( IntCExpr 0 ) ( AlexPn 1065 41 17 ) )
            ( AlexPn 1055 41 7 )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "n" )
                  ( AlexPn 1078 42 12 )
                , Pattern
                  ( IdPattern "t1" )
                  ( AlexPn 1080 42 14 )
                , Pattern
                  ( IdPattern "t2" )
                  ( AlexPn 1083 42 17 )
                ]
              )
              ( AlexPn 1073 42 7 )
            )
            ( Expr
              ( BinOpExpr PlusOp
                ( Expr
                  ( BinOpExpr PlusOp
                    ( Expr ( IntCExpr 1 ) ( AlexPn 1089 42 23 ) )
                    ( Expr
                      ( FunAppExpr "treeCount"
                        [ Expr ( ConstExpr "t1" ) ( AlexPn 1103 42 37 ) ]
                      )
                      ( AlexPn 1093 42 27 )
                    )
                  )
                  ( AlexPn 1089 42 23 )
                )
                ( Expr
                  ( FunAppExpr "treeCount"
                    [ Expr ( ConstExpr "t2" ) ( AlexPn 1118 42 52 ) ]
                  )
                  ( AlexPn 1108 42 42 )
                )
              )
              ( AlexPn 1089 42 23 )
            )
            ( AlexPn 1073 42 7 )
          ]
          ( AlexPn 1036 40 3 )
        )
        ( AlexPn 1020 39 9 )
      ]
      ( AlexPn 1012 39 1 )
    )
  , Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ VarDef "seed" Nothing ( AlexPn 1146 47 7 ) ]
            ( AlexPn 1142 47 3 )
          )
          ( LetIn
            ( Let
              [ FunDef "next"
                [ Param "u" ( AlexPn 1173 48 12 ) ] Nothing
                ( Expr
                  ( BinOpExpr SemicolonOp
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr ( ConstExpr "seed" ) ( AlexPn 1181 49 5 ) )
                        ( Expr
                          ( BinOpExpr ModOp
                            ( Expr
                              ( BinOpExpr PlusOp
                                ( Expr
                                  ( BinOpExpr TimesOp
                                    ( Expr
                                      ( UnOpExpr BangOp
                                        ( Expr
                                          ( ConstExpr "seed" )
                                          ( AlexPn 1191 49 15 )
                                        )
                                      )
                                      ( AlexPn 1190 49 14 )
                                    )
                                    ( Expr
                                      ( IntCExpr 4241 )
                                      ( AlexPn 1198 49 22 )
                                    )
                                  )
                                  ( AlexPn 1190 49 14 )
                                )
                                ( Expr ( IntCExpr 22 ) ( AlexPn 1205 49 29 ) )
                              )
                              ( AlexPn 1190 49 14 )
                            )
                            ( Expr ( IntCExpr 9949 ) ( AlexPn 1213 49 37 ) )
                          )
                          ( AlexPn 1189 49 13 )
                        )
                      )
                      ( AlexPn 1181 49 5 )
                    )
                    ( Expr
                      ( UnOpExpr BangOp
                        ( Expr ( ConstExpr "seed" ) ( AlexPn 1224 50 6 ) )
                      )
                      ( AlexPn 1223 50 5 )
                    )
                  )
                  ( AlexPn 1181 49 5 )
                )
                ( AlexPn 1168 48 7 )
              ]
              ( AlexPn 1164 48 3 )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( BinOpExpr AssignMutableOp
                    ( Expr ( ConstExpr "seed" ) ( AlexPn 1234 51 3 ) )
                    ( Expr ( IntCExpr 65 ) ( AlexPn 1242 51 11 ) )
                  )
                  ( AlexPn 1234 51 3 )
                )
                ( LetIn
                  ( Let
                    [ FunDef "random"
                      [ Param "max" ( AlexPn 1260 53 14 ) ] Nothing
                      ( Expr
                        ( BinOpExpr ModOp
                          ( Expr
                            ( FunAppExpr "next"
                              [ Expr UnitCExpr ( AlexPn 1271 53 25 ) ]
                            )
                            ( AlexPn 1266 53 20 )
                          )
                          ( Expr ( ConstExpr "max" ) ( AlexPn 1278 53 32 ) )
                        )
                        ( AlexPn 1266 53 20 )
                      )
                      ( AlexPn 1253 53 7 )
                    ]
                    ( AlexPn 1249 53 3 )
                  )
                  ( LetIn
                    ( Let
                      [ VarDef "t" Nothing ( AlexPn 1292 55 7 ) ]
                      ( AlexPn 1288 55 3 )
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
                                          ( BinOpExpr AssignMutableOp
                                            ( Expr
                                              ( ConstExpr "t" )
                                              ( AlexPn 1307 56 3 )
                                            )
                                            ( Expr
                                              ( ConstConstrExpr "Nil" )
                                              ( AlexPn 1312 56 8 )
                                            )
                                          )
                                          ( AlexPn 1307 56 3 )
                                        )
                                        ( Expr
                                          ( ForExpr "i"
                                            ( Expr
                                              ( IntCExpr 1 )
                                              ( AlexPn 1328 58 11 )
                                            )
                                            ( Expr
                                              ( IntCExpr 10 )
                                              ( AlexPn 1333 58 16 )
                                            )
                                            ( Expr
                                              ( BinOpExpr AssignMutableOp
                                                ( Expr
                                                  ( ConstExpr "t" )
                                                  ( AlexPn 1343 59 5 )
                                                )
                                                ( Expr
                                                  ( FunAppExpr "treeInsert"
                                                    [ Expr
                                                      ( UnOpExpr BangOp
                                                        ( Expr
                                                          ( ConstExpr "t" )
                                                          ( AlexPn 1360 59 22 )
                                                        )
                                                      )
                                                      ( AlexPn 1359 59 21 )
                                                    , Expr
                                                      ( FunAppExpr "random"
                                                        [ Expr
                                                          ( IntCExpr 100 )
                                                          ( AlexPn 1370 59 32 )
                                                        ]
                                                      )
                                                      ( AlexPn 1363 59 25 )
                                                    ]
                                                  )
                                                  ( AlexPn 1348 59 10 )
                                                )
                                              )
                                              ( AlexPn 1343 59 5 )
                                            )
                                          )
                                          ( AlexPn 1320 58 3 )
                                        )
                                      )
                                      ( AlexPn 1307 56 3 )
                                    )
                                    ( Expr
                                      ( FunAppExpr "print_string"
                                        [ Expr
                                          ( StringCExpr "Initial tree: " )
                                          ( AlexPn 1399 62 16 )
                                        ]
                                      )
                                      ( AlexPn 1386 62 3 )
                                    )
                                  )
                                  ( AlexPn 1307 56 3 )
                                )
                                ( Expr
                                  ( FunAppExpr "treePrint"
                                    [ Expr
                                      ( UnOpExpr BangOp
                                        ( Expr
                                          ( ConstExpr "t" )
                                          ( AlexPn 1430 63 14 )
                                        )
                                      )
                                      ( AlexPn 1429 63 13 )
                                    ]
                                  )
                                  ( AlexPn 1419 63 3 )
                                )
                              )
                              ( AlexPn 1307 56 3 )
                            )
                            ( Expr
                              ( FunAppExpr "print_string"
                                [ Expr
                                  ( StringCExpr "\n" )
                                  ( AlexPn 1448 64 16 )
                                ]
                              )
                              ( AlexPn 1435 64 3 )
                            )
                          )
                          ( AlexPn 1307 56 3 )
                        )
                        ( LetIn
                          ( LetRec
                            [ FunDef "choose"
                              [ Param "t" ( AlexPn 1472 66 18 ) ] Nothing
                              ( MatchExpr
                                ( Expr ( ConstExpr "t" ) ( AlexPn 1486 67 11 ) )
                                [ Match
                                  ( Pattern
                                    ( ConstrPattern "Node"
                                      [ Pattern
                                        ( IdPattern "n" )
                                        ( AlexPn 1504 68 12 )
                                      , Pattern
                                        ( IdPattern "t1" )
                                        ( AlexPn 1506 68 14 )
                                      , Pattern
                                        ( IdPattern "t2" )
                                        ( AlexPn 1509 68 17 )
                                      ]
                                    )
                                    ( AlexPn 1499 68 7 )
                                  )
                                  ( LetIn
                                    ( Let
                                      [ FunDef "c1" [] Nothing
                                        ( Expr
                                          ( FunAppExpr "treeCount"
                                            [ Expr
                                              ( ConstExpr "t1" )
                                              ( AlexPn 1542 69 28 )
                                            ]
                                          )
                                          ( AlexPn 1532 69 18 )
                                        )
                                        ( AlexPn 1527 69 13 )
                                      , FunDef "c2" [] Nothing
                                        ( Expr
                                          ( FunAppExpr "treeCount"
                                            [ Expr
                                              ( ConstExpr "t2" )
                                              ( AlexPn 1572 70 28 )
                                            ]
                                          )
                                          ( AlexPn 1562 70 18 )
                                        )
                                        ( AlexPn 1557 70 13 )
                                      ]
                                      ( AlexPn 1523 69 9 )
                                    )
                                    ( LetIn
                                      ( Let
                                        [ FunDef "r" [] Nothing
                                          ( Expr
                                            ( FunAppExpr "random"
                                              [ Expr
                                                ( BinOpExpr PlusOp
                                                  ( Expr
                                                    ( BinOpExpr PlusOp
                                                      ( Expr
                                                        ( IntCExpr 1 )
                                                        ( AlexPn 1602 71 25 )
                                                      )
                                                      ( Expr
                                                        ( ConstExpr "c1" )
                                                        ( AlexPn 1606 71 29 )
                                                      )
                                                    )
                                                    ( AlexPn 1602 71 25 )
                                                  )
                                                  ( Expr
                                                    ( ConstExpr "c2" )
                                                    ( AlexPn 1611 71 34 )
                                                  )
                                                )
                                                ( AlexPn 1602 71 25 )
                                              ]
                                            )
                                            ( AlexPn 1594 71 17 )
                                          )
                                          ( AlexPn 1590 71 13 )
                                        ]
                                        ( AlexPn 1586 71 9 )
                                      )
                                      ( Expr
                                        ( IfThenElseExpr
                                          ( Expr
                                            ( BinOpExpr EqOp
                                              ( Expr
                                                ( ConstExpr "r" )
                                                ( AlexPn 1629 72 12 )
                                              )
                                              ( Expr
                                                ( IntCExpr 0 )
                                                ( AlexPn 1633 72 16 )
                                              )
                                            )
                                            ( AlexPn 1629 72 12 )
                                          )
                                          ( Expr
                                            ( ConstExpr "n" )
                                            ( AlexPn 1650 73 11 )
                                          )
                                          ( Expr
                                            ( IfThenElseExpr
                                              ( Expr
                                                ( BinOpExpr LEqOp
                                                  ( Expr
                                                    ( ConstExpr "r" )
                                                    ( AlexPn 1668 74 17 )
                                                  )
                                                  ( Expr
                                                    ( ConstExpr "c1" )
                                                    ( AlexPn 1673 74 22 )
                                                  )
                                                )
                                                ( AlexPn 1668 74 17 )
                                              )
                                              ( Expr
                                                ( FunAppExpr "choose"
                                                  [ Expr
                                                    ( ConstExpr "t1" )
                                                    ( AlexPn 1698 75 18 )
                                                  ]
                                                )
                                                ( AlexPn 1691 75 11 )
                                              )
                                              ( Expr
                                                ( FunAppExpr "choose"
                                                  [ Expr
                                                    ( ConstExpr "t2" )
                                                    ( AlexPn 1731 77 18 )
                                                  ]
                                                )
                                                ( AlexPn 1724 77 11 )
                                              )
                                            )
                                            ( AlexPn 1665 74 14 )
                                          )
                                        )
                                        ( AlexPn 1626 72 9 )
                                      )
                                      ( AlexPn 1586 71 9 )
                                    )
                                    ( AlexPn 1523 69 9 )
                                  )
                                  ( AlexPn 1499 68 7 )
                                ]
                                ( AlexPn 1480 67 5 )
                              )
                              ( AlexPn 1465 66 11 )
                            ]
                            ( AlexPn 1457 66 3 )
                          )
                          ( Expr
                            ( ForExpr "i"
                              ( Expr ( IntCExpr 1 ) ( AlexPn 1756 80 11 ) )
                              ( Expr
                                ( FunAppExpr "treeCount"
                                  [ Expr
                                    ( UnOpExpr BangOp
                                      ( Expr
                                        ( ConstExpr "t" )
                                        ( AlexPn 1772 80 27 )
                                      )
                                    )
                                    ( AlexPn 1771 80 26 )
                                  ]
                                )
                                ( AlexPn 1761 80 16 )
                              )
                              ( LetIn
                                ( Let
                                  [ FunDef "n" [] Nothing
                                    ( Expr
                                      ( FunAppExpr "choose"
                                        [ Expr
                                          ( UnOpExpr BangOp
                                            ( Expr
                                              ( ConstExpr "t" )
                                              ( AlexPn 1797 81 21 )
                                            )
                                          )
                                          ( AlexPn 1796 81 20 )
                                        ]
                                      )
                                      ( AlexPn 1789 81 13 )
                                    )
                                    ( AlexPn 1785 81 9 )
                                  ]
                                  ( AlexPn 1781 81 5 )
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
                                                      ( FunAppExpr "print_string"
                                                        [ Expr
                                                          ( StringCExpr "Deleting " )
                                                          ( AlexPn 1819 82 18 )
                                                        ]
                                                      )
                                                      ( AlexPn 1806 82 5 )
                                                    )
                                                    ( Expr
                                                      ( FunAppExpr "print_int"
                                                        [ Expr
                                                          ( ConstExpr "n" )
                                                          ( AlexPn 1846 83 15 )
                                                        ]
                                                      )
                                                      ( AlexPn 1836 83 5 )
                                                    )
                                                  )
                                                  ( AlexPn 1806 82 5 )
                                                )
                                                ( Expr
                                                  ( FunAppExpr "print_string"
                                                    [ Expr
                                                      ( StringCExpr ": " )
                                                      ( AlexPn 1866 84 18 )
                                                    ]
                                                  )
                                                  ( AlexPn 1853 84 5 )
                                                )
                                              )
                                              ( AlexPn 1806 82 5 )
                                            )
                                            ( Expr
                                              ( BinOpExpr AssignMutableOp
                                                ( Expr
                                                  ( ConstExpr "t" )
                                                  ( AlexPn 1876 85 5 )
                                                )
                                                ( Expr
                                                  ( FunAppExpr "treeDelete"
                                                    [ Expr
                                                      ( UnOpExpr BangOp
                                                        ( Expr
                                                          ( ConstExpr "t" )
                                                          ( AlexPn 1893 85 22 )
                                                        )
                                                      )
                                                      ( AlexPn 1892 85 21 )
                                                    , Expr
                                                      ( ConstExpr "n" )
                                                      ( AlexPn 1895 85 24 )
                                                    ]
                                                  )
                                                  ( AlexPn 1881 85 10 )
                                                )
                                              )
                                              ( AlexPn 1876 85 5 )
                                            )
                                          )
                                          ( AlexPn 1806 82 5 )
                                        )
                                        ( Expr
                                          ( FunAppExpr "treePrint"
                                            [ Expr
                                              ( UnOpExpr BangOp
                                                ( Expr
                                                  ( ConstExpr "t" )
                                                  ( AlexPn 1913 86 16 )
                                                )
                                              )
                                              ( AlexPn 1912 86 15 )
                                            ]
                                          )
                                          ( AlexPn 1902 86 5 )
                                        )
                                      )
                                      ( AlexPn 1806 82 5 )
                                    )
                                    ( Expr
                                      ( FunAppExpr "print_string"
                                        [ Expr
                                          ( StringCExpr "\n" )
                                          ( AlexPn 1933 87 18 )
                                        ]
                                      )
                                      ( AlexPn 1920 87 5 )
                                    )
                                  )
                                  ( AlexPn 1806 82 5 )
                                )
                                ( AlexPn 1781 81 5 )
                              )
                            )
                            ( AlexPn 1748 80 3 )
                          )
                          ( AlexPn 1457 66 3 )
                        )
                      )
                      ( AlexPn 1307 56 3 )
                    )
                    ( AlexPn 1288 55 3 )
                  )
                  ( AlexPn 1249 53 3 )
                )
              )
              ( AlexPn 1234 51 3 )
            )
            ( AlexPn 1164 48 3 )
          )
          ( AlexPn 1142 47 3 )
        )
        ( AlexPn 1133 46 5 )
      ]
      ( AlexPn 1129 46 1 )
    )
  ]
  ( AlexPn 0 1 1 )
