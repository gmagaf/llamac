module Unit.Parser.Mean (meanAST) where

import Common.AST
import Lexer.Lexer (AlexPosn(AlexPn))

meanAST :: AST AlexPosn
meanAST = AST
  [ Left
    ( Let
      [ FunDef "main" [] Nothing
        ( Expr
          ( BinOpExpr SemicolonOp
            ( Expr
              ( FunAppExpr "print_string"
                [ Expr ( StringCExpr "Give n: " ) ( AlexPn 26 2 16 ) ]
              )
              ( AlexPn 13 2 3 )
            )
            ( LetIn
              ( Let
                [ FunDef "n" [] Nothing
                  ( Expr
                    ( FunAppExpr "read_int"
                      [ Expr UnitCExpr ( AlexPn 57 3 20 ) ]
                    )
                    ( AlexPn 48 3 11 )
                  )
                  ( AlexPn 44 3 7 )
                ]
                ( AlexPn 40 3 3 )
              )
              ( Expr
                ( BinOpExpr SemicolonOp
                  ( Expr
                    ( FunAppExpr "print_string"
                      [ Expr ( StringCExpr "Give k: " ) ( AlexPn 78 4 16 ) ]
                    )
                    ( AlexPn 65 4 3 )
                  )
                  ( LetIn
                    ( Let
                      [ FunDef "k" [] Nothing
                        ( Expr
                          ( FunAppExpr "read_int"
                            [ Expr UnitCExpr ( AlexPn 109 5 20 ) ]
                          )
                          ( AlexPn 100 5 11 )
                        )
                        ( AlexPn 96 5 7 )
                      ]
                      ( AlexPn 92 5 3 )
                    )
                    ( LetIn
                      ( Let
                        [ VarDef "sum" Nothing
                          ( AlexPn 122 7 7 )
                        , VarDef "seed" Nothing
                          ( AlexPn 140 8 7 )
                        ]
                        ( AlexPn 118 7 3 )
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
                                        ( ConstExpr "sum" )
                                        ( AlexPn 159 10 3 )
                                      )
                                      ( Expr
                                        ( FloatCExpr 0.0 )
                                        ( AlexPn 166 10 10 )
                                      )
                                    )
                                    ( AlexPn 159 10 3 )
                                  )
                                  ( Expr
                                    ( BinOpExpr AssignMutableOp
                                      ( Expr
                                        ( ConstExpr "seed" )
                                        ( AlexPn 173 11 3 )
                                      )
                                      ( Expr
                                        ( IntCExpr 65 )
                                        ( AlexPn 181 11 11 )
                                      )
                                    )
                                    ( AlexPn 173 11 3 )
                                  )
                                )
                                ( AlexPn 159 10 3 )
                              )
                              ( Expr
                                ( ForExpr "i"
                                  ( Expr ( IntCExpr 1 ) ( AlexPn 196 13 11 ) )
                                  ( Expr
                                    ( ConstExpr "k" )
                                    ( AlexPn 201 13 16 )
                                  )
                                  ( Expr
                                    ( BinOpExpr SemicolonOp
                                      ( Expr
                                        ( BinOpExpr AssignMutableOp
                                          ( Expr
                                            ( ConstExpr "seed" )
                                            ( AlexPn 210 14 5 )
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
                                                                ( AlexPn 220 14 15 )
                                                              )
                                                            )
                                                            ( AlexPn 219 14 14 )
                                                          )
                                                          ( Expr
                                                            ( IntCExpr 137 )
                                                            ( AlexPn 227 14 22 )
                                                          )
                                                        )
                                                        ( AlexPn 219 14 14 )
                                                      )
                                                      ( Expr
                                                        ( IntCExpr 220 )
                                                        ( AlexPn 233 14 28 )
                                                      )
                                                    )
                                                    ( AlexPn 219 14 14 )
                                                  )
                                                  ( Expr
                                                    ( ConstExpr "i" )
                                                    ( AlexPn 239 14 34 )
                                                  )
                                                )
                                                ( AlexPn 219 14 14 )
                                              )
                                              ( Expr
                                                ( ConstExpr "n" )
                                                ( AlexPn 246 14 41 )
                                              )
                                            )
                                            ( AlexPn 218 14 13 )
                                          )
                                        )
                                        ( AlexPn 210 14 5 )
                                      )
                                      ( Expr
                                        ( BinOpExpr AssignMutableOp
                                          ( Expr
                                            ( ConstExpr "sum" )
                                            ( AlexPn 253 15 5 )
                                          )
                                          ( Expr
                                            ( BinOpExpr PlusFloatOp
                                              ( Expr
                                                ( UnOpExpr BangOp
                                                  ( Expr
                                                    ( ConstExpr "sum" )
                                                    ( AlexPn 261 15 13 )
                                                  )
                                                )
                                                ( AlexPn 260 15 12 )
                                              )
                                              ( Expr
                                                ( FunAppExpr "float_of_int"
                                                  [ Expr
                                                    ( UnOpExpr BangOp
                                                      ( Expr
                                                        ( ConstExpr "seed" )
                                                        ( AlexPn 282 15 34 )
                                                      )
                                                    )
                                                    ( AlexPn 281 15 33 )
                                                  ]
                                                )
                                                ( AlexPn 268 15 20 )
                                              )
                                            )
                                            ( AlexPn 260 15 12 )
                                          )
                                        )
                                        ( AlexPn 253 15 5 )
                                      )
                                    )
                                    ( AlexPn 210 14 5 )
                                  )
                                )
                                ( AlexPn 188 13 3 )
                              )
                            )
                            ( AlexPn 159 10 3 )
                          )
                          ( Expr
                            ( IfThenExpr
                              ( Expr
                                ( BinOpExpr GTOp
                                  ( Expr ( ConstExpr "k" ) ( AlexPn 301 18 6 ) )
                                  ( Expr ( IntCExpr 0 ) ( AlexPn 305 18 10 ) )
                                )
                                ( AlexPn 301 18 6 )
                              )
                              ( Expr
                                ( BeginExpr
                                  ( Expr
                                    ( BinOpExpr SemicolonOp
                                      ( Expr
                                        ( BinOpExpr SemicolonOp
                                          ( Expr
                                            ( FunAppExpr "print_string"
                                              [ Expr
                                                ( StringCExpr "Mean: " )
                                                ( AlexPn 337 20 18 )
                                              ]
                                            )
                                            ( AlexPn 324 20 5 )
                                          )
                                          ( Expr
                                            ( FunAppExpr "print_float"
                                              [ Expr
                                                ( BinOpExpr DivFloatOp
                                                  ( Expr
                                                    ( UnOpExpr BangOp
                                                      ( Expr
                                                        ( ConstExpr "sum" )
                                                        ( AlexPn 365 21 19 )
                                                      )
                                                    )
                                                    ( AlexPn 364 21 18 )
                                                  )
                                                  ( Expr
                                                    ( FunAppExpr "float_of_int"
                                                      [ Expr
                                                        ( ConstExpr "k" )
                                                        ( AlexPn 385 21 39 )
                                                      ]
                                                    )
                                                    ( AlexPn 372 21 26 )
                                                  )
                                                )
                                                ( AlexPn 364 21 18 )
                                              ]
                                            )
                                            ( AlexPn 351 21 5 )
                                          )
                                        )
                                        ( AlexPn 324 20 5 )
                                      )
                                      ( Expr
                                        ( FunAppExpr "print_string"
                                          [ Expr
                                            ( StringCExpr "\n" )
                                            ( AlexPn 406 22 18 )
                                          ]
                                        )
                                        ( AlexPn 393 22 5 )
                                      )
                                    )
                                    ( AlexPn 324 20 5 )
                                  )
                                )
                                ( AlexPn 314 19 3 )
                              )
                            )
                            ( AlexPn 298 18 3 )
                          )
                        )
                        ( AlexPn 159 10 3 )
                      )
                      ( AlexPn 118 7 3 )
                    )
                    ( AlexPn 92 5 3 )
                  )
                )
                ( AlexPn 65 4 3 )
              )
              ( AlexPn 40 3 3 )
            )
          )
          ( AlexPn 13 2 3 )
        )
        ( AlexPn 4 1 5 )
      ]
      ( AlexPn 0 1 1 )
    )
  ]
  ( AlexPn 0 1 1 )
