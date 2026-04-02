module Unit.Semantics.Mean (meanSemAST) where

import Common.AST
import Common.SymbolType (SymbolType(..), TypeScheme(..))
import Lexer.Lexer (AlexPosn(..))
import Semantics.Utils (SemanticTag(..), TypeInfo(..))

meanSemAST :: AST SemanticTag
meanSemAST = AST
  [ Left
    ( Let
      [ FunDef "main" [] Nothing
        ( Expr
          ( BinOpExpr SemicolonOp
            ( Expr
              ( FunAppExpr "print_string"
                [ Expr
                  ( StringCExpr "Give n: " )
                  ( SemTag
                    { posn = AlexPn 26 2 16
                    , typeInfo = NodeType
                      ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                    }
                  )
                ]
              )
              ( SemTag
                { posn = AlexPn 13 2 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( LetIn
              ( Let
                [ FunDef "n" [] Nothing
                  ( Expr
                    ( FunAppExpr "read_int"
                      [ Expr UnitCExpr
                        ( SemTag
                          { posn = AlexPn 57 3 20
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      ]
                    )
                    ( SemTag
                      { posn = AlexPn 48 3 11
                      , typeInfo = NodeType ( SymType IntType )
                      }
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 44 3 7
                    , typeInfo = DefType
                      ( MonoType ( SymType IntType ) )
                    }
                  )
                ]
                ( SemTag { posn = AlexPn 40 3 3, typeInfo = NotTypable } )
              )
              ( Expr
                ( BinOpExpr SemicolonOp
                  ( Expr
                    ( FunAppExpr "print_string"
                      [ Expr
                        ( StringCExpr "Give k: " )
                        ( SemTag
                          { posn = AlexPn 78 4 16
                          , typeInfo = NodeType
                            ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                          }
                        )
                      ]
                    )
                    ( SemTag
                      { posn = AlexPn 65 4 3
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                  ( LetIn
                    ( Let
                      [ FunDef "k" [] Nothing
                        ( Expr
                          ( FunAppExpr "read_int"
                            [ Expr UnitCExpr
                              ( SemTag
                                { posn = AlexPn 109 5 20
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            ]
                          )
                          ( SemTag
                            { posn = AlexPn 100 5 11
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( SemTag
                          { posn = AlexPn 96 5 7
                          , typeInfo = DefType
                            ( MonoType ( SymType IntType ) )
                          }
                        )
                      ]
                      ( SemTag { posn = AlexPn 92 5 3, typeInfo = NotTypable } )
                    )
                    ( LetIn
                      ( Let
                        [ VarDef "sum" Nothing
                          ( SemTag
                            { posn = AlexPn 122 7 7
                            , typeInfo = DefType
                              ( MonoType
                                ( SymType ( RefType ( SymType FloatType ) ) )
                              )
                            }
                          )
                        , VarDef "seed" Nothing
                          ( SemTag
                            { posn = AlexPn 140 8 7
                            , typeInfo = DefType
                              ( MonoType
                                ( SymType ( RefType ( SymType IntType ) ) )
                              )
                            }
                          )
                        ]
                        ( SemTag
                          { posn = AlexPn 118 7 3, typeInfo = NotTypable }
                        )
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
                                        ( SemTag
                                          { posn = AlexPn 159 10 3
                                          , typeInfo = NodeType
                                            ( SymType
                                              ( RefType ( SymType FloatType ) )
                                            )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( FloatCExpr 0.0 )
                                        ( SemTag
                                          { posn = AlexPn 166 10 10
                                          , typeInfo = NodeType ( SymType FloatType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 159 10 3
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                  ( Expr
                                    ( BinOpExpr AssignMutableOp
                                      ( Expr
                                        ( ConstExpr "seed" )
                                        ( SemTag
                                          { posn = AlexPn 173 11 3
                                          , typeInfo = NodeType
                                            ( SymType
                                              ( RefType ( SymType IntType ) )
                                            )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( IntCExpr 65 )
                                        ( SemTag
                                          { posn = AlexPn 181 11 11
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 173 11 3
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 159 10 3
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                              ( Expr
                                ( ForExpr "i"
                                  ( Expr
                                    ( IntCExpr 1 )
                                    ( SemTag
                                      { posn = AlexPn 196 13 11
                                      , typeInfo = NodeType ( SymType IntType )
                                      }
                                    )
                                  )
                                  ( Expr
                                    ( ConstExpr "k" )
                                    ( SemTag
                                      { posn = AlexPn 201 13 16
                                      , typeInfo = NodeType ( SymType IntType )
                                      }
                                    )
                                  )
                                  ( Expr
                                    ( BinOpExpr SemicolonOp
                                      ( Expr
                                        ( BinOpExpr AssignMutableOp
                                          ( Expr
                                            ( ConstExpr "seed" )
                                            ( SemTag
                                              { posn = AlexPn 210 14 5
                                              , typeInfo = NodeType
                                                ( SymType
                                                  ( RefType ( SymType IntType ) )
                                                )
                                              }
                                            )
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
                                                                ( SemTag
                                                                  { posn = AlexPn 220 14 15
                                                                  , typeInfo = NodeType
                                                                    ( SymType
                                                                      ( RefType ( SymType IntType ) )
                                                                    )
                                                                  }
                                                                )
                                                              )
                                                            )
                                                            ( SemTag
                                                              { posn = AlexPn 219 14 14
                                                              , typeInfo = NodeType ( SymType IntType )
                                                              }
                                                            )
                                                          )
                                                          ( Expr
                                                            ( IntCExpr 137 )
                                                            ( SemTag
                                                              { posn = AlexPn 227 14 22
                                                              , typeInfo = NodeType ( SymType IntType )
                                                              }
                                                            )
                                                          )
                                                        )
                                                        ( SemTag
                                                          { posn = AlexPn 219 14 14
                                                          , typeInfo = NodeType ( SymType IntType )
                                                          }
                                                        )
                                                      )
                                                      ( Expr
                                                        ( IntCExpr 220 )
                                                        ( SemTag
                                                          { posn = AlexPn 233 14 28
                                                          , typeInfo = NodeType ( SymType IntType )
                                                          }
                                                        )
                                                      )
                                                    )
                                                    ( SemTag
                                                      { posn = AlexPn 219 14 14
                                                      , typeInfo = NodeType ( SymType IntType )
                                                      }
                                                    )
                                                  )
                                                  ( Expr
                                                    ( ConstExpr "i" )
                                                    ( SemTag
                                                      { posn = AlexPn 239 14 34
                                                      , typeInfo = NodeType ( SymType IntType )
                                                      }
                                                    )
                                                  )
                                                )
                                                ( SemTag
                                                  { posn = AlexPn 219 14 14
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                              ( Expr
                                                ( ConstExpr "n" )
                                                ( SemTag
                                                  { posn = AlexPn 246 14 41
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                            )
                                            ( SemTag
                                              { posn = AlexPn 218 14 13
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 210 14 5
                                          , typeInfo = NodeType ( SymType UnitType )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( BinOpExpr AssignMutableOp
                                          ( Expr
                                            ( ConstExpr "sum" )
                                            ( SemTag
                                              { posn = AlexPn 253 15 5
                                              , typeInfo = NodeType
                                                ( SymType
                                                  ( RefType ( SymType FloatType ) )
                                                )
                                              }
                                            )
                                          )
                                          ( Expr
                                            ( BinOpExpr PlusFloatOp
                                              ( Expr
                                                ( UnOpExpr BangOp
                                                  ( Expr
                                                    ( ConstExpr "sum" )
                                                    ( SemTag
                                                      { posn = AlexPn 261 15 13
                                                      , typeInfo = NodeType
                                                        ( SymType
                                                          ( RefType ( SymType FloatType ) )
                                                        )
                                                      }
                                                    )
                                                  )
                                                )
                                                ( SemTag
                                                  { posn = AlexPn 260 15 12
                                                  , typeInfo = NodeType ( SymType FloatType )
                                                  }
                                                )
                                              )
                                              ( Expr
                                                ( FunAppExpr "float_of_int"
                                                  [ Expr
                                                    ( UnOpExpr BangOp
                                                      ( Expr
                                                        ( ConstExpr "seed" )
                                                        ( SemTag
                                                          { posn = AlexPn 282 15 34
                                                          , typeInfo = NodeType
                                                            ( SymType
                                                              ( RefType ( SymType IntType ) )
                                                            )
                                                          }
                                                        )
                                                      )
                                                    )
                                                    ( SemTag
                                                      { posn = AlexPn 281 15 33
                                                      , typeInfo = NodeType ( SymType IntType )
                                                      }
                                                    )
                                                  ]
                                                )
                                                ( SemTag
                                                  { posn = AlexPn 268 15 20
                                                  , typeInfo = NodeType ( SymType FloatType )
                                                  }
                                                )
                                              )
                                            )
                                            ( SemTag
                                              { posn = AlexPn 260 15 12
                                              , typeInfo = NodeType ( SymType FloatType )
                                              }
                                            )
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 253 15 5
                                          , typeInfo = NodeType ( SymType UnitType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 210 14 5
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 188 13 3
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                            )
                            ( SemTag
                              { posn = AlexPn 159 10 3
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                          ( Expr
                            ( IfThenExpr
                              ( Expr
                                ( BinOpExpr GTOp
                                  ( Expr
                                    ( ConstExpr "k" )
                                    ( SemTag
                                      { posn = AlexPn 301 18 6
                                      , typeInfo = NodeType ( SymType IntType )
                                      }
                                    )
                                  )
                                  ( Expr
                                    ( IntCExpr 0 )
                                    ( SemTag
                                      { posn = AlexPn 305 18 10
                                      , typeInfo = NodeType ( SymType IntType )
                                      }
                                    )
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 301 18 6
                                  , typeInfo = NodeType ( SymType BoolType )
                                  }
                                )
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
                                                ( SemTag
                                                  { posn = AlexPn 337 20 18
                                                  , typeInfo = NodeType
                                                    ( SymType
                                                      ( ArrayType 1 ( SymType CharType ) )
                                                    )
                                                  }
                                                )
                                              ]
                                            )
                                            ( SemTag
                                              { posn = AlexPn 324 20 5
                                              , typeInfo = NodeType ( SymType UnitType )
                                              }
                                            )
                                          )
                                          ( Expr
                                            ( FunAppExpr "print_float"
                                              [ Expr
                                                ( BinOpExpr DivFloatOp
                                                  ( Expr
                                                    ( UnOpExpr BangOp
                                                      ( Expr
                                                        ( ConstExpr "sum" )
                                                        ( SemTag
                                                          { posn = AlexPn 365 21 19
                                                          , typeInfo = NodeType
                                                            ( SymType
                                                              ( RefType ( SymType FloatType ) )
                                                            )
                                                          }
                                                        )
                                                      )
                                                    )
                                                    ( SemTag
                                                      { posn = AlexPn 364 21 18
                                                      , typeInfo = NodeType ( SymType FloatType )
                                                      }
                                                    )
                                                  )
                                                  ( Expr
                                                    ( FunAppExpr "float_of_int"
                                                      [ Expr
                                                        ( ConstExpr "k" )
                                                        ( SemTag
                                                          { posn = AlexPn 385 21 39
                                                          , typeInfo = NodeType ( SymType IntType )
                                                          }
                                                        )
                                                      ]
                                                    )
                                                    ( SemTag
                                                      { posn = AlexPn 372 21 26
                                                      , typeInfo = NodeType ( SymType FloatType )
                                                      }
                                                    )
                                                  )
                                                )
                                                ( SemTag
                                                  { posn = AlexPn 364 21 18
                                                  , typeInfo = NodeType ( SymType FloatType )
                                                  }
                                                )
                                              ]
                                            )
                                            ( SemTag
                                              { posn = AlexPn 351 21 5
                                              , typeInfo = NodeType ( SymType UnitType )
                                              }
                                            )
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 324 20 5
                                          , typeInfo = NodeType ( SymType UnitType )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( FunAppExpr "print_string"
                                          [ Expr
                                            ( StringCExpr "\n" )
                                            ( SemTag
                                              { posn = AlexPn 406 22 18
                                              , typeInfo = NodeType
                                                ( SymType
                                                  ( ArrayType 1 ( SymType CharType ) )
                                                )
                                              }
                                            )
                                          ]
                                        )
                                        ( SemTag
                                          { posn = AlexPn 393 22 5
                                          , typeInfo = NodeType ( SymType UnitType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 324 20 5
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 314 19 3
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                            )
                            ( SemTag
                              { posn = AlexPn 298 18 3
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                        )
                        ( SemTag
                          { posn = AlexPn 159 10 3
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 118 7 3
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                    ( SemTag
                      { posn = AlexPn 92 5 3
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                )
                ( SemTag
                  { posn = AlexPn 65 4 3
                  , typeInfo = NodeType ( SymType UnitType )
                  }
                )
              )
              ( SemTag
                { posn = AlexPn 40 3 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
          )
          ( SemTag
            { posn = AlexPn 13 2 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 4 1 5
          , typeInfo = DefType
            ( MonoType ( SymType UnitType ) )
          }
        )
      ]
      ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
    )
  ]
  ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
