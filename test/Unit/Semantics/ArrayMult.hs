module Unit.Semantics.ArrayMult (arrayMultSemAST) where

import Common.AST
import Common.SymbolType (SymbolType(..), TypeScheme(..))
import Lexer.Lexer (AlexPosn(..))
import Semantics.Utils (SemanticTag(..), TypeInfo(..))

arrayMultSemAST :: AST SemanticTag
arrayMultSemAST = AST
  [ Left
    ( Let
      [ FunDef "mmult"
        [ Param "a"
          ( SemTag
            { posn = AlexPn 10 1 11
            , typeInfo = NodeType
              ( SymType ( ArrayType 2 ( SymType IntType ) ) )
            }
          )
        , Param "b"
          ( SemTag
            { posn = AlexPn 12 1 13
            , typeInfo = NodeType
              ( SymType ( ArrayType 2 ( SymType IntType ) ) )
            }
          )
        , Param "c"
          ( SemTag
            { posn = AlexPn 14 1 15
            , typeInfo = NodeType
              ( SymType ( ArrayType 2 ( SymType IntType ) ) )
            }
          )
        ] Nothing
        ( Expr
          ( IfThenExpr
            ( Expr
              ( BinOpExpr AndOp
                ( Expr
                  ( BinOpExpr AndOp
                    ( Expr
                      ( BinOpExpr EqOp
                        ( Expr
                          ( ArrayDim "a" 2 )
                          ( SemTag
                            { posn = AlexPn 23 2 6
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( ArrayDim "b" 1 )
                          ( SemTag
                            { posn = AlexPn 33 2 16
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 23 2 6
                        , typeInfo = NodeType ( SymType BoolType )
                        }
                      )
                    )
                    ( Expr
                      ( BinOpExpr EqOp
                        ( Expr
                          ( ArrayDim "c" 1 )
                          ( SemTag
                            { posn = AlexPn 44 2 27
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( ArrayDim "a" 1 )
                          ( SemTag
                            { posn = AlexPn 54 2 37
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 44 2 27
                        , typeInfo = NodeType ( SymType BoolType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 23 2 6
                    , typeInfo = NodeType ( SymType BoolType )
                    }
                  )
                )
                ( Expr
                  ( BinOpExpr EqOp
                    ( Expr
                      ( ArrayDim "c" 2 )
                      ( SemTag
                        { posn = AlexPn 65 2 48
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( Expr
                      ( ArrayDim "b" 2 )
                      ( SemTag
                        { posn = AlexPn 75 2 58
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 65 2 48
                    , typeInfo = NodeType ( SymType BoolType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 23 2 6
                , typeInfo = NodeType ( SymType BoolType )
                }
              )
            )
            ( Expr
              ( BeginExpr
                ( Expr
                  ( ForExpr "i"
                    ( Expr
                      ( IntCExpr 0 )
                      ( SemTag
                        { posn = AlexPn 108 4 13
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( Expr
                      ( BinOpExpr MinusOp
                        ( Expr
                          ( ArrayDim "c" 1 )
                          ( SemTag
                            { posn = AlexPn 113 4 18
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( IntCExpr 1 )
                          ( SemTag
                            { posn = AlexPn 123 4 28
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 113 4 18
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( Expr
                      ( ForExpr "j"
                        ( Expr
                          ( IntCExpr 0 )
                          ( SemTag
                            { posn = AlexPn 142 5 15
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( BinOpExpr MinusOp
                            ( Expr
                              ( ArrayDim "c" 2 )
                              ( SemTag
                                { posn = AlexPn 147 5 20
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                            ( Expr
                              ( IntCExpr 1 )
                              ( SemTag
                                { posn = AlexPn 157 5 30
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 147 5 20
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( BinOpExpr SemicolonOp
                            ( Expr
                              ( BinOpExpr AssignMutableOp
                                ( Expr
                                  ( ArrayAccess "c"
                                    [ Expr
                                      ( ConstExpr "i" )
                                      ( SemTag
                                        { posn = AlexPn 172 6 11
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    , Expr
                                      ( ConstExpr "j" )
                                      ( SemTag
                                        { posn = AlexPn 175 6 14
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    ]
                                  )
                                  ( SemTag
                                    { posn = AlexPn 170 6 9
                                    , typeInfo = NodeType
                                      ( SymType
                                        ( RefType ( SymType IntType ) )
                                      )
                                    }
                                  )
                                )
                                ( Expr
                                  ( IntCExpr 0 )
                                  ( SemTag
                                    { posn = AlexPn 181 6 20
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 170 6 9
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                            ( Expr
                              ( ForExpr "k"
                                ( Expr
                                  ( IntCExpr 0 )
                                  ( SemTag
                                    { posn = AlexPn 200 7 17
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( BinOpExpr MinusOp
                                    ( Expr
                                      ( ArrayDim "a" 2 )
                                      ( SemTag
                                        { posn = AlexPn 205 7 22
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( IntCExpr 1 )
                                      ( SemTag
                                        { posn = AlexPn 215 7 32
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 205 7 22
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( BinOpExpr AssignMutableOp
                                    ( Expr
                                      ( ArrayAccess "c"
                                        [ Expr
                                          ( ConstExpr "i" )
                                          ( SemTag
                                            { posn = AlexPn 232 8 13
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        , Expr
                                          ( ConstExpr "j" )
                                          ( SemTag
                                            { posn = AlexPn 235 8 16
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        ]
                                      )
                                      ( SemTag
                                        { posn = AlexPn 230 8 11
                                        , typeInfo = NodeType
                                          ( SymType
                                            ( RefType ( SymType IntType ) )
                                          )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( BinOpExpr PlusOp
                                        ( Expr
                                          ( UnOpExpr BangOp
                                            ( Expr
                                              ( ArrayAccess "c"
                                                [ Expr
                                                  ( ConstExpr "i" )
                                                  ( SemTag
                                                    { posn = AlexPn 244 8 25
                                                    , typeInfo = NodeType ( SymType IntType )
                                                    }
                                                  )
                                                , Expr
                                                  ( ConstExpr "j" )
                                                  ( SemTag
                                                    { posn = AlexPn 247 8 28
                                                    , typeInfo = NodeType ( SymType IntType )
                                                    }
                                                  )
                                                ]
                                              )
                                              ( SemTag
                                                { posn = AlexPn 242 8 23
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( RefType ( SymType IntType ) )
                                                  )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 241 8 22
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( BinOpExpr TimesOp
                                            ( Expr
                                              ( UnOpExpr BangOp
                                                ( Expr
                                                  ( ArrayAccess "a"
                                                    [ Expr
                                                      ( ConstExpr "i" )
                                                      ( SemTag
                                                        { posn = AlexPn 255 8 36
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    , Expr
                                                      ( ConstExpr "k" )
                                                      ( SemTag
                                                        { posn = AlexPn 258 8 39
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    ]
                                                  )
                                                  ( SemTag
                                                    { posn = AlexPn 253 8 34
                                                    , typeInfo = NodeType
                                                      ( SymType
                                                        ( RefType ( SymType IntType ) )
                                                      )
                                                    }
                                                  )
                                                )
                                              )
                                              ( SemTag
                                                { posn = AlexPn 252 8 33
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            )
                                            ( Expr
                                              ( UnOpExpr BangOp
                                                ( Expr
                                                  ( ArrayAccess "b"
                                                    [ Expr
                                                      ( ConstExpr "k" )
                                                      ( SemTag
                                                        { posn = AlexPn 266 8 47
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    , Expr
                                                      ( ConstExpr "j" )
                                                      ( SemTag
                                                        { posn = AlexPn 269 8 50
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    ]
                                                  )
                                                  ( SemTag
                                                    { posn = AlexPn 264 8 45
                                                    , typeInfo = NodeType
                                                      ( SymType
                                                        ( RefType ( SymType IntType ) )
                                                      )
                                                    }
                                                  )
                                                )
                                              )
                                              ( SemTag
                                                { posn = AlexPn 263 8 44
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 252 8 33
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 241 8 22
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 230 8 11
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 192 7 9
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 170 6 9
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 134 5 7
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 100 4 5
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 90 3 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
          )
          ( SemTag
            { posn = AlexPn 20 2 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 4 1 5
          , typeInfo = DefType
            ( MonoType
              ( SymType
                ( FunType
                  ( SymType ( ArrayType 2 ( SymType IntType ) ) )
                  ( SymType
                    ( FunType
                      ( SymType ( ArrayType 2 ( SymType IntType ) ) )
                      ( SymType
                        ( FunType
                          ( SymType
                            ( ArrayType 2 ( SymType IntType ) )
                          ) ( SymType UnitType )
                        )
                      )
                    )
                  )
                )
              )
            )
          }
        )
      ]
      ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
    )
  , Left
    ( Let
      [ VarDef "seed" Nothing
        ( SemTag
          { posn = AlexPn 316 14 5
          , typeInfo = DefType
            ( MonoType ( SymType ( RefType ( SymType IntType ) ) ) )
          }
        )
      ]
      ( SemTag { posn = AlexPn 312 14 1, typeInfo = NotTypable } )
    )
  , Left
    ( Let
      [ FunDef "init" [] Nothing
        ( Expr
          ( BinOpExpr AssignMutableOp
            ( Expr
              ( ConstExpr "seed" )
              ( SemTag
                { posn = AlexPn 341 16 12
                , typeInfo = NodeType
                  ( SymType ( RefType ( SymType IntType ) ) )
                }
              )
            )
            ( Expr
              ( IntCExpr 65 )
              ( SemTag
                { posn = AlexPn 349 16 20
                , typeInfo = NodeType ( SymType IntType )
                }
              )
            )
          )
          ( SemTag
            { posn = AlexPn 341 16 12
            , typeInfo = NodeType ( SymType UnitType )
            }
          )
        )
        ( SemTag
          { posn = AlexPn 334 16 5
          , typeInfo = DefType
            ( MonoType ( SymType UnitType ) )
          }
        )
      ]
      ( SemTag { posn = AlexPn 330 16 1, typeInfo = NotTypable } )
    )
  , Left
    ( Let
      [ FunDef "minit"
        [ Param "m"
          ( SemTag
            { posn = AlexPn 363 18 11
            , typeInfo = NodeType
              ( SymType ( ArrayType 2 ( SymType IntType ) ) )
            }
          )
        ] Nothing
        ( Expr
          ( ForExpr "i"
            ( Expr
              ( IntCExpr 0 )
              ( SemTag
                { posn = AlexPn 377 19 11
                , typeInfo = NodeType ( SymType IntType )
                }
              )
            )
            ( Expr
              ( BinOpExpr MinusOp
                ( Expr
                  ( ArrayDim "m" 1 )
                  ( SemTag
                    { posn = AlexPn 382 19 16
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
                ( Expr
                  ( IntCExpr 1 )
                  ( SemTag
                    { posn = AlexPn 392 19 26
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 382 19 16
                , typeInfo = NodeType ( SymType IntType )
                }
              )
            )
            ( Expr
              ( ForExpr "j"
                ( Expr
                  ( IntCExpr 0 )
                  ( SemTag
                    { posn = AlexPn 409 20 13
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
                ( Expr
                  ( BinOpExpr MinusOp
                    ( Expr
                      ( ArrayDim "m" 2 )
                      ( SemTag
                        { posn = AlexPn 414 20 18
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( Expr
                      ( IntCExpr 1 )
                      ( SemTag
                        { posn = AlexPn 424 20 28
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 414 20 18
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
                            { posn = AlexPn 435 21 7
                            , typeInfo = NodeType
                              ( SymType ( RefType ( SymType IntType ) ) )
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
                                                { posn = AlexPn 445 21 17
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( RefType ( SymType IntType ) )
                                                  )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 444 21 16
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( IntCExpr 137 )
                                          ( SemTag
                                            { posn = AlexPn 452 21 24
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 444 21 16
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( BinOpExpr TimesOp
                                        ( Expr
                                          ( IntCExpr 2 )
                                          ( SemTag
                                            { posn = AlexPn 458 21 30
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( ConstExpr "i" )
                                          ( SemTag
                                            { posn = AlexPn 460 21 32
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 458 21 30
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 444 21 16
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( ConstExpr "j" )
                                  ( SemTag
                                    { posn = AlexPn 464 21 36
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 444 21 16
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                            ( Expr
                              ( IntCExpr 101 )
                              ( SemTag
                                { posn = AlexPn 471 21 43
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 443 21 15
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 435 21 7
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr
                          ( ArrayAccess "m"
                            [ Expr
                              ( ConstExpr "i" )
                              ( SemTag
                                { posn = AlexPn 484 22 9
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            , Expr
                              ( ConstExpr "j" )
                              ( SemTag
                                { posn = AlexPn 487 22 12
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            ]
                          )
                          ( SemTag
                            { posn = AlexPn 482 22 7
                            , typeInfo = NodeType
                              ( SymType ( RefType ( SymType IntType ) ) )
                            }
                          )
                        )
                        ( Expr
                          ( UnOpExpr BangOp
                            ( Expr
                              ( ConstExpr "seed" )
                              ( SemTag
                                { posn = AlexPn 494 22 19
                                , typeInfo = NodeType
                                  ( SymType ( RefType ( SymType IntType ) ) )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 493 22 18
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 482 22 7
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 435 21 7
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 401 20 5
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
          )
          ( SemTag
            { posn = AlexPn 369 19 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 357 18 5
          , typeInfo = DefType
            ( MonoType
              ( SymType
                ( FunType
                  ( SymType
                    ( ArrayType 2 ( SymType IntType ) )
                  ) ( SymType UnitType )
                )
              )
            )
          }
        )
      ]
      ( SemTag { posn = AlexPn 353 18 1, typeInfo = NotTypable } )
    )
  , Left
    ( Let
      [ FunDef "mprint"
        [ Param "m"
          ( SemTag
            { posn = AlexPn 527 26 12
            , typeInfo = NodeType
              ( SymType ( ArrayType 2 ( SymType IntType ) ) )
            }
          )
        ] Nothing
        ( Expr
          ( ForExpr "i"
            ( Expr
              ( IntCExpr 0 )
              ( SemTag
                { posn = AlexPn 541 27 11
                , typeInfo = NodeType ( SymType IntType )
                }
              )
            )
            ( Expr
              ( BinOpExpr MinusOp
                ( Expr
                  ( ArrayDim "m" 1 )
                  ( SemTag
                    { posn = AlexPn 546 27 16
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
                ( Expr
                  ( IntCExpr 1 )
                  ( SemTag
                    { posn = AlexPn 556 27 26
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 546 27 16
                , typeInfo = NodeType ( SymType IntType )
                }
              )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( ForExpr "j"
                    ( Expr
                      ( IntCExpr 0 )
                      ( SemTag
                        { posn = AlexPn 573 28 13
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( Expr
                      ( BinOpExpr MinusOp
                        ( Expr
                          ( ArrayDim "m" 2 )
                          ( SemTag
                            { posn = AlexPn 578 28 18
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( IntCExpr 1 )
                          ( SemTag
                            { posn = AlexPn 588 28 28
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 578 28 18
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
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
                                      ( SemTag
                                        { posn = AlexPn 612 29 20
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    , Expr
                                      ( ConstExpr "j" )
                                      ( SemTag
                                        { posn = AlexPn 615 29 23
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    ]
                                  )
                                  ( SemTag
                                    { posn = AlexPn 610 29 18
                                    , typeInfo = NodeType
                                      ( SymType
                                        ( RefType ( SymType IntType ) )
                                      )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 609 29 17
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            ]
                          )
                          ( SemTag
                            { posn = AlexPn 599 29 7
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                        ( Expr
                          ( FunAppExpr "print_string"
                            [ Expr
                              ( StringCExpr " " )
                              ( SemTag
                                { posn = AlexPn 638 30 20
                                , typeInfo = NodeType
                                  ( SymType
                                    ( ArrayType 1 ( SymType CharType ) )
                                  )
                                }
                              )
                            ]
                          )
                          ( SemTag
                            { posn = AlexPn 625 30 7
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 599 29 7
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 565 28 5
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
                ( Expr
                  ( FunAppExpr "print_string"
                    [ Expr
                      ( StringCExpr "\n" )
                      ( SemTag
                        { posn = AlexPn 669 32 18
                        , typeInfo = NodeType
                          ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                        }
                      )
                    ]
                  )
                  ( SemTag
                    { posn = AlexPn 656 32 5
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 565 28 5
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
          )
          ( SemTag
            { posn = AlexPn 533 27 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 520 26 5
          , typeInfo = DefType
            ( MonoType
              ( SymType
                ( FunType
                  ( SymType
                    ( ArrayType 2 ( SymType IntType ) )
                  ) ( SymType UnitType )
                )
              )
            )
          }
        )
      ]
      ( SemTag { posn = AlexPn 516 26 1, typeInfo = NotTypable } )
    )
  , Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ ArrayDef "x"
              [ Expr
                ( IntCExpr 3 )
                ( SemTag
                  { posn = AlexPn 709 36 17
                  , typeInfo = NodeType ( SymType IntType )
                  }
                )
              , Expr
                ( IntCExpr 4 )
                ( SemTag
                  { posn = AlexPn 711 36 19
                  , typeInfo = NodeType ( SymType IntType )
                  }
                )
              ] Nothing
              ( SemTag
                { posn = AlexPn 699 36 7
                , typeInfo = DefType
                  ( MonoType ( SymType ( ArrayType 2 ( SymType IntType ) ) ) )
                }
              )
            , ArrayDef "y"
              [ Expr
                ( IntCExpr 4 )
                ( SemTag
                  { posn = AlexPn 730 37 17
                  , typeInfo = NodeType ( SymType IntType )
                  }
                )
              , Expr
                ( IntCExpr 5 )
                ( SemTag
                  { posn = AlexPn 732 37 19
                  , typeInfo = NodeType ( SymType IntType )
                  }
                )
              ] Nothing
              ( SemTag
                { posn = AlexPn 720 37 7
                , typeInfo = DefType
                  ( MonoType ( SymType ( ArrayType 2 ( SymType IntType ) ) ) )
                }
              )
            , ArrayDef "z"
              [ Expr
                ( IntCExpr 3 )
                ( SemTag
                  { posn = AlexPn 751 38 17
                  , typeInfo = NodeType ( SymType IntType )
                  }
                )
              , Expr
                ( IntCExpr 5 )
                ( SemTag
                  { posn = AlexPn 753 38 19
                  , typeInfo = NodeType ( SymType IntType )
                  }
                )
              ] Nothing
              ( SemTag
                { posn = AlexPn 741 38 7
                , typeInfo = DefType
                  ( MonoType ( SymType ( ArrayType 2 ( SymType IntType ) ) ) )
                }
              )
            ]
            ( SemTag { posn = AlexPn 695 36 3, typeInfo = NotTypable } )
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
                                            ( SemTag
                                              { posn = AlexPn 768 40 9
                                              , typeInfo = NodeType
                                                ( SymType
                                                  ( ArrayType 2 ( SymType IntType ) )
                                                )
                                              }
                                            )
                                          ]
                                        )
                                        ( SemTag
                                          { posn = AlexPn 762 40 3
                                          , typeInfo = NodeType ( SymType UnitType )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( FunAppExpr "minit"
                                          [ Expr
                                            ( ConstExpr "y" )
                                            ( SemTag
                                              { posn = AlexPn 779 41 9
                                              , typeInfo = NodeType
                                                ( SymType
                                                  ( ArrayType 2 ( SymType IntType ) )
                                                )
                                              }
                                            )
                                          ]
                                        )
                                        ( SemTag
                                          { posn = AlexPn 773 41 3
                                          , typeInfo = NodeType ( SymType UnitType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 762 40 3
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                  ( Expr
                                    ( FunAppExpr "mprint"
                                      [ Expr
                                        ( ConstExpr "x" )
                                        ( SemTag
                                          { posn = AlexPn 792 43 10
                                          , typeInfo = NodeType
                                            ( SymType
                                              ( ArrayType 2 ( SymType IntType ) )
                                            )
                                          }
                                        )
                                      ]
                                    )
                                    ( SemTag
                                      { posn = AlexPn 785 43 3
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 762 40 3
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                              ( Expr
                                ( FunAppExpr "print_string"
                                  [ Expr
                                    ( StringCExpr "\ntimes\n\n" )
                                    ( SemTag
                                      { posn = AlexPn 810 44 16
                                      , typeInfo = NodeType
                                        ( SymType
                                          ( ArrayType 1 ( SymType CharType ) )
                                        )
                                      }
                                    )
                                  ]
                                )
                                ( SemTag
                                  { posn = AlexPn 797 44 3
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                            )
                            ( SemTag
                              { posn = AlexPn 762 40 3
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                          ( Expr
                            ( FunAppExpr "mprint"
                              [ Expr
                                ( ConstExpr "y" )
                                ( SemTag
                                  { posn = AlexPn 834 45 10
                                  , typeInfo = NodeType
                                    ( SymType
                                      ( ArrayType 2 ( SymType IntType ) )
                                    )
                                  }
                                )
                              ]
                            )
                            ( SemTag
                              { posn = AlexPn 827 45 3
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                        )
                        ( SemTag
                          { posn = AlexPn 762 40 3
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      )
                      ( Expr
                        ( FunAppExpr "print_string"
                          [ Expr
                            ( StringCExpr "\nmakes\n\n" )
                            ( SemTag
                              { posn = AlexPn 852 46 16
                              , typeInfo = NodeType
                                ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                              }
                            )
                          ]
                        )
                        ( SemTag
                          { posn = AlexPn 839 46 3
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      )
                    )
                    ( SemTag
                      { posn = AlexPn 762 40 3
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                  ( Expr
                    ( FunAppExpr "mmult"
                      [ Expr
                        ( ConstExpr "x" )
                        ( SemTag
                          { posn = AlexPn 875 47 9
                          , typeInfo = NodeType
                            ( SymType ( ArrayType 2 ( SymType IntType ) ) )
                          }
                        )
                      , Expr
                        ( ConstExpr "y" )
                        ( SemTag
                          { posn = AlexPn 877 47 11
                          , typeInfo = NodeType
                            ( SymType ( ArrayType 2 ( SymType IntType ) ) )
                          }
                        )
                      , Expr
                        ( ConstExpr "z" )
                        ( SemTag
                          { posn = AlexPn 879 47 13
                          , typeInfo = NodeType
                            ( SymType ( ArrayType 2 ( SymType IntType ) ) )
                          }
                        )
                      ]
                    )
                    ( SemTag
                      { posn = AlexPn 869 47 3
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                )
                ( SemTag
                  { posn = AlexPn 762 40 3
                  , typeInfo = NodeType ( SymType UnitType )
                  }
                )
              )
              ( Expr
                ( FunAppExpr "mprint"
                  [ Expr
                    ( ConstExpr "z" )
                    ( SemTag
                      { posn = AlexPn 891 48 10
                      , typeInfo = NodeType
                        ( SymType ( ArrayType 2 ( SymType IntType ) ) )
                      }
                    )
                  ]
                )
                ( SemTag
                  { posn = AlexPn 884 48 3
                  , typeInfo = NodeType ( SymType UnitType )
                  }
                )
              )
            )
            ( SemTag
              { posn = AlexPn 762 40 3
              , typeInfo = NodeType ( SymType UnitType )
              }
            )
          )
          ( SemTag
            { posn = AlexPn 695 36 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 686 35 5
          , typeInfo = DefType
            ( MonoType ( SymType UnitType ) )
          }
        )
      ]
      ( SemTag { posn = AlexPn 682 35 1, typeInfo = NotTypable } )
    )
  ]
  ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
