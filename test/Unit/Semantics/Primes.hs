module Unit.Semantics.Primes (primesSemAST) where

import Common.AST
import Common.SymbolType (SymbolType(..), TypeScheme(..))
import Lexer.Lexer (AlexPosn(..))
import Semantics.Utils (SemanticTag(..), TypeInfo(..))

primesSemAST :: AST SemanticTag
primesSemAST = AST
  [ Left
    ( LetRec
      [ FunDef "prime"
        [ Param "n"
          ( SemTag
            { posn = AlexPn 14 1 15, typeInfo = NodeType ( SymType IntType ) }
          )
        ] Nothing
        ( Expr
          ( IfThenElseExpr
            ( Expr
              ( BinOpExpr LTOp
                ( Expr
                  ( ConstExpr "n" )
                  ( SemTag
                    { posn = AlexPn 28 2 11
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
                ( Expr
                  ( IntCExpr 0 )
                  ( SemTag
                    { posn = AlexPn 32 2 15
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 28 2 11
                , typeInfo = NodeType ( SymType BoolType )
                }
              )
            )
            ( Expr
              ( FunAppExpr "prime"
                [ Expr
                  ( UnOpExpr MinusUnOp
                    ( Expr
                      ( ConstExpr "n" )
                      ( SemTag
                        { posn = AlexPn 53 2 36
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 52 2 35
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                ]
              )
              ( SemTag
                { posn = AlexPn 45 2 28
                , typeInfo = NodeType ( SymType BoolType )
                }
              )
            )
            ( Expr
              ( IfThenElseExpr
                ( Expr
                  ( BinOpExpr LTOp
                    ( Expr
                      ( ConstExpr "n" )
                      ( SemTag
                        { posn = AlexPn 66 3 11
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( Expr
                      ( IntCExpr 2 )
                      ( SemTag
                        { posn = AlexPn 70 3 15
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 66 3 11
                    , typeInfo = NodeType ( SymType BoolType )
                    }
                  )
                )
                ( Expr FalseCExpr
                  ( SemTag
                    { posn = AlexPn 83 3 28
                    , typeInfo = NodeType ( SymType BoolType )
                    }
                  )
                )
                ( Expr
                  ( IfThenElseExpr
                    ( Expr
                      ( BinOpExpr EqOp
                        ( Expr
                          ( ConstExpr "n" )
                          ( SemTag
                            { posn = AlexPn 99 4 11
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( IntCExpr 2 )
                          ( SemTag
                            { posn = AlexPn 103 4 15
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 99 4 11
                        , typeInfo = NodeType ( SymType BoolType )
                        }
                      )
                    )
                    ( Expr TrueCExpr
                      ( SemTag
                        { posn = AlexPn 116 4 28
                        , typeInfo = NodeType ( SymType BoolType )
                        }
                      )
                    )
                    ( Expr
                      ( IfThenElseExpr
                        ( Expr
                          ( BinOpExpr EqOp
                            ( Expr
                              ( BinOpExpr ModOp
                                ( Expr
                                  ( ConstExpr "n" )
                                  ( SemTag
                                    { posn = AlexPn 131 5 11
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( IntCExpr 2 )
                                  ( SemTag
                                    { posn = AlexPn 137 5 17
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 131 5 11
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                            ( Expr
                              ( IntCExpr 0 )
                              ( SemTag
                                { posn = AlexPn 141 5 21
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 131 5 11
                            , typeInfo = NodeType ( SymType BoolType )
                            }
                          )
                        )
                        ( Expr FalseCExpr
                          ( SemTag
                            { posn = AlexPn 148 5 28
                            , typeInfo = NodeType ( SymType BoolType )
                            }
                          )
                        )
                        ( LetIn
                          ( LetRec
                            [ FunDef "loop"
                              [ Param "i"
                                ( SemTag
                                  { posn = AlexPn 174 6 21
                                  , typeInfo = NodeType ( SymType IntType )
                                  }
                                )
                              ] Nothing
                              ( Expr
                                ( IfThenElseExpr
                                  ( Expr
                                    ( BinOpExpr LEqOp
                                      ( Expr
                                        ( ConstExpr "i" )
                                        ( SemTag
                                          { posn = AlexPn 185 7 8
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( BinOpExpr DivOp
                                          ( Expr
                                            ( ConstExpr "n" )
                                            ( SemTag
                                              { posn = AlexPn 190 7 13
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          )
                                          ( Expr
                                            ( IntCExpr 2 )
                                            ( SemTag
                                              { posn = AlexPn 194 7 17
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 190 7 13
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 185 7 8
                                      , typeInfo = NodeType ( SymType BoolType )
                                      }
                                    )
                                  )
                                  ( Expr
                                    ( IfThenElseExpr
                                      ( Expr
                                        ( BinOpExpr EqOp
                                          ( Expr
                                            ( BinOpExpr ModOp
                                              ( Expr
                                                ( ConstExpr "n" )
                                                ( SemTag
                                                  { posn = AlexPn 210 8 10
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                              ( Expr
                                                ( ConstExpr "i" )
                                                ( SemTag
                                                  { posn = AlexPn 216 8 16
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                            )
                                            ( SemTag
                                              { posn = AlexPn 210 8 10
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          )
                                          ( Expr
                                            ( IntCExpr 0 )
                                            ( SemTag
                                              { posn = AlexPn 220 8 20
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 210 8 10
                                          , typeInfo = NodeType ( SymType BoolType )
                                          }
                                        )
                                      )
                                      ( Expr FalseCExpr
                                        ( SemTag
                                          { posn = AlexPn 227 8 27
                                          , typeInfo = NodeType ( SymType BoolType )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( FunAppExpr "loop"
                                          [ Expr
                                            ( BinOpExpr PlusOp
                                              ( Expr
                                                ( ConstExpr "i" )
                                                ( SemTag
                                                  { posn = AlexPn 265 9 33
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                              ( Expr
                                                ( IntCExpr 2 )
                                                ( SemTag
                                                  { posn = AlexPn 267 9 35
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                            )
                                            ( SemTag
                                              { posn = AlexPn 265 9 33
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          ]
                                        )
                                        ( SemTag
                                          { posn = AlexPn 259 9 27
                                          , typeInfo = NodeType ( SymType BoolType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 207 8 7
                                      , typeInfo = NodeType ( SymType BoolType )
                                      }
                                    )
                                  )
                                  ( Expr TrueCExpr
                                    ( SemTag
                                      { posn = AlexPn 289 11 9
                                      , typeInfo = NodeType ( SymType BoolType )
                                      }
                                    )
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 182 7 5
                                  , typeInfo = NodeType ( SymType BoolType )
                                  }
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 169 6 16
                                , typeInfo = DefType
                                  ( MonoType
                                    ( SymType
                                      ( FunType ( SymType IntType ) ( SymType BoolType ) )
                                    )
                                  )
                                }
                              )
                            ]
                            ( SemTag
                              { posn = AlexPn 161 6 8, typeInfo = NotTypable }
                            )
                          )
                          ( Expr
                            ( FunAppExpr "loop"
                              [ Expr
                                ( IntCExpr 3 )
                                ( SemTag
                                  { posn = AlexPn 306 12 10
                                  , typeInfo = NodeType ( SymType IntType )
                                  }
                                )
                              ]
                            )
                            ( SemTag
                              { posn = AlexPn 301 12 5
                              , typeInfo = NodeType ( SymType BoolType )
                              }
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 161 6 8
                            , typeInfo = NodeType ( SymType BoolType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 128 5 8
                        , typeInfo = NodeType ( SymType BoolType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 96 4 8
                    , typeInfo = NodeType ( SymType BoolType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 63 3 8
                , typeInfo = NodeType ( SymType BoolType )
                }
              )
            )
          )
          ( SemTag
            { posn = AlexPn 25 2 8, typeInfo = NodeType ( SymType BoolType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 8 1 9
          , typeInfo = DefType
            ( MonoType
              ( SymType ( FunType ( SymType IntType ) ( SymType BoolType ) ) )
            )
          }
        )
      ]
      ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
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
                  ( SemTag
                    { posn = AlexPn 335 15 16
                    , typeInfo = NodeType
                      ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                    }
                  )
                ]
              )
              ( SemTag
                { posn = AlexPn 322 15 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( LetIn
              ( Let
                [ FunDef "limit" [] Nothing
                  ( Expr
                    ( FunAppExpr "read_int"
                      [ Expr UnitCExpr
                        ( SemTag
                          { posn = AlexPn 392 16 24
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      ]
                    )
                    ( SemTag
                      { posn = AlexPn 383 16 15
                      , typeInfo = NodeType ( SymType IntType )
                      }
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 375 16 7
                    , typeInfo = DefType
                      ( MonoType ( SymType IntType ) )
                    }
                  )
                ]
                ( SemTag { posn = AlexPn 371 16 3, typeInfo = NotTypable } )
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
                                ( SemTag
                                  { posn = AlexPn 413 17 16
                                  , typeInfo = NodeType
                                    ( SymType
                                      ( ArrayType 1 ( SymType CharType ) )
                                    )
                                  }
                                )
                              ]
                            )
                            ( SemTag
                              { posn = AlexPn 400 17 3
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                          ( Expr
                            ( FunAppExpr "print_int"
                              [ Expr
                                ( ConstExpr "limit" )
                                ( SemTag
                                  { posn = AlexPn 457 18 13
                                  , typeInfo = NodeType ( SymType IntType )
                                  }
                                )
                              ]
                            )
                            ( SemTag
                              { posn = AlexPn 447 18 3
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                        )
                        ( SemTag
                          { posn = AlexPn 400 17 3
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      )
                      ( Expr
                        ( FunAppExpr "print_string"
                          [ Expr
                            ( StringCExpr "\n\n" )
                            ( SemTag
                              { posn = AlexPn 479 19 16
                              , typeInfo = NodeType
                                ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                              }
                            )
                          ]
                        )
                        ( SemTag
                          { posn = AlexPn 466 19 3
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      )
                    )
                    ( SemTag
                      { posn = AlexPn 400 17 3
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                  ( LetIn
                    ( Let
                      [ VarDef "counter" Nothing
                        ( SemTag
                          { posn = AlexPn 493 20 7
                          , typeInfo = DefType
                            ( MonoType
                              ( SymType ( RefType ( SymType IntType ) ) )
                            )
                          }
                        )
                      ]
                      ( SemTag
                        { posn = AlexPn 489 20 3, typeInfo = NotTypable }
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
                                      ( ConstExpr "counter" )
                                      ( SemTag
                                        { posn = AlexPn 514 21 3
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
                                        { posn = AlexPn 525 21 14
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 514 21 3
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( IfThenExpr
                                    ( Expr
                                      ( BinOpExpr GEqOp
                                        ( Expr
                                          ( ConstExpr "limit" )
                                          ( SemTag
                                            { posn = AlexPn 533 22 6
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( IntCExpr 2 )
                                          ( SemTag
                                            { posn = AlexPn 542 22 15
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 533 22 6
                                        , typeInfo = NodeType ( SymType BoolType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( BinOpExpr SemicolonOp
                                        ( Expr
                                          ( FunAppExpr "incr"
                                            [ Expr
                                              ( ConstExpr "counter" )
                                              ( SemTag
                                                { posn = AlexPn 555 22 28
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( RefType ( SymType IntType ) )
                                                  )
                                                }
                                              )
                                            ]
                                          )
                                          ( SemTag
                                            { posn = AlexPn 550 22 23
                                            , typeInfo = NodeType ( SymType UnitType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( FunAppExpr "print_string"
                                            [ Expr
                                              ( StringCExpr "2\n" )
                                              ( SemTag
                                                { posn = AlexPn 577 22 50
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( ArrayType 1 ( SymType CharType ) )
                                                  )
                                                }
                                              )
                                            ]
                                          )
                                          ( SemTag
                                            { posn = AlexPn 564 22 37
                                            , typeInfo = NodeType ( SymType UnitType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 550 22 23
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 530 22 3
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 514 21 3
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                            ( Expr
                              ( IfThenExpr
                                ( Expr
                                  ( BinOpExpr GEqOp
                                    ( Expr
                                      ( ConstExpr "limit" )
                                      ( SemTag
                                        { posn = AlexPn 590 23 6
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( IntCExpr 3 )
                                      ( SemTag
                                        { posn = AlexPn 599 23 15
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 590 23 6
                                    , typeInfo = NodeType ( SymType BoolType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( BinOpExpr SemicolonOp
                                    ( Expr
                                      ( FunAppExpr "incr"
                                        [ Expr
                                          ( ConstExpr "counter" )
                                          ( SemTag
                                            { posn = AlexPn 612 23 28
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( RefType ( SymType IntType ) )
                                              )
                                            }
                                          )
                                        ]
                                      )
                                      ( SemTag
                                        { posn = AlexPn 607 23 23
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( FunAppExpr "print_string"
                                        [ Expr
                                          ( StringCExpr "3\n" )
                                          ( SemTag
                                            { posn = AlexPn 634 23 50
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( ArrayType 1 ( SymType CharType ) )
                                              )
                                            }
                                          )
                                        ]
                                      )
                                      ( SemTag
                                        { posn = AlexPn 621 23 37
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 607 23 23
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 587 23 3
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 514 21 3
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                        ( LetIn
                          ( LetRec
                            [ FunDef "loop"
                              [ Param "number"
                                ( SemTag
                                  { posn = AlexPn 657 24 16
                                  , typeInfo = NodeType ( SymType IntType )
                                  }
                                )
                              ] Nothing
                              ( Expr
                                ( IfThenExpr
                                  ( Expr
                                    ( BinOpExpr LEqOp
                                      ( Expr
                                        ( ConstExpr "number" )
                                        ( SemTag
                                          { posn = AlexPn 673 25 8
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( ConstExpr "limit" )
                                        ( SemTag
                                          { posn = AlexPn 683 25 18
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 673 25 8
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
                                                ( IfThenExpr
                                                  ( Expr
                                                    ( FunAppExpr "prime"
                                                      [ Expr
                                                        ( BinOpExpr MinusOp
                                                          ( Expr
                                                            ( ConstExpr "number" )
                                                            ( SemTag
                                                              { posn = AlexPn 720 27 17
                                                              , typeInfo = NodeType ( SymType IntType )
                                                              }
                                                            )
                                                          )
                                                          ( Expr
                                                            ( IntCExpr 1 )
                                                            ( SemTag
                                                              { posn = AlexPn 729 27 26
                                                              , typeInfo = NodeType ( SymType IntType )
                                                              }
                                                            )
                                                          )
                                                        )
                                                        ( SemTag
                                                          { posn = AlexPn 720 27 17
                                                          , typeInfo = NodeType ( SymType IntType )
                                                          }
                                                        )
                                                      ]
                                                    )
                                                    ( SemTag
                                                      { posn = AlexPn 713 27 10
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
                                                                ( FunAppExpr "incr"
                                                                  [ Expr
                                                                    ( ConstExpr "counter" )
                                                                    ( SemTag
                                                                      { posn = AlexPn 762 29 14
                                                                      , typeInfo = NodeType
                                                                        ( SymType
                                                                          ( RefType ( SymType IntType ) )
                                                                        )
                                                                      }
                                                                    )
                                                                  ]
                                                                )
                                                                ( SemTag
                                                                  { posn = AlexPn 757 29 9
                                                                  , typeInfo = NodeType ( SymType UnitType )
                                                                  }
                                                                )
                                                              )
                                                              ( Expr
                                                                ( FunAppExpr "print_int"
                                                                  [ Expr
                                                                    ( BinOpExpr MinusOp
                                                                      ( Expr
                                                                        ( ConstExpr "number" )
                                                                        ( SemTag
                                                                          { posn = AlexPn 790 30 20
                                                                          , typeInfo = NodeType ( SymType IntType )
                                                                          }
                                                                        )
                                                                      )
                                                                      ( Expr
                                                                        ( IntCExpr 1 )
                                                                        ( SemTag
                                                                          { posn = AlexPn 799 30 29
                                                                          , typeInfo = NodeType ( SymType IntType )
                                                                          }
                                                                        )
                                                                      )
                                                                    )
                                                                    ( SemTag
                                                                      { posn = AlexPn 790 30 20
                                                                      , typeInfo = NodeType ( SymType IntType )
                                                                      }
                                                                    )
                                                                  ]
                                                                )
                                                                ( SemTag
                                                                  { posn = AlexPn 779 30 9
                                                                  , typeInfo = NodeType ( SymType UnitType )
                                                                  }
                                                                )
                                                              )
                                                            )
                                                            ( SemTag
                                                              { posn = AlexPn 757 29 9
                                                              , typeInfo = NodeType ( SymType UnitType )
                                                              }
                                                            )
                                                          )
                                                          ( Expr
                                                            ( FunAppExpr "print_string"
                                                              [ Expr
                                                                ( StringCExpr "\n" )
                                                                ( SemTag
                                                                  { posn = AlexPn 824 31 22
                                                                  , typeInfo = NodeType
                                                                    ( SymType
                                                                      ( ArrayType 1 ( SymType CharType ) )
                                                                    )
                                                                  }
                                                                )
                                                              ]
                                                            )
                                                            ( SemTag
                                                              { posn = AlexPn 811 31 9
                                                              , typeInfo = NodeType ( SymType UnitType )
                                                              }
                                                            )
                                                          )
                                                        )
                                                        ( SemTag
                                                          { posn = AlexPn 757 29 9
                                                          , typeInfo = NodeType ( SymType UnitType )
                                                          }
                                                        )
                                                      )
                                                    )
                                                    ( SemTag
                                                      { posn = AlexPn 743 28 7
                                                      , typeInfo = NodeType ( SymType UnitType )
                                                      }
                                                    )
                                                  )
                                                )
                                                ( SemTag
                                                  { posn = AlexPn 710 27 7
                                                  , typeInfo = NodeType ( SymType UnitType )
                                                  }
                                                )
                                              )
                                              ( Expr
                                                ( IfThenExpr
                                                  ( Expr
                                                    ( BinOpExpr AndOp
                                                      ( Expr
                                                        ( BinOpExpr NotEqOp
                                                          ( Expr
                                                            ( ConstExpr "number" )
                                                            ( SemTag
                                                              { posn = AlexPn 849 33 10
                                                              , typeInfo = NodeType ( SymType IntType )
                                                              }
                                                            )
                                                          )
                                                          ( Expr
                                                            ( ConstExpr "limit" )
                                                            ( SemTag
                                                              { posn = AlexPn 859 33 20
                                                              , typeInfo = NodeType ( SymType IntType )
                                                              }
                                                            )
                                                          )
                                                        )
                                                        ( SemTag
                                                          { posn = AlexPn 849 33 10
                                                          , typeInfo = NodeType ( SymType BoolType )
                                                          }
                                                        )
                                                      )
                                                      ( Expr
                                                        ( FunAppExpr "prime"
                                                          [ Expr
                                                            ( BinOpExpr PlusOp
                                                              ( Expr
                                                                ( ConstExpr "number" )
                                                                ( SemTag
                                                                  { posn = AlexPn 875 33 36
                                                                  , typeInfo = NodeType ( SymType IntType )
                                                                  }
                                                                )
                                                              )
                                                              ( Expr
                                                                ( IntCExpr 1 )
                                                                ( SemTag
                                                                  { posn = AlexPn 884 33 45
                                                                  , typeInfo = NodeType ( SymType IntType )
                                                                  }
                                                                )
                                                              )
                                                            )
                                                            ( SemTag
                                                              { posn = AlexPn 875 33 36
                                                              , typeInfo = NodeType ( SymType IntType )
                                                              }
                                                            )
                                                          ]
                                                        )
                                                        ( SemTag
                                                          { posn = AlexPn 868 33 29
                                                          , typeInfo = NodeType ( SymType BoolType )
                                                          }
                                                        )
                                                      )
                                                    )
                                                    ( SemTag
                                                      { posn = AlexPn 849 33 10
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
                                                                ( FunAppExpr "incr"
                                                                  [ Expr
                                                                    ( ConstExpr "counter" )
                                                                    ( SemTag
                                                                      { posn = AlexPn 917 35 14
                                                                      , typeInfo = NodeType
                                                                        ( SymType
                                                                          ( RefType ( SymType IntType ) )
                                                                        )
                                                                      }
                                                                    )
                                                                  ]
                                                                )
                                                                ( SemTag
                                                                  { posn = AlexPn 912 35 9
                                                                  , typeInfo = NodeType ( SymType UnitType )
                                                                  }
                                                                )
                                                              )
                                                              ( Expr
                                                                ( FunAppExpr "print_int"
                                                                  [ Expr
                                                                    ( BinOpExpr PlusOp
                                                                      ( Expr
                                                                        ( ConstExpr "number" )
                                                                        ( SemTag
                                                                          { posn = AlexPn 945 36 20
                                                                          , typeInfo = NodeType ( SymType IntType )
                                                                          }
                                                                        )
                                                                      )
                                                                      ( Expr
                                                                        ( IntCExpr 1 )
                                                                        ( SemTag
                                                                          { posn = AlexPn 954 36 29
                                                                          , typeInfo = NodeType ( SymType IntType )
                                                                          }
                                                                        )
                                                                      )
                                                                    )
                                                                    ( SemTag
                                                                      { posn = AlexPn 945 36 20
                                                                      , typeInfo = NodeType ( SymType IntType )
                                                                      }
                                                                    )
                                                                  ]
                                                                )
                                                                ( SemTag
                                                                  { posn = AlexPn 934 36 9
                                                                  , typeInfo = NodeType ( SymType UnitType )
                                                                  }
                                                                )
                                                              )
                                                            )
                                                            ( SemTag
                                                              { posn = AlexPn 912 35 9
                                                              , typeInfo = NodeType ( SymType UnitType )
                                                              }
                                                            )
                                                          )
                                                          ( Expr
                                                            ( FunAppExpr "print_string"
                                                              [ Expr
                                                                ( StringCExpr "\n" )
                                                                ( SemTag
                                                                  { posn = AlexPn 979 37 22
                                                                  , typeInfo = NodeType
                                                                    ( SymType
                                                                      ( ArrayType 1 ( SymType CharType ) )
                                                                    )
                                                                  }
                                                                )
                                                              ]
                                                            )
                                                            ( SemTag
                                                              { posn = AlexPn 966 37 9
                                                              , typeInfo = NodeType ( SymType UnitType )
                                                              }
                                                            )
                                                          )
                                                        )
                                                        ( SemTag
                                                          { posn = AlexPn 912 35 9
                                                          , typeInfo = NodeType ( SymType UnitType )
                                                          }
                                                        )
                                                      )
                                                    )
                                                    ( SemTag
                                                      { posn = AlexPn 898 34 7
                                                      , typeInfo = NodeType ( SymType UnitType )
                                                      }
                                                    )
                                                  )
                                                )
                                                ( SemTag
                                                  { posn = AlexPn 846 33 7
                                                  , typeInfo = NodeType ( SymType UnitType )
                                                  }
                                                )
                                              )
                                            )
                                            ( SemTag
                                              { posn = AlexPn 710 27 7
                                              , typeInfo = NodeType ( SymType UnitType )
                                              }
                                            )
                                          )
                                          ( Expr
                                            ( FunAppExpr "loop"
                                              [ Expr
                                                ( BinOpExpr PlusOp
                                                  ( Expr
                                                    ( ConstExpr "number" )
                                                    ( SemTag
                                                      { posn = AlexPn 1007 39 13
                                                      , typeInfo = NodeType ( SymType IntType )
                                                      }
                                                    )
                                                  )
                                                  ( Expr
                                                    ( IntCExpr 6 )
                                                    ( SemTag
                                                      { posn = AlexPn 1016 39 22
                                                      , typeInfo = NodeType ( SymType IntType )
                                                      }
                                                    )
                                                  )
                                                )
                                                ( SemTag
                                                  { posn = AlexPn 1007 39 13
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              ]
                                            )
                                            ( SemTag
                                              { posn = AlexPn 1001 39 7
                                              , typeInfo = NodeType ( SymType UnitType )
                                              }
                                            )
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 710 27 7
                                          , typeInfo = NodeType ( SymType UnitType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 698 26 5
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 670 25 5
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 652 24 11
                                , typeInfo = DefType
                                  ( MonoType
                                    ( SymType
                                      ( FunType ( SymType IntType ) ( SymType UnitType ) )
                                    )
                                  )
                                }
                              )
                            ]
                            ( SemTag
                              { posn = AlexPn 644 24 3, typeInfo = NotTypable }
                            )
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
                                            ( SemTag
                                              { posn = AlexPn 1037 41 8
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          ]
                                        )
                                        ( SemTag
                                          { posn = AlexPn 1032 41 3
                                          , typeInfo = NodeType ( SymType UnitType )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( FunAppExpr "print_string"
                                          [ Expr
                                            ( StringCExpr "\n" )
                                            ( SemTag
                                              { posn = AlexPn 1055 42 16
                                              , typeInfo = NodeType
                                                ( SymType
                                                  ( ArrayType 1 ( SymType CharType ) )
                                                )
                                              }
                                            )
                                          ]
                                        )
                                        ( SemTag
                                          { posn = AlexPn 1042 42 3
                                          , typeInfo = NodeType ( SymType UnitType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 1032 41 3
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                  ( Expr
                                    ( FunAppExpr "print_int"
                                      [ Expr
                                        ( UnOpExpr BangOp
                                          ( Expr
                                            ( ConstExpr "counter" )
                                            ( SemTag
                                              { posn = AlexPn 1074 43 14
                                              , typeInfo = NodeType
                                                ( SymType
                                                  ( RefType ( SymType IntType ) )
                                                )
                                              }
                                            )
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 1073 43 13
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      ]
                                    )
                                    ( SemTag
                                      { posn = AlexPn 1063 43 3
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 1032 41 3
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                              ( Expr
                                ( FunAppExpr "print_string"
                                  [ Expr
                                    ( StringCExpr " prime number(s) were found.\n" )
                                    ( SemTag
                                      { posn = AlexPn 1098 44 16
                                      , typeInfo = NodeType
                                        ( SymType
                                          ( ArrayType 1 ( SymType CharType ) )
                                        )
                                      }
                                    )
                                  ]
                                )
                                ( SemTag
                                  { posn = AlexPn 1085 44 3
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                            )
                            ( SemTag
                              { posn = AlexPn 1032 41 3
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 644 24 3
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 514 21 3
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                    ( SemTag
                      { posn = AlexPn 489 20 3
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                )
                ( SemTag
                  { posn = AlexPn 400 17 3
                  , typeInfo = NodeType ( SymType UnitType )
                  }
                )
              )
              ( SemTag
                { posn = AlexPn 371 16 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
          )
          ( SemTag
            { posn = AlexPn 322 15 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 313 14 5
          , typeInfo = DefType
            ( MonoType ( SymType UnitType ) )
          }
        )
      ]
      ( SemTag { posn = AlexPn 309 14 1, typeInfo = NotTypable } )
    )
  ]
  ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
