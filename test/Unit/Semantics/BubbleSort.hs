module Unit.Semantics.BubbleSort (bubbleSortSemAST) where

import Common.AST
import Common.SymbolType (SymbolType(..), TypeScheme(..))
import Lexer.Lexer (AlexPosn(..))
import Semantics.Utils (SemanticTag(..), TypeInfo(..))

bubbleSortSemAST :: AST SemanticTag
bubbleSortSemAST = AST
  [ Left
    ( Let
      [ FunDef "bsort"
        [ Param "x"
          ( SemTag
            { posn = AlexPn 10 1 11
            , typeInfo = NodeType
              ( SymType ( ArrayType 1 ( TVar 16 ) ) )
            }
          )
        ] Nothing
        ( LetIn
          ( Let
            [ FunDef "swap"
              [ Param "x"
                ( SemTag
                  { posn = AlexPn 25 2 12
                  , typeInfo = NodeType
                    ( SymType ( RefType ( TVar 5 ) ) )
                  }
                )
              , Param "y"
                ( SemTag
                  { posn = AlexPn 27 2 14
                  , typeInfo = NodeType
                    ( SymType ( RefType ( TVar 5 ) ) )
                  }
                )
              ] Nothing
              ( LetIn
                ( Let
                  [ FunDef "t" [] Nothing
                    ( Expr
                      ( UnOpExpr BangOp
                        ( Expr
                          ( ConstExpr "x" )
                          ( SemTag
                            { posn = AlexPn 44 3 14
                            , typeInfo = NodeType
                              ( SymType ( RefType ( TVar 5 ) ) )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 43 3 13
                        , typeInfo = NodeType
                          ( TVar 5 )
                        }
                      )
                    )
                    ( SemTag
                      { posn = AlexPn 39 3 9
                      , typeInfo = DefType
                        ( MonoType ( TVar 5 ) )
                      }
                    )
                  ]
                  ( SemTag { posn = AlexPn 35 3 5, typeInfo = NotTypable } )
                )
                ( Expr
                  ( BinOpExpr SemicolonOp
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr
                          ( ConstExpr "x" )
                          ( SemTag
                            { posn = AlexPn 49 3 19
                            , typeInfo = NodeType
                              ( SymType ( RefType ( TVar 5 ) ) )
                            }
                          )
                        )
                        ( Expr
                          ( UnOpExpr BangOp
                            ( Expr
                              ( ConstExpr "y" )
                              ( SemTag
                                { posn = AlexPn 55 3 25
                                , typeInfo = NodeType
                                  ( SymType ( RefType ( TVar 5 ) ) )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 54 3 24
                            , typeInfo = NodeType
                              ( TVar 5 )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 49 3 19
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr
                          ( ConstExpr "y" )
                          ( SemTag
                            { posn = AlexPn 58 3 28
                            , typeInfo = NodeType
                              ( SymType ( RefType ( TVar 5 ) ) )
                            }
                          )
                        )
                        ( Expr
                          ( ConstExpr "t" )
                          ( SemTag
                            { posn = AlexPn 63 3 33
                            , typeInfo = NodeType
                              ( TVar 5 )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 58 3 28
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 49 3 19
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
                ( SemTag
                  { posn = AlexPn 35 3 5
                  , typeInfo = NodeType ( SymType UnitType )
                  }
                )
              )
              ( SemTag
                { posn = AlexPn 20 2 7
                , typeInfo = DefType
                  ( AbsType 5
                    ( MonoType
                      ( SymType
                        ( FunType
                          ( SymType ( RefType ( TVar 5 ) ) )
                          ( SymType
                            ( FunType
                              ( SymType
                                ( RefType ( TVar 5 ) )
                              ) ( SymType UnitType )
                            )
                          )
                        )
                      )
                    )
                  )
                }
              )
            ]
            ( SemTag { posn = AlexPn 16 2 3, typeInfo = NotTypable } )
          )
          ( LetIn
            ( Let
              [ VarDef "changed" Nothing
                ( SemTag
                  { posn = AlexPn 74 4 7
                  , typeInfo = DefType
                    ( MonoType ( SymType ( RefType ( SymType BoolType ) ) ) )
                  }
                )
              ]
              ( SemTag { posn = AlexPn 70 4 3, typeInfo = NotTypable } )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( BinOpExpr AssignMutableOp
                    ( Expr
                      ( ConstExpr "changed" )
                      ( SemTag
                        { posn = AlexPn 95 5 3
                        , typeInfo = NodeType
                          ( SymType ( RefType ( SymType BoolType ) ) )
                        }
                      )
                    )
                    ( Expr TrueCExpr
                      ( SemTag
                        { posn = AlexPn 106 5 14
                        , typeInfo = NodeType ( SymType BoolType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 95 5 3
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
                ( Expr
                  ( WhileExpr
                    ( Expr
                      ( UnOpExpr BangOp
                        ( Expr
                          ( ConstExpr "changed" )
                          ( SemTag
                            { posn = AlexPn 121 6 10
                            , typeInfo = NodeType
                              ( SymType ( RefType ( SymType BoolType ) ) )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 120 6 9
                        , typeInfo = NodeType ( SymType BoolType )
                        }
                      )
                    )
                    ( Expr
                      ( BinOpExpr SemicolonOp
                        ( Expr
                          ( BinOpExpr AssignMutableOp
                            ( Expr
                              ( ConstExpr "changed" )
                              ( SemTag
                                { posn = AlexPn 136 7 5
                                , typeInfo = NodeType
                                  ( SymType ( RefType ( SymType BoolType ) ) )
                                }
                              )
                            )
                            ( Expr FalseCExpr
                              ( SemTag
                                { posn = AlexPn 147 7 16
                                , typeInfo = NodeType ( SymType BoolType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 136 7 5
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                        ( Expr
                          ( ForExpr "i"
                            ( Expr
                              ( IntCExpr 0 )
                              ( SemTag
                                { posn = AlexPn 166 8 13
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                            ( Expr
                              ( BinOpExpr MinusOp
                                ( Expr
                                  ( ArrayDim "x" 1 )
                                  ( SemTag
                                    { posn = AlexPn 171 8 18
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( IntCExpr 2 )
                                  ( SemTag
                                    { posn = AlexPn 179 8 26
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 171 8 18
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
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
                                              ( SemTag
                                                { posn = AlexPn 196 9 13
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            ]
                                          )
                                          ( SemTag
                                            { posn = AlexPn 194 9 11
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( RefType ( TVar 16 ) )
                                              )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 193 9 10
                                        , typeInfo = NodeType
                                          ( TVar 16 )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( UnOpExpr BangOp
                                        ( Expr
                                          ( ArrayAccess "x"
                                            [ Expr
                                              ( BinOpExpr PlusOp
                                                ( Expr
                                                  ( ConstExpr "i" )
                                                  ( SemTag
                                                    { posn = AlexPn 204 9 21
                                                    , typeInfo = NodeType ( SymType IntType )
                                                    }
                                                  )
                                                )
                                                ( Expr
                                                  ( IntCExpr 1 )
                                                  ( SemTag
                                                    { posn = AlexPn 206 9 23
                                                    , typeInfo = NodeType ( SymType IntType )
                                                    }
                                                  )
                                                )
                                              )
                                              ( SemTag
                                                { posn = AlexPn 204 9 21
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            ]
                                          )
                                          ( SemTag
                                            { posn = AlexPn 202 9 19
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( RefType ( TVar 16 ) )
                                              )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 201 9 18
                                        , typeInfo = NodeType
                                          ( TVar 16 )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 193 9 10
                                    , typeInfo = NodeType ( SymType BoolType )
                                    }
                                  )
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
                                                  ( SemTag
                                                    { posn = AlexPn 241 11 16
                                                    , typeInfo = NodeType ( SymType IntType )
                                                    }
                                                  )
                                                ]
                                              )
                                              ( SemTag
                                                { posn = AlexPn 239 11 14
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( RefType ( TVar 16 ) )
                                                  )
                                                }
                                              )
                                            , Expr
                                              ( ArrayAccess "x"
                                                [ Expr
                                                  ( BinOpExpr PlusOp
                                                    ( Expr
                                                      ( ConstExpr "i" )
                                                      ( SemTag
                                                        { posn = AlexPn 246 11 21
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    )
                                                    ( Expr
                                                      ( IntCExpr 1 )
                                                      ( SemTag
                                                        { posn = AlexPn 248 11 23
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    )
                                                  )
                                                  ( SemTag
                                                    { posn = AlexPn 246 11 21
                                                    , typeInfo = NodeType ( SymType IntType )
                                                    }
                                                  )
                                                ]
                                              )
                                              ( SemTag
                                                { posn = AlexPn 244 11 19
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( RefType ( TVar 16 ) )
                                                  )
                                                }
                                              )
                                            ]
                                          )
                                          ( SemTag
                                            { posn = AlexPn 234 11 9
                                            , typeInfo = NodeType ( SymType UnitType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( BinOpExpr AssignMutableOp
                                            ( Expr
                                              ( ConstExpr "changed" )
                                              ( SemTag
                                                { posn = AlexPn 260 12 9
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( RefType ( SymType BoolType ) )
                                                  )
                                                }
                                              )
                                            )
                                            ( Expr TrueCExpr
                                              ( SemTag
                                                { posn = AlexPn 271 12 20
                                                , typeInfo = NodeType ( SymType BoolType )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 260 12 9
                                            , typeInfo = NodeType ( SymType UnitType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 234 11 9
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 220 10 7
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 190 9 7
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 158 8 5
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 136 7 5
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 114 6 3
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 95 5 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag
              { posn = AlexPn 70 4 3, typeInfo = NodeType ( SymType UnitType ) }
            )
          )
          ( SemTag
            { posn = AlexPn 16 2 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 4 1 5
          , typeInfo = DefType
            ( AbsType 16
              ( MonoType
                ( SymType
                  ( FunType
                    ( SymType ( ArrayType 1 ( TVar 16 ) ) ) ( SymType UnitType )
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
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ FunDef "print_array"
              [ Param "msg"
                ( SemTag
                  { posn = AlexPn 332 18 19
                  , typeInfo = NodeType
                    ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                  }
                )
              , Param "x"
                ( SemTag
                  { posn = AlexPn 336 18 23
                  , typeInfo = NodeType
                    ( SymType ( ArrayType 1 ( SymType IntType ) ) )
                  }
                )
              ] Nothing
              ( Expr
                ( BinOpExpr SemicolonOp
                  ( Expr
                    ( BinOpExpr SemicolonOp
                      ( Expr
                        ( FunAppExpr "print_string"
                          [ Expr
                            ( ConstExpr "msg" )
                            ( SemTag
                              { posn = AlexPn 357 19 18
                              , typeInfo = NodeType
                                ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                              }
                            )
                          ]
                        )
                        ( SemTag
                          { posn = AlexPn 344 19 5
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      )
                      ( Expr
                        ( ForExpr "i"
                          ( Expr
                            ( IntCExpr 0 )
                            ( SemTag
                              { posn = AlexPn 374 20 13
                              , typeInfo = NodeType ( SymType IntType )
                              }
                            )
                          )
                          ( Expr
                            ( BinOpExpr MinusOp
                              ( Expr
                                ( ArrayDim "x" 1 )
                                ( SemTag
                                  { posn = AlexPn 379 20 18
                                  , typeInfo = NodeType ( SymType IntType )
                                  }
                                )
                              )
                              ( Expr
                                ( IntCExpr 1 )
                                ( SemTag
                                  { posn = AlexPn 387 20 26
                                  , typeInfo = NodeType ( SymType IntType )
                                  }
                                )
                              )
                            )
                            ( SemTag
                              { posn = AlexPn 379 20 18
                              , typeInfo = NodeType ( SymType IntType )
                              }
                            )
                          )
                          ( Expr
                            ( BinOpExpr SemicolonOp
                              ( Expr
                                ( IfThenExpr
                                  ( Expr
                                    ( BinOpExpr GTOp
                                      ( Expr
                                        ( ConstExpr "i" )
                                        ( SemTag
                                          { posn = AlexPn 401 21 10
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( IntCExpr 0 )
                                        ( SemTag
                                          { posn = AlexPn 405 21 14
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 401 21 10
                                      , typeInfo = NodeType ( SymType BoolType )
                                      }
                                    )
                                  )
                                  ( Expr
                                    ( FunAppExpr "print_string"
                                      [ Expr
                                        ( StringCExpr ", " )
                                        ( SemTag
                                          { posn = AlexPn 425 21 34
                                          , typeInfo = NodeType
                                            ( SymType
                                              ( ArrayType 1 ( SymType CharType ) )
                                            )
                                          }
                                        )
                                      ]
                                    )
                                    ( SemTag
                                      { posn = AlexPn 412 21 21
                                      , typeInfo = NodeType ( SymType UnitType )
                                      }
                                    )
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 398 21 7
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                              ( Expr
                                ( FunAppExpr "print_int"
                                  [ Expr
                                    ( UnOpExpr BangOp
                                      ( Expr
                                        ( ArrayAccess "x"
                                          [ Expr
                                            ( ConstExpr "i" )
                                            ( SemTag
                                              { posn = AlexPn 450 22 20
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          ]
                                        )
                                        ( SemTag
                                          { posn = AlexPn 448 22 18
                                          , typeInfo = NodeType
                                            ( SymType
                                              ( RefType ( SymType IntType ) )
                                            )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 447 22 17
                                      , typeInfo = NodeType ( SymType IntType )
                                      }
                                    )
                                  ]
                                )
                                ( SemTag
                                  { posn = AlexPn 437 22 7
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                            )
                            ( SemTag
                              { posn = AlexPn 398 21 7
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                        )
                        ( SemTag
                          { posn = AlexPn 366 20 5
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      )
                    )
                    ( SemTag
                      { posn = AlexPn 344 19 5
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                  ( Expr
                    ( FunAppExpr "print_string"
                      [ Expr
                        ( StringCExpr "\n" )
                        ( SemTag
                          { posn = AlexPn 480 24 18
                          , typeInfo = NodeType
                            ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                          }
                        )
                      ]
                    )
                    ( SemTag
                      { posn = AlexPn 467 24 5
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                )
                ( SemTag
                  { posn = AlexPn 344 19 5
                  , typeInfo = NodeType ( SymType UnitType )
                  }
                )
              )
              ( SemTag
                { posn = AlexPn 320 18 7
                , typeInfo = DefType
                  ( MonoType
                    ( SymType
                      ( FunType
                        ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                        ( SymType
                          ( FunType
                            ( SymType
                              ( ArrayType 1 ( SymType IntType ) )
                            ) ( SymType UnitType )
                          )
                        )
                      )
                    )
                  )
                }
              )
            ]
            ( SemTag { posn = AlexPn 316 18 3, typeInfo = NotTypable } )
          )
          ( LetIn
            ( Let
              [ VarDef "seed" Nothing
                ( SemTag
                  { posn = AlexPn 495 26 7
                  , typeInfo = DefType
                    ( MonoType ( SymType ( RefType ( SymType IntType ) ) ) )
                  }
                )
              , ArrayDef "x"
                [ Expr
                  ( IntCExpr 16 )
                  ( SemTag
                    { posn = AlexPn 524 27 17
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                ] Nothing
                ( SemTag
                  { posn = AlexPn 514 27 7
                  , typeInfo = DefType
                    ( MonoType ( SymType ( ArrayType 1 ( SymType IntType ) ) ) )
                  }
                )
              ]
              ( SemTag { posn = AlexPn 491 26 3, typeInfo = NotTypable } )
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
                                  ( SemTag
                                    { posn = AlexPn 533 28 3
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
                                    { posn = AlexPn 541 28 11
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 533 28 3
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                            ( Expr
                              ( ForExpr "i"
                                ( Expr
                                  ( IntCExpr 0 )
                                  ( SemTag
                                    { posn = AlexPn 555 29 11
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( IntCExpr 15 )
                                  ( SemTag
                                    { posn = AlexPn 560 29 16
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
                                            { posn = AlexPn 570 30 5
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
                                                                { posn = AlexPn 580 30 15
                                                                , typeInfo = NodeType
                                                                  ( SymType
                                                                    ( RefType ( SymType IntType ) )
                                                                  )
                                                                }
                                                              )
                                                            )
                                                          )
                                                          ( SemTag
                                                            { posn = AlexPn 579 30 14
                                                            , typeInfo = NodeType ( SymType IntType )
                                                            }
                                                          )
                                                        )
                                                        ( Expr
                                                          ( IntCExpr 137 )
                                                          ( SemTag
                                                            { posn = AlexPn 587 30 22
                                                            , typeInfo = NodeType ( SymType IntType )
                                                            }
                                                          )
                                                        )
                                                      )
                                                      ( SemTag
                                                        { posn = AlexPn 579 30 14
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    )
                                                    ( Expr
                                                      ( IntCExpr 220 )
                                                      ( SemTag
                                                        { posn = AlexPn 593 30 28
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    )
                                                  )
                                                  ( SemTag
                                                    { posn = AlexPn 579 30 14
                                                    , typeInfo = NodeType ( SymType IntType )
                                                    }
                                                  )
                                                )
                                                ( Expr
                                                  ( ConstExpr "i" )
                                                  ( SemTag
                                                    { posn = AlexPn 599 30 34
                                                    , typeInfo = NodeType ( SymType IntType )
                                                    }
                                                  )
                                                )
                                              )
                                              ( SemTag
                                                { posn = AlexPn 579 30 14
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            )
                                            ( Expr
                                              ( IntCExpr 101 )
                                              ( SemTag
                                                { posn = AlexPn 606 30 41
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 578 30 13
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 570 30 5
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( BinOpExpr AssignMutableOp
                                        ( Expr
                                          ( ArrayAccess "x"
                                            [ Expr
                                              ( ConstExpr "i" )
                                              ( SemTag
                                                { posn = AlexPn 617 31 7
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            ]
                                          )
                                          ( SemTag
                                            { posn = AlexPn 615 31 5
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( RefType ( SymType IntType ) )
                                              )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( UnOpExpr BangOp
                                            ( Expr
                                              ( ConstExpr "seed" )
                                              ( SemTag
                                                { posn = AlexPn 624 31 14
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( RefType ( SymType IntType ) )
                                                  )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 623 31 13
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 615 31 5
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 570 30 5
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 547 29 3
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 533 28 3
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                        ( Expr
                          ( FunAppExpr "print_array"
                            [ Expr
                              ( StringCExpr "Initial array: " )
                              ( SemTag
                                { posn = AlexPn 651 33 15
                                , typeInfo = NodeType
                                  ( SymType
                                    ( ArrayType 1 ( SymType CharType ) )
                                  )
                                }
                              )
                            , Expr
                              ( ConstExpr "x" )
                              ( SemTag
                                { posn = AlexPn 669 33 33
                                , typeInfo = NodeType
                                  ( SymType
                                    ( ArrayType 1 ( SymType IntType ) )
                                  )
                                }
                              )
                            ]
                          )
                          ( SemTag
                            { posn = AlexPn 639 33 3
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 533 28 3
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                    ( Expr
                      ( FunAppExpr "bsort"
                        [ Expr
                          ( ConstExpr "x" )
                          ( SemTag
                            { posn = AlexPn 680 34 9
                            , typeInfo = NodeType
                              ( SymType ( ArrayType 1 ( SymType IntType ) ) )
                            }
                          )
                        ]
                      )
                      ( SemTag
                        { posn = AlexPn 674 34 3
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 533 28 3
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
                ( Expr
                  ( FunAppExpr "print_array"
                    [ Expr
                      ( StringCExpr "Sorted array: " )
                      ( SemTag
                        { posn = AlexPn 697 35 15
                        , typeInfo = NodeType
                          ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                        }
                      )
                    , Expr
                      ( ConstExpr "x" )
                      ( SemTag
                        { posn = AlexPn 714 35 32
                        , typeInfo = NodeType
                          ( SymType ( ArrayType 1 ( SymType IntType ) ) )
                        }
                      )
                    ]
                  )
                  ( SemTag
                    { posn = AlexPn 685 35 3
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 533 28 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag
              { posn = AlexPn 491 26 3
              , typeInfo = NodeType ( SymType UnitType )
              }
            )
          )
          ( SemTag
            { posn = AlexPn 316 18 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 307 17 5
          , typeInfo = DefType
            ( MonoType ( SymType UnitType ) )
          }
        )
      ]
      ( SemTag { posn = AlexPn 303 17 1, typeInfo = NotTypable } )
    )
  ]
  ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
