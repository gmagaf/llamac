module Unit.Semantics.HanoiType (hanoiTypeSemAST) where

import Common.AST
import Common.Source (Source(FileIn))
import Common.SymbolType (SymbolType(..), TypeScheme(..), PosnId (..))
import Lexer.Lexer (AlexPosn(..))
import Semantics.Utils (SemanticTag(..), TypeInfo(..))

hanoiTypeSemAST :: AST SemanticTag
hanoiTypeSemAST = AST
  [ Right
    ( TypeDef
      [ TDef "pile"
        [ Constr "Left" []
          ( SemTag { posn = AlexPn 12 1 13, typeInfo = NotTypable } )
        , Constr "Middle" []
          ( SemTag { posn = AlexPn 19 1 20, typeInfo = NotTypable } )
        , Constr "Right" []
          ( SemTag { posn = AlexPn 28 1 29, typeInfo = NotTypable } )
        ]
        ( SemTag { posn = AlexPn 5 1 6, typeInfo = NotTypable } )
      ]
      ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
    )
  , Left
    ( Let
      [ FunDef "print_pile"
        [ Param "pile"
          ( SemTag
            { posn = AlexPn 50 3 16
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "pile"
                    , def_source = FileIn "./test/resources/hanoiType.llama"
                    , def_posn = AlexPn 5 1 6
                    }
                  )
                )
              )
            }
          )
        ] Nothing
        ( MatchExpr
          ( Expr
            ( ConstExpr "pile" )
            ( SemTag
              { posn = AlexPn 65 4 9
              , typeInfo = NodeType
                ( SymType
                  ( UserDefinedType
                    ( PosnId
                      { identifier = "pile"
                      , def_source = FileIn "./test/resources/hanoiType.llama"
                      , def_posn = AlexPn 5 1 6
                      }
                    )
                  )
                )
              }
            )
          )
          [ Match
            ( Pattern
              ( ConstrPattern "Left" [] )
              ( SemTag
                { posn = AlexPn 79 5 5
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "pile"
                        , def_source = FileIn "./test/resources/hanoiType.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr
              ( FunAppExpr "print_string"
                [ Expr
                  ( StringCExpr "left" )
                  ( SemTag
                    { posn = AlexPn 102 5 28
                    , typeInfo = NodeType
                      ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                    }
                  )
                ]
              )
              ( SemTag
                { posn = AlexPn 89 5 15
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag { posn = AlexPn 79 5 5, typeInfo = NotTypable } )
          , Match
            ( Pattern
              ( ConstrPattern "Middle" [] )
              ( SemTag
                { posn = AlexPn 113 6 5
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "pile"
                        , def_source = FileIn "./test/resources/hanoiType.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr
              ( FunAppExpr "print_string"
                [ Expr
                  ( StringCExpr "middle" )
                  ( SemTag
                    { posn = AlexPn 136 6 28
                    , typeInfo = NodeType
                      ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                    }
                  )
                ]
              )
              ( SemTag
                { posn = AlexPn 123 6 15
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag { posn = AlexPn 113 6 5, typeInfo = NotTypable } )
          , Match
            ( Pattern
              ( ConstrPattern "Right" [] )
              ( SemTag
                { posn = AlexPn 149 7 5
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "pile"
                        , def_source = FileIn "./test/resources/hanoiType.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr
              ( FunAppExpr "print_string"
                [ Expr
                  ( StringCExpr "right" )
                  ( SemTag
                    { posn = AlexPn 172 7 28
                    , typeInfo = NodeType
                      ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                    }
                  )
                ]
              )
              ( SemTag
                { posn = AlexPn 159 7 15
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag { posn = AlexPn 149 7 5, typeInfo = NotTypable } )
          ]
          ( SemTag
            { posn = AlexPn 59 4 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 39 3 5
          , typeInfo = DefType
            ( MonoType
              ( SymType
                ( FunType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "pile"
                        , def_source = FileIn "./test/resources/hanoiType.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  ) ( SymType UnitType )
                )
              )
            )
          }
        )
      ]
      ( SemTag { posn = AlexPn 35 3 1, typeInfo = NotTypable } )
    )
  , Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ FunDef "move"
              [ Param "source"
                ( SemTag
                  { posn = AlexPn 209 11 12
                  , typeInfo = NodeType
                    ( SymType
                      ( UserDefinedType
                        ( PosnId
                          { identifier = "pile"
                          , def_source = FileIn "./test/resources/hanoiType.llama"
                          , def_posn = AlexPn 5 1 6
                          }
                        )
                      )
                    )
                  }
                )
              , Param "target"
                ( SemTag
                  { posn = AlexPn 216 11 19
                  , typeInfo = NodeType
                    ( SymType
                      ( UserDefinedType
                        ( PosnId
                          { identifier = "pile"
                          , def_source = FileIn "./test/resources/hanoiType.llama"
                          , def_posn = AlexPn 5 1 6
                          }
                        )
                      )
                    )
                  }
                )
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
                                    ( SemTag
                                      { posn = AlexPn 242 12 18
                                      , typeInfo = NodeType
                                        ( SymType
                                          ( ArrayType 1 ( SymType CharType ) )
                                        )
                                      }
                                    )
                                  ]
                                )
                                ( SemTag
                                  { posn = AlexPn 229 12 5
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                              ( Expr
                                ( FunAppExpr "print_pile"
                                  [ Expr
                                    ( ConstExpr "source" )
                                    ( SemTag
                                      { posn = AlexPn 274 13 16
                                      , typeInfo = NodeType
                                        ( SymType
                                          ( UserDefinedType
                                            ( PosnId
                                              { identifier = "pile"
                                              , def_source = FileIn "./test/resources/hanoiType.llama"
                                              , def_posn = AlexPn 5 1 6
                                              }
                                            )
                                          )
                                        )
                                      }
                                    )
                                  ]
                                )
                                ( SemTag
                                  { posn = AlexPn 263 13 5
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                            )
                            ( SemTag
                              { posn = AlexPn 229 12 5
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                          ( Expr
                            ( FunAppExpr "print_string"
                              [ Expr
                                ( StringCExpr " to " )
                                ( SemTag
                                  { posn = AlexPn 299 14 18
                                  , typeInfo = NodeType
                                    ( SymType
                                      ( ArrayType 1 ( SymType CharType ) )
                                    )
                                  }
                                )
                              ]
                            )
                            ( SemTag
                              { posn = AlexPn 286 14 5
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                        )
                        ( SemTag
                          { posn = AlexPn 229 12 5
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      )
                      ( Expr
                        ( FunAppExpr "print_pile"
                          [ Expr
                            ( ConstExpr "target" )
                            ( SemTag
                              { posn = AlexPn 322 15 16
                              , typeInfo = NodeType
                                ( SymType
                                  ( UserDefinedType
                                    ( PosnId
                                      { identifier = "pile"
                                      , def_source = FileIn "./test/resources/hanoiType.llama"
                                      , def_posn = AlexPn 5 1 6
                                      }
                                    )
                                  )
                                )
                              }
                            )
                          ]
                        )
                        ( SemTag
                          { posn = AlexPn 311 15 5
                          , typeInfo = NodeType ( SymType UnitType )
                          }
                        )
                      )
                    )
                    ( SemTag
                      { posn = AlexPn 229 12 5
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                  ( Expr
                    ( FunAppExpr "print_string"
                      [ Expr
                        ( StringCExpr "\n" )
                        ( SemTag
                          { posn = AlexPn 347 16 18
                          , typeInfo = NodeType
                            ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                          }
                        )
                      ]
                    )
                    ( SemTag
                      { posn = AlexPn 334 16 5
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                )
                ( SemTag
                  { posn = AlexPn 229 12 5
                  , typeInfo = NodeType ( SymType UnitType )
                  }
                )
              )
              ( SemTag
                { posn = AlexPn 204 11 7
                , typeInfo = DefType
                  ( MonoType
                    ( SymType
                      ( FunType
                        ( SymType
                          ( UserDefinedType
                            ( PosnId
                              { identifier = "pile"
                              , def_source = FileIn "./test/resources/hanoiType.llama"
                              , def_posn = AlexPn 5 1 6
                              }
                            )
                          )
                        )
                        ( SymType
                          ( FunType
                            ( SymType
                              ( UserDefinedType
                                ( PosnId
                                  { identifier = "pile"
                                  , def_source = FileIn "./test/resources/hanoiType.llama"
                                  , def_posn = AlexPn 5 1 6
                                  }
                                )
                              )
                            ) ( SymType UnitType )
                          )
                        )
                      )
                    )
                  )
                }
              )
            ]
            ( SemTag { posn = AlexPn 200 11 3, typeInfo = NotTypable } )
          )
          ( LetIn
            ( LetRec
              [ FunDef "hanoi"
                [ Param "rings"
                  ( SemTag
                    { posn = AlexPn 371 17 17
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                , Param "source"
                  ( SemTag
                    { posn = AlexPn 377 17 23
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "pile"
                            , def_source = FileIn "./test/resources/hanoiType.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                , Param "target"
                  ( SemTag
                    { posn = AlexPn 384 17 30
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "pile"
                            , def_source = FileIn "./test/resources/hanoiType.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                , Param "auxil"
                  ( SemTag
                    { posn = AlexPn 391 17 37
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "pile"
                            , def_source = FileIn "./test/resources/hanoiType.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                ] Nothing
                ( Expr
                  ( IfThenExpr
                    ( Expr
                      ( BinOpExpr GTOp
                        ( Expr
                          ( ConstExpr "rings" )
                          ( SemTag
                            { posn = AlexPn 406 18 8
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( IntCExpr 0 )
                          ( SemTag
                            { posn = AlexPn 414 18 16
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 406 18 8
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
                                  ( FunAppExpr "hanoi"
                                    [ Expr
                                      ( BinOpExpr MinusOp
                                        ( Expr
                                          ( ConstExpr "rings" )
                                          ( SemTag
                                            { posn = AlexPn 444 20 14
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( IntCExpr 1 )
                                          ( SemTag
                                            { posn = AlexPn 450 20 20
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 444 20 14
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    , Expr
                                      ( ConstExpr "source" )
                                      ( SemTag
                                        { posn = AlexPn 453 20 23
                                        , typeInfo = NodeType
                                          ( SymType
                                            ( UserDefinedType
                                              ( PosnId
                                                { identifier = "pile"
                                                , def_source = FileIn "./test/resources/hanoiType.llama"
                                                , def_posn = AlexPn 5 1 6
                                                }
                                              )
                                            )
                                          )
                                        }
                                      )
                                    , Expr
                                      ( ConstExpr "auxil" )
                                      ( SemTag
                                        { posn = AlexPn 460 20 30
                                        , typeInfo = NodeType
                                          ( SymType
                                            ( UserDefinedType
                                              ( PosnId
                                                { identifier = "pile"
                                                , def_source = FileIn "./test/resources/hanoiType.llama"
                                                , def_posn = AlexPn 5 1 6
                                                }
                                              )
                                            )
                                          )
                                        }
                                      )
                                    , Expr
                                      ( ConstExpr "target" )
                                      ( SemTag
                                        { posn = AlexPn 466 20 36
                                        , typeInfo = NodeType
                                          ( SymType
                                            ( UserDefinedType
                                              ( PosnId
                                                { identifier = "pile"
                                                , def_source = FileIn "./test/resources/hanoiType.llama"
                                                , def_posn = AlexPn 5 1 6
                                                }
                                              )
                                            )
                                          )
                                        }
                                      )
                                    ]
                                  )
                                  ( SemTag
                                    { posn = AlexPn 437 20 7
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( FunAppExpr "move"
                                    [ Expr
                                      ( ConstExpr "source" )
                                      ( SemTag
                                        { posn = AlexPn 485 21 12
                                        , typeInfo = NodeType
                                          ( SymType
                                            ( UserDefinedType
                                              ( PosnId
                                                { identifier = "pile"
                                                , def_source = FileIn "./test/resources/hanoiType.llama"
                                                , def_posn = AlexPn 5 1 6
                                                }
                                              )
                                            )
                                          )
                                        }
                                      )
                                    , Expr
                                      ( ConstExpr "target" )
                                      ( SemTag
                                        { posn = AlexPn 492 21 19
                                        , typeInfo = NodeType
                                          ( SymType
                                            ( UserDefinedType
                                              ( PosnId
                                                { identifier = "pile"
                                                , def_source = FileIn "./test/resources/hanoiType.llama"
                                                , def_posn = AlexPn 5 1 6
                                                }
                                              )
                                            )
                                          )
                                        }
                                      )
                                    ]
                                  )
                                  ( SemTag
                                    { posn = AlexPn 480 21 7
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 437 20 7
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                            ( Expr
                              ( FunAppExpr "hanoi"
                                [ Expr
                                  ( BinOpExpr MinusOp
                                    ( Expr
                                      ( ConstExpr "rings" )
                                      ( SemTag
                                        { posn = AlexPn 513 22 14
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( IntCExpr 1 )
                                      ( SemTag
                                        { posn = AlexPn 519 22 20
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 513 22 14
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                , Expr
                                  ( ConstExpr "auxil" )
                                  ( SemTag
                                    { posn = AlexPn 522 22 23
                                    , typeInfo = NodeType
                                      ( SymType
                                        ( UserDefinedType
                                          ( PosnId
                                            { identifier = "pile"
                                            , def_source = FileIn "./test/resources/hanoiType.llama"
                                            , def_posn = AlexPn 5 1 6
                                            }
                                          )
                                        )
                                      )
                                    }
                                  )
                                , Expr
                                  ( ConstExpr "target" )
                                  ( SemTag
                                    { posn = AlexPn 528 22 29
                                    , typeInfo = NodeType
                                      ( SymType
                                        ( UserDefinedType
                                          ( PosnId
                                            { identifier = "pile"
                                            , def_source = FileIn "./test/resources/hanoiType.llama"
                                            , def_posn = AlexPn 5 1 6
                                            }
                                          )
                                        )
                                      )
                                    }
                                  )
                                , Expr
                                  ( ConstExpr "source" )
                                  ( SemTag
                                    { posn = AlexPn 535 22 36
                                    , typeInfo = NodeType
                                      ( SymType
                                        ( UserDefinedType
                                          ( PosnId
                                            { identifier = "pile"
                                            , def_source = FileIn "./test/resources/hanoiType.llama"
                                            , def_posn = AlexPn 5 1 6
                                            }
                                          )
                                        )
                                      )
                                    }
                                  )
                                ]
                              )
                              ( SemTag
                                { posn = AlexPn 506 22 7
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 437 20 7
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 425 19 5
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 403 18 5
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
                ( SemTag
                  { posn = AlexPn 365 17 11
                  , typeInfo = DefType
                    ( MonoType
                      ( SymType
                        ( FunType ( SymType IntType )
                          ( SymType
                            ( FunType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "pile"
                                    , def_source = FileIn "./test/resources/hanoiType.llama"
                                    , def_posn = AlexPn 5 1 6
                                    }
                                  )
                                )
                              )
                              ( SymType
                                ( FunType
                                  ( SymType
                                    ( UserDefinedType
                                      ( PosnId
                                        { identifier = "pile"
                                        , def_source = FileIn "./test/resources/hanoiType.llama"
                                        , def_posn = AlexPn 5 1 6
                                        }
                                      )
                                    )
                                  )
                                  ( SymType
                                    ( FunType
                                      ( SymType
                                        ( UserDefinedType
                                          ( PosnId
                                            { identifier = "pile"
                                            , def_source = FileIn "./test/resources/hanoiType.llama"
                                            , def_posn = AlexPn 5 1 6
                                            }
                                          )
                                        )
                                      ) ( SymType UnitType )
                                    )
                                  )
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
              ( SemTag { posn = AlexPn 357 17 3, typeInfo = NotTypable } )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( FunAppExpr "print_string"
                    [ Expr
                      ( StringCExpr "Please, give the number of rings: " )
                      ( SemTag
                        { posn = AlexPn 568 24 16
                        , typeInfo = NodeType
                          ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                        }
                      )
                    ]
                  )
                  ( SemTag
                    { posn = AlexPn 555 24 3
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
                              { posn = AlexPn 625 25 20
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          ]
                        )
                        ( SemTag
                          { posn = AlexPn 616 25 11
                          , typeInfo = NodeType ( SymType IntType )
                          }
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 612 25 7
                        , typeInfo = DefType
                          ( MonoType ( SymType IntType ) )
                        }
                      )
                    ]
                    ( SemTag { posn = AlexPn 608 25 3, typeInfo = NotTypable } )
                  )
                  ( Expr
                    ( FunAppExpr "hanoi"
                      [ Expr
                        ( ConstExpr "n" )
                        ( SemTag
                          { posn = AlexPn 639 26 9
                          , typeInfo = NodeType ( SymType IntType )
                          }
                        )
                      , Expr
                        ( ConstConstrExpr "Left" )
                        ( SemTag
                          { posn = AlexPn 641 26 11
                          , typeInfo = NodeType
                            ( SymType
                              ( UserDefinedType
                                ( PosnId
                                  { identifier = "pile"
                                  , def_source = FileIn "./test/resources/hanoiType.llama"
                                  , def_posn = AlexPn 5 1 6
                                  }
                                )
                              )
                            )
                          }
                        )
                      , Expr
                        ( ConstConstrExpr "Right" )
                        ( SemTag
                          { posn = AlexPn 646 26 16
                          , typeInfo = NodeType
                            ( SymType
                              ( UserDefinedType
                                ( PosnId
                                  { identifier = "pile"
                                  , def_source = FileIn "./test/resources/hanoiType.llama"
                                  , def_posn = AlexPn 5 1 6
                                  }
                                )
                              )
                            )
                          }
                        )
                      , Expr
                        ( ConstConstrExpr "Middle" )
                        ( SemTag
                          { posn = AlexPn 652 26 22
                          , typeInfo = NodeType
                            ( SymType
                              ( UserDefinedType
                                ( PosnId
                                  { identifier = "pile"
                                  , def_source = FileIn "./test/resources/hanoiType.llama"
                                  , def_posn = AlexPn 5 1 6
                                  }
                                )
                              )
                            )
                          }
                        )
                      ]
                    )
                    ( SemTag
                      { posn = AlexPn 633 26 3
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 608 25 3
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 555 24 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag
              { posn = AlexPn 357 17 3
              , typeInfo = NodeType ( SymType UnitType )
              }
            )
          )
          ( SemTag
            { posn = AlexPn 200 11 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 191 10 5
          , typeInfo = DefType
            ( MonoType ( SymType UnitType ) )
          }
        )
      ]
      ( SemTag { posn = AlexPn 187 10 1, typeInfo = NotTypable } )
    )
  ]
  ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
