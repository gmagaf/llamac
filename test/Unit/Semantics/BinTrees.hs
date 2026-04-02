module Unit.Semantics.BinTrees (binTreesSemAST) where

import Common.AST
import Common.Source (Source(FileIn))
import Common.SymbolType (SymbolType(..), TypeScheme(..), PosnId (..))
import Lexer.Lexer (AlexPosn(..))
import Semantics.Utils (SemanticTag(..), TypeInfo(..))

binTreesSemAST :: AST SemanticTag
binTreesSemAST = AST
  [ Right
    ( TypeDef
      [ TDef "tree"
        [ Constr "Nil" []
          ( SemTag { posn = AlexPn 12 1 13, typeInfo = NotTypable } )
        , Constr "Node"
          [ Type IntType
            ( SemTag { posn = AlexPn 26 1 27, typeInfo = NotTypable } )
          , Type
            ( UserDefinedType "tree" )
            ( SemTag { posn = AlexPn 30 1 31, typeInfo = NotTypable } )
          , Type
            ( UserDefinedType "tree" )
            ( SemTag { posn = AlexPn 35 1 36, typeInfo = NotTypable } )
          ]
          ( SemTag { posn = AlexPn 18 1 19, typeInfo = NotTypable } )
        ]
        ( SemTag { posn = AlexPn 5 1 6, typeInfo = NotTypable } )
      ]
      ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
    )
  , Left
    ( LetRec
      [ FunDef "treeInsert"
        [ Param "t"
          ( SemTag
            { posn = AlexPn 60 3 20
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "tree"
                    , def_source = FileIn "./test/resources/binTrees.llama"
                    , def_posn = AlexPn 5 1 6
                    }
                  )
                )
              )
            }
          )
        , Param "n"
          ( SemTag
            { posn = AlexPn 62 3 22, typeInfo = NodeType ( SymType IntType ) }
          )
        ] Nothing
        ( MatchExpr
          ( Expr
            ( ConstExpr "t" )
            ( SemTag
              { posn = AlexPn 74 4 9
              , typeInfo = NodeType
                ( SymType
                  ( UserDefinedType
                    ( PosnId
                      { identifier = "tree"
                      , def_source = FileIn "./test/resources/binTrees.llama"
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
              ( ConstrPattern "Nil" [] )
              ( SemTag
                { posn = AlexPn 87 5 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr
              ( ConstrAppExpr "Node"
                [ Expr
                  ( ConstExpr "n" )
                  ( SemTag
                    { posn = AlexPn 108 5 28
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                , Expr
                  ( ConstConstrExpr "Nil" )
                  ( SemTag
                    { posn = AlexPn 110 5 30
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                , Expr
                  ( ConstConstrExpr "Nil" )
                  ( SemTag
                    { posn = AlexPn 114 5 34
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
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
                { posn = AlexPn 103 5 23
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( SemTag { posn = AlexPn 87 5 7, typeInfo = NotTypable } )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "m" )
                  ( SemTag
                    { posn = AlexPn 129 6 12
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                , Pattern
                  ( IdPattern "t1" )
                  ( SemTag
                    { posn = AlexPn 131 6 14
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                , Pattern
                  ( IdPattern "t2" )
                  ( SemTag
                    { posn = AlexPn 134 6 17
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
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
                { posn = AlexPn 124 6 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
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
                        { posn = AlexPn 148 6 31
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( Expr
                      ( ConstExpr "m" )
                      ( SemTag
                        { posn = AlexPn 152 6 35
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 148 6 31
                    , typeInfo = NodeType ( SymType BoolType )
                    }
                  )
                )
                ( Expr
                  ( ConstrAppExpr "Node"
                    [ Expr
                      ( ConstExpr "m" )
                      ( SemTag
                        { posn = AlexPn 164 6 47
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    , Expr
                      ( FunAppExpr "treeInsert"
                        [ Expr
                          ( ConstExpr "t1" )
                          ( SemTag
                            { posn = AlexPn 178 6 61
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
                                    , def_posn = AlexPn 5 1 6
                                    }
                                  )
                                )
                              )
                            }
                          )
                        , Expr
                          ( ConstExpr "n" )
                          ( SemTag
                            { posn = AlexPn 181 6 64
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        ]
                      )
                      ( SemTag
                        { posn = AlexPn 167 6 50
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
                                , def_posn = AlexPn 5 1 6
                                }
                              )
                            )
                          )
                        }
                      )
                    , Expr
                      ( ConstExpr "t2" )
                      ( SemTag
                        { posn = AlexPn 184 6 67
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
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
                    { posn = AlexPn 159 6 42
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                )
                ( Expr
                  ( IfThenElseExpr
                    ( Expr
                      ( BinOpExpr GTOp
                        ( Expr
                          ( ConstExpr "n" )
                          ( SemTag
                            { posn = AlexPn 217 7 31
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( ConstExpr "m" )
                          ( SemTag
                            { posn = AlexPn 221 7 35
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 217 7 31
                        , typeInfo = NodeType ( SymType BoolType )
                        }
                      )
                    )
                    ( Expr
                      ( ConstrAppExpr "Node"
                        [ Expr
                          ( ConstExpr "m" )
                          ( SemTag
                            { posn = AlexPn 233 7 47
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        , Expr
                          ( ConstExpr "t1" )
                          ( SemTag
                            { posn = AlexPn 235 7 49
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
                                    , def_posn = AlexPn 5 1 6
                                    }
                                  )
                                )
                              )
                            }
                          )
                        , Expr
                          ( FunAppExpr "treeInsert"
                            [ Expr
                              ( ConstExpr "t2" )
                              ( SemTag
                                { posn = AlexPn 250 7 64
                                , typeInfo = NodeType
                                  ( SymType
                                    ( UserDefinedType
                                      ( PosnId
                                        { identifier = "tree"
                                        , def_source = FileIn "./test/resources/binTrees.llama"
                                        , def_posn = AlexPn 5 1 6
                                        }
                                      )
                                    )
                                  )
                                }
                              )
                            , Expr
                              ( ConstExpr "n" )
                              ( SemTag
                                { posn = AlexPn 253 7 67
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            ]
                          )
                          ( SemTag
                            { posn = AlexPn 239 7 53
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
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
                        { posn = AlexPn 228 7 42
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
                                , def_posn = AlexPn 5 1 6
                                }
                              )
                            )
                          )
                        }
                      )
                    )
                    ( Expr
                      ( ConstExpr "t" )
                      ( SemTag
                        { posn = AlexPn 283 8 28
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
                                , def_posn = AlexPn 5 1 6
                                }
                              )
                            )
                          )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 214 7 28
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 145 6 28
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( SemTag { posn = AlexPn 124 6 7, typeInfo = NotTypable } )
          ]
          ( SemTag
            { posn = AlexPn 68 4 3
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "tree"
                    , def_source = FileIn "./test/resources/binTrees.llama"
                    , def_posn = AlexPn 5 1 6
                    }
                  )
                )
              )
            }
          )
        )
        ( SemTag
          { posn = AlexPn 49 3 9
          , typeInfo = DefType
            ( MonoType
              ( SymType
                ( FunType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                  ( SymType
                    ( FunType ( SymType IntType )
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
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
      ( SemTag { posn = AlexPn 41 3 1, typeInfo = NotTypable } )
    )
  , Left
    ( LetRec
      [ FunDef "treeMerge"
        [ Param "t1"
          ( SemTag
            { posn = AlexPn 310 11 19
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "tree"
                    , def_source = FileIn "./test/resources/binTrees.llama"
                    , def_posn = AlexPn 5 1 6
                    }
                  )
                )
              )
            }
          )
        , Param "t2"
          ( SemTag
            { posn = AlexPn 313 11 22
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "tree"
                    , def_source = FileIn "./test/resources/binTrees.llama"
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
            ( ConstExpr "t1" )
            ( SemTag
              { posn = AlexPn 326 12 9
              , typeInfo = NodeType
                ( SymType
                  ( UserDefinedType
                    ( PosnId
                      { identifier = "tree"
                      , def_source = FileIn "./test/resources/binTrees.llama"
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
              ( ConstrPattern "Nil" [] )
              ( SemTag
                { posn = AlexPn 340 13 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr
              ( ConstExpr "t2" )
              ( SemTag
                { posn = AlexPn 348 13 15
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( SemTag { posn = AlexPn 340 13 7, typeInfo = NotTypable } )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "n" )
                  ( SemTag
                    { posn = AlexPn 362 14 12
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                , Pattern
                  ( IdPattern "t11" )
                  ( SemTag
                    { posn = AlexPn 364 14 14
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                , Pattern
                  ( IdPattern "t12" )
                  ( SemTag
                    { posn = AlexPn 368 14 18
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
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
                { posn = AlexPn 357 14 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr
              ( ConstrAppExpr "Node"
                [ Expr
                  ( ConstExpr "n" )
                  ( SemTag
                    { posn = AlexPn 380 14 30
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                , Expr
                  ( ConstExpr "t11" )
                  ( SemTag
                    { posn = AlexPn 382 14 32
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                , Expr
                  ( FunAppExpr "treeMerge"
                    [ Expr
                      ( ConstExpr "t12" )
                      ( SemTag
                        { posn = AlexPn 397 14 47
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
                                , def_posn = AlexPn 5 1 6
                                }
                              )
                            )
                          )
                        }
                      )
                    , Expr
                      ( ConstExpr "t2" )
                      ( SemTag
                        { posn = AlexPn 401 14 51
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
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
                    { posn = AlexPn 387 14 37
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
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
                { posn = AlexPn 375 14 25
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( SemTag { posn = AlexPn 357 14 7, typeInfo = NotTypable } )
          ]
          ( SemTag
            { posn = AlexPn 320 12 3
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "tree"
                    , def_source = FileIn "./test/resources/binTrees.llama"
                    , def_posn = AlexPn 5 1 6
                    }
                  )
                )
              )
            }
          )
        )
        ( SemTag
          { posn = AlexPn 300 11 9
          , typeInfo = DefType
            ( MonoType
              ( SymType
                ( FunType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
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
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
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
      ( SemTag { posn = AlexPn 292 11 1, typeInfo = NotTypable } )
    )
  , Left
    ( LetRec
      [ FunDef "treeDelete"
        [ Param "t"
          ( SemTag
            { posn = AlexPn 431 17 20
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "tree"
                    , def_source = FileIn "./test/resources/binTrees.llama"
                    , def_posn = AlexPn 5 1 6
                    }
                  )
                )
              )
            }
          )
        , Param "n"
          ( SemTag
            { posn = AlexPn 433 17 22, typeInfo = NodeType ( SymType IntType ) }
          )
        ] Nothing
        ( MatchExpr
          ( Expr
            ( ConstExpr "t" )
            ( SemTag
              { posn = AlexPn 445 18 9
              , typeInfo = NodeType
                ( SymType
                  ( UserDefinedType
                    ( PosnId
                      { identifier = "tree"
                      , def_source = FileIn "./test/resources/binTrees.llama"
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
              ( ConstrPattern "Nil" [] )
              ( SemTag
                { posn = AlexPn 458 19 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr
              ( ConstExpr "t" )
              ( SemTag
                { posn = AlexPn 468 19 17
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( SemTag { posn = AlexPn 458 19 7, typeInfo = NotTypable } )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "m" )
                  ( SemTag
                    { posn = AlexPn 481 20 12
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                , Pattern
                  ( IdPattern "t1" )
                  ( SemTag
                    { posn = AlexPn 483 20 14
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                , Pattern
                  ( IdPattern "t2" )
                  ( SemTag
                    { posn = AlexPn 486 20 17
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
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
                { posn = AlexPn 476 20 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
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
                        { posn = AlexPn 495 20 26
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( Expr
                      ( ConstExpr "m" )
                      ( SemTag
                        { posn = AlexPn 499 20 30
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 495 20 26
                    , typeInfo = NodeType ( SymType BoolType )
                    }
                  )
                )
                ( Expr
                  ( ConstrAppExpr "Node"
                    [ Expr
                      ( ConstExpr "m" )
                      ( SemTag
                        { posn = AlexPn 535 21 30
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    , Expr
                      ( FunAppExpr "treeDelete"
                        [ Expr
                          ( ConstExpr "t1" )
                          ( SemTag
                            { posn = AlexPn 549 21 44
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
                                    , def_posn = AlexPn 5 1 6
                                    }
                                  )
                                )
                              )
                            }
                          )
                        , Expr
                          ( ConstExpr "n" )
                          ( SemTag
                            { posn = AlexPn 552 21 47
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        ]
                      )
                      ( SemTag
                        { posn = AlexPn 538 21 33
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
                                , def_posn = AlexPn 5 1 6
                                }
                              )
                            )
                          )
                        }
                      )
                    , Expr
                      ( ConstExpr "t2" )
                      ( SemTag
                        { posn = AlexPn 555 21 50
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
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
                    { posn = AlexPn 530 21 25
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                )
                ( Expr
                  ( IfThenElseExpr
                    ( Expr
                      ( BinOpExpr GTOp
                        ( Expr
                          ( ConstExpr "n" )
                          ( SemTag
                            { posn = AlexPn 588 22 31
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( ConstExpr "m" )
                          ( SemTag
                            { posn = AlexPn 592 22 35
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 588 22 31
                        , typeInfo = NodeType ( SymType BoolType )
                        }
                      )
                    )
                    ( Expr
                      ( ConstrAppExpr "Node"
                        [ Expr
                          ( ConstExpr "m" )
                          ( SemTag
                            { posn = AlexPn 628 23 30
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        , Expr
                          ( ConstExpr "t1" )
                          ( SemTag
                            { posn = AlexPn 630 23 32
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
                                    , def_posn = AlexPn 5 1 6
                                    }
                                  )
                                )
                              )
                            }
                          )
                        , Expr
                          ( FunAppExpr "treeDelete"
                            [ Expr
                              ( ConstExpr "t2" )
                              ( SemTag
                                { posn = AlexPn 645 23 47
                                , typeInfo = NodeType
                                  ( SymType
                                    ( UserDefinedType
                                      ( PosnId
                                        { identifier = "tree"
                                        , def_source = FileIn "./test/resources/binTrees.llama"
                                        , def_posn = AlexPn 5 1 6
                                        }
                                      )
                                    )
                                  )
                                }
                              )
                            , Expr
                              ( ConstExpr "n" )
                              ( SemTag
                                { posn = AlexPn 648 23 50
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            ]
                          )
                          ( SemTag
                            { posn = AlexPn 634 23 36
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
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
                        { posn = AlexPn 623 23 25
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
                                , def_posn = AlexPn 5 1 6
                                }
                              )
                            )
                          )
                        }
                      )
                    )
                    ( Expr
                      ( FunAppExpr "treeMerge"
                        [ Expr
                          ( ConstExpr "t1" )
                          ( SemTag
                            { posn = AlexPn 712 25 35
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
                                    , def_posn = AlexPn 5 1 6
                                    }
                                  )
                                )
                              )
                            }
                          )
                        , Expr
                          ( ConstExpr "t2" )
                          ( SemTag
                            { posn = AlexPn 715 25 38
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
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
                        { posn = AlexPn 702 25 25
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
                                , def_posn = AlexPn 5 1 6
                                }
                              )
                            )
                          )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 585 22 28
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 492 20 23
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( SemTag { posn = AlexPn 476 20 7, typeInfo = NotTypable } )
          ]
          ( SemTag
            { posn = AlexPn 439 18 3
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "tree"
                    , def_source = FileIn "./test/resources/binTrees.llama"
                    , def_posn = AlexPn 5 1 6
                    }
                  )
                )
              )
            }
          )
        )
        ( SemTag
          { posn = AlexPn 420 17 9
          , typeInfo = DefType
            ( MonoType
              ( SymType
                ( FunType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                  ( SymType
                    ( FunType ( SymType IntType )
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
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
      ( SemTag { posn = AlexPn 412 17 1, typeInfo = NotTypable } )
    )
  , Left
    ( LetRec
      [ FunDef "treePrint"
        [ Param "t"
          ( SemTag
            { posn = AlexPn 743 28 19
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "tree"
                    , def_source = FileIn "./test/resources/binTrees.llama"
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
            ( ConstExpr "t" )
            ( SemTag
              { posn = AlexPn 755 29 9
              , typeInfo = NodeType
                ( SymType
                  ( UserDefinedType
                    ( PosnId
                      { identifier = "tree"
                      , def_source = FileIn "./test/resources/binTrees.llama"
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
              ( ConstrPattern "Nil" [] )
              ( SemTag
                { posn = AlexPn 768 30 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr UnitCExpr
              ( SemTag
                { posn = AlexPn 776 30 15
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag { posn = AlexPn 768 30 7, typeInfo = NotTypable } )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "n" )
                  ( SemTag
                    { posn = AlexPn 790 31 12
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                , Pattern
                  ( IdPattern "t1" )
                  ( SemTag
                    { posn = AlexPn 792 31 14
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                , Pattern
                  ( IdPattern "t2" )
                  ( SemTag
                    { posn = AlexPn 795 31 17
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
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
                { posn = AlexPn 785 31 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
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
                                      ( SemTag
                                        { posn = AlexPn 811 31 33
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    ]
                                  )
                                  ( SemTag
                                    { posn = AlexPn 801 31 23
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( FunAppExpr "print_string"
                                    [ Expr
                                      ( StringCExpr "(" )
                                      ( SemTag
                                        { posn = AlexPn 849 32 36
                                        , typeInfo = NodeType
                                          ( SymType
                                            ( ArrayType 1 ( SymType CharType ) )
                                          )
                                        }
                                      )
                                    ]
                                  )
                                  ( SemTag
                                    { posn = AlexPn 836 32 23
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 801 31 23
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                            ( Expr
                              ( FunAppExpr "treePrint"
                                [ Expr
                                  ( ConstExpr "t1" )
                                  ( SemTag
                                    { posn = AlexPn 886 33 33
                                    , typeInfo = NodeType
                                      ( SymType
                                        ( UserDefinedType
                                          ( PosnId
                                            { identifier = "tree"
                                            , def_source = FileIn "./test/resources/binTrees.llama"
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
                                { posn = AlexPn 876 33 23
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 801 31 23
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                        ( Expr
                          ( FunAppExpr "print_string"
                            [ Expr
                              ( StringCExpr "|" )
                              ( SemTag
                                { posn = AlexPn 925 34 36
                                , typeInfo = NodeType
                                  ( SymType
                                    ( ArrayType 1 ( SymType CharType ) )
                                  )
                                }
                              )
                            ]
                          )
                          ( SemTag
                            { posn = AlexPn 912 34 23
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 801 31 23
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                    ( Expr
                      ( FunAppExpr "treePrint"
                        [ Expr
                          ( ConstExpr "t2" )
                          ( SemTag
                            { posn = AlexPn 962 35 33
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
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
                        { posn = AlexPn 952 35 23
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 801 31 23
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
                ( Expr
                  ( FunAppExpr "print_string"
                    [ Expr
                      ( StringCExpr ")" )
                      ( SemTag
                        { posn = AlexPn 1001 36 36
                        , typeInfo = NodeType
                          ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                        }
                      )
                    ]
                  )
                  ( SemTag
                    { posn = AlexPn 988 36 23
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 801 31 23
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag { posn = AlexPn 785 31 7, typeInfo = NotTypable } )
          ]
          ( SemTag
            { posn = AlexPn 749 29 3, typeInfo = NodeType ( SymType UnitType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 733 28 9
          , typeInfo = DefType
            ( MonoType
              ( SymType
                ( FunType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
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
      ( SemTag { posn = AlexPn 725 28 1, typeInfo = NotTypable } )
    )
  , Left
    ( LetRec
      [ FunDef "treeCount"
        [ Param "t"
          ( SemTag
            { posn = AlexPn 1030 39 19
            , typeInfo = NodeType
              ( SymType
                ( UserDefinedType
                  ( PosnId
                    { identifier = "tree"
                    , def_source = FileIn "./test/resources/binTrees.llama"
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
            ( ConstExpr "t" )
            ( SemTag
              { posn = AlexPn 1042 40 9
              , typeInfo = NodeType
                ( SymType
                  ( UserDefinedType
                    ( PosnId
                      { identifier = "tree"
                      , def_source = FileIn "./test/resources/binTrees.llama"
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
              ( ConstrPattern "Nil" [] )
              ( SemTag
                { posn = AlexPn 1055 41 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr
              ( IntCExpr 0 )
              ( SemTag
                { posn = AlexPn 1065 41 17
                , typeInfo = NodeType ( SymType IntType )
                }
              )
            )
            ( SemTag { posn = AlexPn 1055 41 7, typeInfo = NotTypable } )
          , Match
            ( Pattern
              ( ConstrPattern "Node"
                [ Pattern
                  ( IdPattern "n" )
                  ( SemTag
                    { posn = AlexPn 1078 42 12
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                , Pattern
                  ( IdPattern "t1" )
                  ( SemTag
                    { posn = AlexPn 1080 42 14
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
                            , def_posn = AlexPn 5 1 6
                            }
                          )
                        )
                      )
                    }
                  )
                , Pattern
                  ( IdPattern "t2" )
                  ( SemTag
                    { posn = AlexPn 1083 42 17
                    , typeInfo = NodeType
                      ( SymType
                        ( UserDefinedType
                          ( PosnId
                            { identifier = "tree"
                            , def_source = FileIn "./test/resources/binTrees.llama"
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
                { posn = AlexPn 1073 42 7
                , typeInfo = NodeType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  )
                }
              )
            )
            ( Expr
              ( BinOpExpr PlusOp
                ( Expr
                  ( BinOpExpr PlusOp
                    ( Expr
                      ( IntCExpr 1 )
                      ( SemTag
                        { posn = AlexPn 1089 42 23
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( Expr
                      ( FunAppExpr "treeCount"
                        [ Expr
                          ( ConstExpr "t1" )
                          ( SemTag
                            { posn = AlexPn 1103 42 37
                            , typeInfo = NodeType
                              ( SymType
                                ( UserDefinedType
                                  ( PosnId
                                    { identifier = "tree"
                                    , def_source = FileIn "./test/resources/binTrees.llama"
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
                        { posn = AlexPn 1093 42 27
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 1089 42 23
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
                ( Expr
                  ( FunAppExpr "treeCount"
                    [ Expr
                      ( ConstExpr "t2" )
                      ( SemTag
                        { posn = AlexPn 1118 42 52
                        , typeInfo = NodeType
                          ( SymType
                            ( UserDefinedType
                              ( PosnId
                                { identifier = "tree"
                                , def_source = FileIn "./test/resources/binTrees.llama"
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
                    { posn = AlexPn 1108 42 42
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 1089 42 23
                , typeInfo = NodeType ( SymType IntType )
                }
              )
            )
            ( SemTag { posn = AlexPn 1073 42 7, typeInfo = NotTypable } )
          ]
          ( SemTag
            { posn = AlexPn 1036 40 3, typeInfo = NodeType ( SymType IntType ) }
          )
        )
        ( SemTag
          { posn = AlexPn 1020 39 9
          , typeInfo = DefType
            ( MonoType
              ( SymType
                ( FunType
                  ( SymType
                    ( UserDefinedType
                      ( PosnId
                        { identifier = "tree"
                        , def_source = FileIn "./test/resources/binTrees.llama"
                        , def_posn = AlexPn 5 1 6
                        }
                      )
                    )
                  ) ( SymType IntType )
                )
              )
            )
          }
        )
      ]
      ( SemTag { posn = AlexPn 1012 39 1, typeInfo = NotTypable } )
    )
  , Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ VarDef "seed" Nothing
              ( SemTag
                { posn = AlexPn 1146 47 7
                , typeInfo = DefType
                  ( MonoType ( SymType ( RefType ( SymType IntType ) ) ) )
                }
              )
            ]
            ( SemTag { posn = AlexPn 1142 47 3, typeInfo = NotTypable } )
          )
          ( LetIn
            ( Let
              [ FunDef "next"
                [ Param "u"
                  ( SemTag
                    { posn = AlexPn 1173 48 12
                    , typeInfo = NodeType
                      ( TVar 130 )
                    }
                  )
                ] Nothing
                ( Expr
                  ( BinOpExpr SemicolonOp
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr
                          ( ConstExpr "seed" )
                          ( SemTag
                            { posn = AlexPn 1181 49 5
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
                                  ( BinOpExpr TimesOp
                                    ( Expr
                                      ( UnOpExpr BangOp
                                        ( Expr
                                          ( ConstExpr "seed" )
                                          ( SemTag
                                            { posn = AlexPn 1191 49 15
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( RefType ( SymType IntType ) )
                                              )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 1190 49 14
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( IntCExpr 4241 )
                                      ( SemTag
                                        { posn = AlexPn 1198 49 22
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 1190 49 14
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( IntCExpr 22 )
                                  ( SemTag
                                    { posn = AlexPn 1205 49 29
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 1190 49 14
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                            ( Expr
                              ( IntCExpr 9949 )
                              ( SemTag
                                { posn = AlexPn 1213 49 37
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 1189 49 13
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 1181 49 5
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                    ( Expr
                      ( UnOpExpr BangOp
                        ( Expr
                          ( ConstExpr "seed" )
                          ( SemTag
                            { posn = AlexPn 1224 50 6
                            , typeInfo = NodeType
                              ( SymType ( RefType ( SymType IntType ) ) )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 1223 50 5
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 1181 49 5
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                )
                ( SemTag
                  { posn = AlexPn 1168 48 7
                  , typeInfo = DefType
                    ( AbsType 130
                      ( MonoType
                        ( SymType ( FunType ( TVar 130 ) ( SymType IntType ) ) )
                      )
                    )
                  }
                )
              ]
              ( SemTag { posn = AlexPn 1164 48 3, typeInfo = NotTypable } )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( BinOpExpr AssignMutableOp
                    ( Expr
                      ( ConstExpr "seed" )
                      ( SemTag
                        { posn = AlexPn 1234 51 3
                        , typeInfo = NodeType
                          ( SymType ( RefType ( SymType IntType ) ) )
                        }
                      )
                    )
                    ( Expr
                      ( IntCExpr 65 )
                      ( SemTag
                        { posn = AlexPn 1242 51 11
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 1234 51 3
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
                ( LetIn
                  ( Let
                    [ FunDef "random"
                      [ Param "max"
                        ( SemTag
                          { posn = AlexPn 1260 53 14
                          , typeInfo = NodeType ( SymType IntType )
                          }
                        )
                      ] Nothing
                      ( Expr
                        ( BinOpExpr ModOp
                          ( Expr
                            ( FunAppExpr "next"
                              [ Expr UnitCExpr
                                ( SemTag
                                  { posn = AlexPn 1271 53 25
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              ]
                            )
                            ( SemTag
                              { posn = AlexPn 1266 53 20
                              , typeInfo = NodeType ( SymType IntType )
                              }
                            )
                          )
                          ( Expr
                            ( ConstExpr "max" )
                            ( SemTag
                              { posn = AlexPn 1278 53 32
                              , typeInfo = NodeType ( SymType IntType )
                              }
                            )
                          )
                        )
                        ( SemTag
                          { posn = AlexPn 1266 53 20
                          , typeInfo = NodeType ( SymType IntType )
                          }
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 1253 53 7
                        , typeInfo = DefType
                          ( MonoType
                            ( SymType
                              ( FunType ( SymType IntType ) ( SymType IntType ) )
                            )
                          )
                        }
                      )
                    ]
                    ( SemTag
                      { posn = AlexPn 1249 53 3, typeInfo = NotTypable }
                    )
                  )
                  ( LetIn
                    ( Let
                      [ VarDef "t" Nothing
                        ( SemTag
                          { posn = AlexPn 1292 55 7
                          , typeInfo = DefType
                            ( MonoType
                              ( SymType
                                ( RefType
                                  ( SymType
                                    ( UserDefinedType
                                      ( PosnId
                                        { identifier = "tree"
                                        , def_source = FileIn "./test/resources/binTrees.llama"
                                        , def_posn = AlexPn 5 1 6
                                        }
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          }
                        )
                      ]
                      ( SemTag
                        { posn = AlexPn 1288 55 3, typeInfo = NotTypable }
                      )
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
                                              ( SemTag
                                                { posn = AlexPn 1307 56 3
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( RefType
                                                      ( SymType
                                                        ( UserDefinedType
                                                          ( PosnId
                                                            { identifier = "tree"
                                                            , def_source = FileIn "./test/resources/binTrees.llama"
                                                            , def_posn = AlexPn 5 1 6
                                                            }
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                }
                                              )
                                            )
                                            ( Expr
                                              ( ConstConstrExpr "Nil" )
                                              ( SemTag
                                                { posn = AlexPn 1312 56 8
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( UserDefinedType
                                                      ( PosnId
                                                        { identifier = "tree"
                                                        , def_source = FileIn "./test/resources/binTrees.llama"
                                                        , def_posn = AlexPn 5 1 6
                                                        }
                                                      )
                                                    )
                                                  )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 1307 56 3
                                            , typeInfo = NodeType ( SymType UnitType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( ForExpr "i"
                                            ( Expr
                                              ( IntCExpr 1 )
                                              ( SemTag
                                                { posn = AlexPn 1328 58 11
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            )
                                            ( Expr
                                              ( IntCExpr 10 )
                                              ( SemTag
                                                { posn = AlexPn 1333 58 16
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            )
                                            ( Expr
                                              ( BinOpExpr AssignMutableOp
                                                ( Expr
                                                  ( ConstExpr "t" )
                                                  ( SemTag
                                                    { posn = AlexPn 1343 59 5
                                                    , typeInfo = NodeType
                                                      ( SymType
                                                        ( RefType
                                                          ( SymType
                                                            ( UserDefinedType
                                                              ( PosnId
                                                                { identifier = "tree"
                                                                , def_source = FileIn "./test/resources/binTrees.llama"
                                                                , def_posn = AlexPn 5 1 6
                                                                }
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    }
                                                  )
                                                )
                                                ( Expr
                                                  ( FunAppExpr "treeInsert"
                                                    [ Expr
                                                      ( UnOpExpr BangOp
                                                        ( Expr
                                                          ( ConstExpr "t" )
                                                          ( SemTag
                                                            { posn = AlexPn 1360 59 22
                                                            , typeInfo = NodeType
                                                              ( SymType
                                                                ( RefType
                                                                  ( SymType
                                                                    ( UserDefinedType
                                                                      ( PosnId
                                                                        { identifier = "tree"
                                                                        , def_source = FileIn "./test/resources/binTrees.llama"
                                                                        , def_posn = AlexPn 5 1 6
                                                                        }
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            }
                                                          )
                                                        )
                                                      )
                                                      ( SemTag
                                                        { posn = AlexPn 1359 59 21
                                                        , typeInfo = NodeType
                                                          ( SymType
                                                            ( UserDefinedType
                                                              ( PosnId
                                                                { identifier = "tree"
                                                                , def_source = FileIn "./test/resources/binTrees.llama"
                                                                , def_posn = AlexPn 5 1 6
                                                                }
                                                              )
                                                            )
                                                          )
                                                        }
                                                      )
                                                    , Expr
                                                      ( FunAppExpr "random"
                                                        [ Expr
                                                          ( IntCExpr 100 )
                                                          ( SemTag
                                                            { posn = AlexPn 1370 59 32
                                                            , typeInfo = NodeType ( SymType IntType )
                                                            }
                                                          )
                                                        ]
                                                      )
                                                      ( SemTag
                                                        { posn = AlexPn 1363 59 25
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    ]
                                                  )
                                                  ( SemTag
                                                    { posn = AlexPn 1348 59 10
                                                    , typeInfo = NodeType
                                                      ( SymType
                                                        ( UserDefinedType
                                                          ( PosnId
                                                            { identifier = "tree"
                                                            , def_source = FileIn "./test/resources/binTrees.llama"
                                                            , def_posn = AlexPn 5 1 6
                                                            }
                                                          )
                                                        )
                                                      )
                                                    }
                                                  )
                                                )
                                              )
                                              ( SemTag
                                                { posn = AlexPn 1343 59 5
                                                , typeInfo = NodeType ( SymType UnitType )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 1320 58 3
                                            , typeInfo = NodeType ( SymType UnitType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 1307 56 3
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( FunAppExpr "print_string"
                                        [ Expr
                                          ( StringCExpr "Initial tree: " )
                                          ( SemTag
                                            { posn = AlexPn 1399 62 16
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( ArrayType 1 ( SymType CharType ) )
                                              )
                                            }
                                          )
                                        ]
                                      )
                                      ( SemTag
                                        { posn = AlexPn 1386 62 3
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 1307 56 3
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                                ( Expr
                                  ( FunAppExpr "treePrint"
                                    [ Expr
                                      ( UnOpExpr BangOp
                                        ( Expr
                                          ( ConstExpr "t" )
                                          ( SemTag
                                            { posn = AlexPn 1430 63 14
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( RefType
                                                  ( SymType
                                                    ( UserDefinedType
                                                      ( PosnId
                                                        { identifier = "tree"
                                                        , def_source = FileIn "./test/resources/binTrees.llama"
                                                        , def_posn = AlexPn 5 1 6
                                                        }
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 1429 63 13
                                        , typeInfo = NodeType
                                          ( SymType
                                            ( UserDefinedType
                                              ( PosnId
                                                { identifier = "tree"
                                                , def_source = FileIn "./test/resources/binTrees.llama"
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
                                    { posn = AlexPn 1419 63 3
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 1307 56 3
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                            ( Expr
                              ( FunAppExpr "print_string"
                                [ Expr
                                  ( StringCExpr "\n" )
                                  ( SemTag
                                    { posn = AlexPn 1448 64 16
                                    , typeInfo = NodeType
                                      ( SymType
                                        ( ArrayType 1 ( SymType CharType ) )
                                      )
                                    }
                                  )
                                ]
                              )
                              ( SemTag
                                { posn = AlexPn 1435 64 3
                                , typeInfo = NodeType ( SymType UnitType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 1307 56 3
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                        ( LetIn
                          ( LetRec
                            [ FunDef "choose"
                              [ Param "t"
                                ( SemTag
                                  { posn = AlexPn 1472 66 18
                                  , typeInfo = NodeType
                                    ( SymType
                                      ( UserDefinedType
                                        ( PosnId
                                          { identifier = "tree"
                                          , def_source = FileIn "./test/resources/binTrees.llama"
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
                                  ( ConstExpr "t" )
                                  ( SemTag
                                    { posn = AlexPn 1486 67 11
                                    , typeInfo = NodeType
                                      ( SymType
                                        ( UserDefinedType
                                          ( PosnId
                                            { identifier = "tree"
                                            , def_source = FileIn "./test/resources/binTrees.llama"
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
                                    ( ConstrPattern "Node"
                                      [ Pattern
                                        ( IdPattern "n" )
                                        ( SemTag
                                          { posn = AlexPn 1504 68 12
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      , Pattern
                                        ( IdPattern "t1" )
                                        ( SemTag
                                          { posn = AlexPn 1506 68 14
                                          , typeInfo = NodeType
                                            ( SymType
                                              ( UserDefinedType
                                                ( PosnId
                                                  { identifier = "tree"
                                                  , def_source = FileIn "./test/resources/binTrees.llama"
                                                  , def_posn = AlexPn 5 1 6
                                                  }
                                                )
                                              )
                                            )
                                          }
                                        )
                                      , Pattern
                                        ( IdPattern "t2" )
                                        ( SemTag
                                          { posn = AlexPn 1509 68 17
                                          , typeInfo = NodeType
                                            ( SymType
                                              ( UserDefinedType
                                                ( PosnId
                                                  { identifier = "tree"
                                                  , def_source = FileIn "./test/resources/binTrees.llama"
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
                                      { posn = AlexPn 1499 68 7
                                      , typeInfo = NodeType
                                        ( SymType
                                          ( UserDefinedType
                                            ( PosnId
                                              { identifier = "tree"
                                              , def_source = FileIn "./test/resources/binTrees.llama"
                                              , def_posn = AlexPn 5 1 6
                                              }
                                            )
                                          )
                                        )
                                      }
                                    )
                                  )
                                  ( LetIn
                                    ( Let
                                      [ FunDef "c1" [] Nothing
                                        ( Expr
                                          ( FunAppExpr "treeCount"
                                            [ Expr
                                              ( ConstExpr "t1" )
                                              ( SemTag
                                                { posn = AlexPn 1542 69 28
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( UserDefinedType
                                                      ( PosnId
                                                        { identifier = "tree"
                                                        , def_source = FileIn "./test/resources/binTrees.llama"
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
                                            { posn = AlexPn 1532 69 18
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 1527 69 13
                                          , typeInfo = DefType
                                            ( MonoType ( SymType IntType ) )
                                          }
                                        )
                                      , FunDef "c2" [] Nothing
                                        ( Expr
                                          ( FunAppExpr "treeCount"
                                            [ Expr
                                              ( ConstExpr "t2" )
                                              ( SemTag
                                                { posn = AlexPn 1572 70 28
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( UserDefinedType
                                                      ( PosnId
                                                        { identifier = "tree"
                                                        , def_source = FileIn "./test/resources/binTrees.llama"
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
                                            { posn = AlexPn 1562 70 18
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 1557 70 13
                                          , typeInfo = DefType
                                            ( MonoType ( SymType IntType ) )
                                          }
                                        )
                                      ]
                                      ( SemTag
                                        { posn = AlexPn 1523 69 9
                                        , typeInfo = NotTypable
                                        }
                                      )
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
                                                        ( SemTag
                                                          { posn = AlexPn 1602 71 25
                                                          , typeInfo = NodeType ( SymType IntType )
                                                          }
                                                        )
                                                      )
                                                      ( Expr
                                                        ( ConstExpr "c1" )
                                                        ( SemTag
                                                          { posn = AlexPn 1606 71 29
                                                          , typeInfo = NodeType ( SymType IntType )
                                                          }
                                                        )
                                                      )
                                                    )
                                                    ( SemTag
                                                      { posn = AlexPn 1602 71 25
                                                      , typeInfo = NodeType ( SymType IntType )
                                                      }
                                                    )
                                                  )
                                                  ( Expr
                                                    ( ConstExpr "c2" )
                                                    ( SemTag
                                                      { posn = AlexPn 1611 71 34
                                                      , typeInfo = NodeType ( SymType IntType )
                                                      }
                                                    )
                                                  )
                                                )
                                                ( SemTag
                                                  { posn = AlexPn 1602 71 25
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              ]
                                            )
                                            ( SemTag
                                              { posn = AlexPn 1594 71 17
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 1590 71 13
                                            , typeInfo = DefType
                                              ( MonoType ( SymType IntType ) )
                                            }
                                          )
                                        ]
                                        ( SemTag
                                          { posn = AlexPn 1586 71 9
                                          , typeInfo = NotTypable
                                          }
                                        )
                                      )
                                      ( Expr
                                        ( IfThenElseExpr
                                          ( Expr
                                            ( BinOpExpr EqOp
                                              ( Expr
                                                ( ConstExpr "r" )
                                                ( SemTag
                                                  { posn = AlexPn 1629 72 12
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                              ( Expr
                                                ( IntCExpr 0 )
                                                ( SemTag
                                                  { posn = AlexPn 1633 72 16
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                            )
                                            ( SemTag
                                              { posn = AlexPn 1629 72 12
                                              , typeInfo = NodeType ( SymType BoolType )
                                              }
                                            )
                                          )
                                          ( Expr
                                            ( ConstExpr "n" )
                                            ( SemTag
                                              { posn = AlexPn 1650 73 11
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          )
                                          ( Expr
                                            ( IfThenElseExpr
                                              ( Expr
                                                ( BinOpExpr LEqOp
                                                  ( Expr
                                                    ( ConstExpr "r" )
                                                    ( SemTag
                                                      { posn = AlexPn 1668 74 17
                                                      , typeInfo = NodeType ( SymType IntType )
                                                      }
                                                    )
                                                  )
                                                  ( Expr
                                                    ( ConstExpr "c1" )
                                                    ( SemTag
                                                      { posn = AlexPn 1673 74 22
                                                      , typeInfo = NodeType ( SymType IntType )
                                                      }
                                                    )
                                                  )
                                                )
                                                ( SemTag
                                                  { posn = AlexPn 1668 74 17
                                                  , typeInfo = NodeType ( SymType BoolType )
                                                  }
                                                )
                                              )
                                              ( Expr
                                                ( FunAppExpr "choose"
                                                  [ Expr
                                                    ( ConstExpr "t1" )
                                                    ( SemTag
                                                      { posn = AlexPn 1698 75 18
                                                      , typeInfo = NodeType
                                                        ( SymType
                                                          ( UserDefinedType
                                                            ( PosnId
                                                              { identifier = "tree"
                                                              , def_source = FileIn "./test/resources/binTrees.llama"
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
                                                  { posn = AlexPn 1691 75 11
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                              ( Expr
                                                ( FunAppExpr "choose"
                                                  [ Expr
                                                    ( ConstExpr "t2" )
                                                    ( SemTag
                                                      { posn = AlexPn 1731 77 18
                                                      , typeInfo = NodeType
                                                        ( SymType
                                                          ( UserDefinedType
                                                            ( PosnId
                                                              { identifier = "tree"
                                                              , def_source = FileIn "./test/resources/binTrees.llama"
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
                                                  { posn = AlexPn 1724 77 11
                                                  , typeInfo = NodeType ( SymType IntType )
                                                  }
                                                )
                                              )
                                            )
                                            ( SemTag
                                              { posn = AlexPn 1665 74 14
                                              , typeInfo = NodeType ( SymType IntType )
                                              }
                                            )
                                          )
                                        )
                                        ( SemTag
                                          { posn = AlexPn 1626 72 9
                                          , typeInfo = NodeType ( SymType IntType )
                                          }
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 1586 71 9
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 1523 69 9
                                      , typeInfo = NodeType ( SymType IntType )
                                      }
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 1499 68 7
                                    , typeInfo = NotTypable
                                    }
                                  )
                                ]
                                ( SemTag
                                  { posn = AlexPn 1480 67 5
                                  , typeInfo = NodeType ( SymType IntType )
                                  }
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 1465 66 11
                                , typeInfo = DefType
                                  ( MonoType
                                    ( SymType
                                      ( FunType
                                        ( SymType
                                          ( UserDefinedType
                                            ( PosnId
                                              { identifier = "tree"
                                              , def_source = FileIn "./test/resources/binTrees.llama"
                                              , def_posn = AlexPn 5 1 6
                                              }
                                            )
                                          )
                                        ) ( SymType IntType )
                                      )
                                    )
                                  )
                                }
                              )
                            ]
                            ( SemTag
                              { posn = AlexPn 1457 66 3, typeInfo = NotTypable }
                            )
                          )
                          ( Expr
                            ( ForExpr "i"
                              ( Expr
                                ( IntCExpr 1 )
                                ( SemTag
                                  { posn = AlexPn 1756 80 11
                                  , typeInfo = NodeType ( SymType IntType )
                                  }
                                )
                              )
                              ( Expr
                                ( FunAppExpr "treeCount"
                                  [ Expr
                                    ( UnOpExpr BangOp
                                      ( Expr
                                        ( ConstExpr "t" )
                                        ( SemTag
                                          { posn = AlexPn 1772 80 27
                                          , typeInfo = NodeType
                                            ( SymType
                                              ( RefType
                                                ( SymType
                                                  ( UserDefinedType
                                                    ( PosnId
                                                      { identifier = "tree"
                                                      , def_source = FileIn "./test/resources/binTrees.llama"
                                                      , def_posn = AlexPn 5 1 6
                                                      }
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          }
                                        )
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 1771 80 26
                                      , typeInfo = NodeType
                                        ( SymType
                                          ( UserDefinedType
                                            ( PosnId
                                              { identifier = "tree"
                                              , def_source = FileIn "./test/resources/binTrees.llama"
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
                                  { posn = AlexPn 1761 80 16
                                  , typeInfo = NodeType ( SymType IntType )
                                  }
                                )
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
                                              ( SemTag
                                                { posn = AlexPn 1797 81 21
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( RefType
                                                      ( SymType
                                                        ( UserDefinedType
                                                          ( PosnId
                                                            { identifier = "tree"
                                                            , def_source = FileIn "./test/resources/binTrees.llama"
                                                            , def_posn = AlexPn 5 1 6
                                                            }
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 1796 81 20
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( UserDefinedType
                                                  ( PosnId
                                                    { identifier = "tree"
                                                    , def_source = FileIn "./test/resources/binTrees.llama"
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
                                        { posn = AlexPn 1789 81 13
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    )
                                    ( SemTag
                                      { posn = AlexPn 1785 81 9
                                      , typeInfo = DefType
                                        ( MonoType ( SymType IntType ) )
                                      }
                                    )
                                  ]
                                  ( SemTag
                                    { posn = AlexPn 1781 81 5
                                    , typeInfo = NotTypable
                                    }
                                  )
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
                                                          ( SemTag
                                                            { posn = AlexPn 1819 82 18
                                                            , typeInfo = NodeType
                                                              ( SymType
                                                                ( ArrayType 1 ( SymType CharType ) )
                                                              )
                                                            }
                                                          )
                                                        ]
                                                      )
                                                      ( SemTag
                                                        { posn = AlexPn 1806 82 5
                                                        , typeInfo = NodeType ( SymType UnitType )
                                                        }
                                                      )
                                                    )
                                                    ( Expr
                                                      ( FunAppExpr "print_int"
                                                        [ Expr
                                                          ( ConstExpr "n" )
                                                          ( SemTag
                                                            { posn = AlexPn 1846 83 15
                                                            , typeInfo = NodeType ( SymType IntType )
                                                            }
                                                          )
                                                        ]
                                                      )
                                                      ( SemTag
                                                        { posn = AlexPn 1836 83 5
                                                        , typeInfo = NodeType ( SymType UnitType )
                                                        }
                                                      )
                                                    )
                                                  )
                                                  ( SemTag
                                                    { posn = AlexPn 1806 82 5
                                                    , typeInfo = NodeType ( SymType UnitType )
                                                    }
                                                  )
                                                )
                                                ( Expr
                                                  ( FunAppExpr "print_string"
                                                    [ Expr
                                                      ( StringCExpr ": " )
                                                      ( SemTag
                                                        { posn = AlexPn 1866 84 18
                                                        , typeInfo = NodeType
                                                          ( SymType
                                                            ( ArrayType 1 ( SymType CharType ) )
                                                          )
                                                        }
                                                      )
                                                    ]
                                                  )
                                                  ( SemTag
                                                    { posn = AlexPn 1853 84 5
                                                    , typeInfo = NodeType ( SymType UnitType )
                                                    }
                                                  )
                                                )
                                              )
                                              ( SemTag
                                                { posn = AlexPn 1806 82 5
                                                , typeInfo = NodeType ( SymType UnitType )
                                                }
                                              )
                                            )
                                            ( Expr
                                              ( BinOpExpr AssignMutableOp
                                                ( Expr
                                                  ( ConstExpr "t" )
                                                  ( SemTag
                                                    { posn = AlexPn 1876 85 5
                                                    , typeInfo = NodeType
                                                      ( SymType
                                                        ( RefType
                                                          ( SymType
                                                            ( UserDefinedType
                                                              ( PosnId
                                                                { identifier = "tree"
                                                                , def_source = FileIn "./test/resources/binTrees.llama"
                                                                , def_posn = AlexPn 5 1 6
                                                                }
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    }
                                                  )
                                                )
                                                ( Expr
                                                  ( FunAppExpr "treeDelete"
                                                    [ Expr
                                                      ( UnOpExpr BangOp
                                                        ( Expr
                                                          ( ConstExpr "t" )
                                                          ( SemTag
                                                            { posn = AlexPn 1893 85 22
                                                            , typeInfo = NodeType
                                                              ( SymType
                                                                ( RefType
                                                                  ( SymType
                                                                    ( UserDefinedType
                                                                      ( PosnId
                                                                        { identifier = "tree"
                                                                        , def_source = FileIn "./test/resources/binTrees.llama"
                                                                        , def_posn = AlexPn 5 1 6
                                                                        }
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            }
                                                          )
                                                        )
                                                      )
                                                      ( SemTag
                                                        { posn = AlexPn 1892 85 21
                                                        , typeInfo = NodeType
                                                          ( SymType
                                                            ( UserDefinedType
                                                              ( PosnId
                                                                { identifier = "tree"
                                                                , def_source = FileIn "./test/resources/binTrees.llama"
                                                                , def_posn = AlexPn 5 1 6
                                                                }
                                                              )
                                                            )
                                                          )
                                                        }
                                                      )
                                                    , Expr
                                                      ( ConstExpr "n" )
                                                      ( SemTag
                                                        { posn = AlexPn 1895 85 24
                                                        , typeInfo = NodeType ( SymType IntType )
                                                        }
                                                      )
                                                    ]
                                                  )
                                                  ( SemTag
                                                    { posn = AlexPn 1881 85 10
                                                    , typeInfo = NodeType
                                                      ( SymType
                                                        ( UserDefinedType
                                                          ( PosnId
                                                            { identifier = "tree"
                                                            , def_source = FileIn "./test/resources/binTrees.llama"
                                                            , def_posn = AlexPn 5 1 6
                                                            }
                                                          )
                                                        )
                                                      )
                                                    }
                                                  )
                                                )
                                              )
                                              ( SemTag
                                                { posn = AlexPn 1876 85 5
                                                , typeInfo = NodeType ( SymType UnitType )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 1806 82 5
                                            , typeInfo = NodeType ( SymType UnitType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( FunAppExpr "treePrint"
                                            [ Expr
                                              ( UnOpExpr BangOp
                                                ( Expr
                                                  ( ConstExpr "t" )
                                                  ( SemTag
                                                    { posn = AlexPn 1913 86 16
                                                    , typeInfo = NodeType
                                                      ( SymType
                                                        ( RefType
                                                          ( SymType
                                                            ( UserDefinedType
                                                              ( PosnId
                                                                { identifier = "tree"
                                                                , def_source = FileIn "./test/resources/binTrees.llama"
                                                                , def_posn = AlexPn 5 1 6
                                                                }
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    }
                                                  )
                                                )
                                              )
                                              ( SemTag
                                                { posn = AlexPn 1912 86 15
                                                , typeInfo = NodeType
                                                  ( SymType
                                                    ( UserDefinedType
                                                      ( PosnId
                                                        { identifier = "tree"
                                                        , def_source = FileIn "./test/resources/binTrees.llama"
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
                                            { posn = AlexPn 1902 86 5
                                            , typeInfo = NodeType ( SymType UnitType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 1806 82 5
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                    ( Expr
                                      ( FunAppExpr "print_string"
                                        [ Expr
                                          ( StringCExpr "\n" )
                                          ( SemTag
                                            { posn = AlexPn 1933 87 18
                                            , typeInfo = NodeType
                                              ( SymType
                                                ( ArrayType 1 ( SymType CharType ) )
                                              )
                                            }
                                          )
                                        ]
                                      )
                                      ( SemTag
                                        { posn = AlexPn 1920 87 5
                                        , typeInfo = NodeType ( SymType UnitType )
                                        }
                                      )
                                    )
                                  )
                                  ( SemTag
                                    { posn = AlexPn 1806 82 5
                                    , typeInfo = NodeType ( SymType UnitType )
                                    }
                                  )
                                )
                                ( SemTag
                                  { posn = AlexPn 1781 81 5
                                  , typeInfo = NodeType ( SymType UnitType )
                                  }
                                )
                              )
                            )
                            ( SemTag
                              { posn = AlexPn 1748 80 3
                              , typeInfo = NodeType ( SymType UnitType )
                              }
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 1457 66 3
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 1307 56 3
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                    ( SemTag
                      { posn = AlexPn 1288 55 3
                      , typeInfo = NodeType ( SymType UnitType )
                      }
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 1249 53 3
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 1234 51 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag
              { posn = AlexPn 1164 48 3
              , typeInfo = NodeType ( SymType UnitType )
              }
            )
          )
          ( SemTag
            { posn = AlexPn 1142 47 3
            , typeInfo = NodeType ( SymType UnitType )
            }
          )
        )
        ( SemTag
          { posn = AlexPn 1133 46 5
          , typeInfo = DefType
            ( MonoType ( SymType UnitType ) )
          }
        )
      ]
      ( SemTag { posn = AlexPn 1129 46 1, typeInfo = NotTypable } )
    )
  ]
  ( SemTag { posn = AlexPn 0 1 1, typeInfo = NotTypable } )
