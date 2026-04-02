module Unit.Semantics.Reverse (reverseSemAST) where

import Common.AST
import Common.SymbolType (SymbolType(..), TypeScheme(..))
import Lexer.Lexer (AlexPosn(..))
import Semantics.Utils (SemanticTag(..), TypeInfo(..))

reverseSemAST :: AST SemanticTag
reverseSemAST = AST
  [ Left
    ( Let
      [ FunDef "main" [] Nothing
        ( LetIn
          ( Let
            [ FunDef "reverse"
              [ Param "s"
                ( SemTag
                  { posn = AlexPn 25 2 15
                  , typeInfo = NodeType
                    ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                  }
                )
              , Param "r"
                ( SemTag
                  { posn = AlexPn 27 2 17
                  , typeInfo = NodeType
                    ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                  }
                )
              ] Nothing
              ( LetIn
                ( Let
                  [ FunDef "l" [] Nothing
                    ( Expr
                      ( FunAppExpr "strlen"
                        [ Expr
                          ( ConstExpr "s" )
                          ( SemTag
                            { posn = AlexPn 50 3 20
                            , typeInfo = NodeType
                              ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                            }
                          )
                        ]
                      )
                      ( SemTag
                        { posn = AlexPn 43 3 13
                        , typeInfo = NodeType ( SymType IntType )
                        }
                      )
                    )
                    ( SemTag
                      { posn = AlexPn 39 3 9
                      , typeInfo = DefType
                        ( MonoType ( SymType IntType ) )
                      }
                    )
                  ]
                  ( SemTag { posn = AlexPn 35 3 5, typeInfo = NotTypable } )
                )
                ( Expr
                  ( BinOpExpr SemicolonOp
                    ( Expr
                      ( ForExpr "i"
                        ( Expr
                          ( IntCExpr 0 )
                          ( SemTag
                            { posn = AlexPn 67 4 13
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( BinOpExpr MinusOp
                            ( Expr
                              ( ConstExpr "l" )
                              ( SemTag
                                { posn = AlexPn 72 4 18
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                            ( Expr
                              ( IntCExpr 1 )
                              ( SemTag
                                { posn = AlexPn 74 4 20
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 72 4 18
                            , typeInfo = NodeType ( SymType IntType )
                            }
                          )
                        )
                        ( Expr
                          ( BinOpExpr AssignMutableOp
                            ( Expr
                              ( ArrayAccess "r"
                                [ Expr
                                  ( ConstExpr "i" )
                                  ( SemTag
                                    { posn = AlexPn 87 5 9
                                    , typeInfo = NodeType ( SymType IntType )
                                    }
                                  )
                                ]
                              )
                              ( SemTag
                                { posn = AlexPn 85 5 7
                                , typeInfo = NodeType
                                  ( SymType ( RefType ( SymType CharType ) ) )
                                }
                              )
                            )
                            ( Expr
                              ( UnOpExpr BangOp
                                ( Expr
                                  ( ArrayAccess "s"
                                    [ Expr
                                      ( BinOpExpr MinusOp
                                        ( Expr
                                          ( BinOpExpr MinusOp
                                            ( Expr
                                              ( ConstExpr "l" )
                                              ( SemTag
                                                { posn = AlexPn 96 5 18
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            )
                                            ( Expr
                                              ( ConstExpr "i" )
                                              ( SemTag
                                                { posn = AlexPn 98 5 20
                                                , typeInfo = NodeType ( SymType IntType )
                                                }
                                              )
                                            )
                                          )
                                          ( SemTag
                                            { posn = AlexPn 96 5 18
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                        ( Expr
                                          ( IntCExpr 1 )
                                          ( SemTag
                                            { posn = AlexPn 100 5 22
                                            , typeInfo = NodeType ( SymType IntType )
                                            }
                                          )
                                        )
                                      )
                                      ( SemTag
                                        { posn = AlexPn 96 5 18
                                        , typeInfo = NodeType ( SymType IntType )
                                        }
                                      )
                                    ]
                                  )
                                  ( SemTag
                                    { posn = AlexPn 94 5 16
                                    , typeInfo = NodeType
                                      ( SymType
                                        ( RefType ( SymType CharType ) )
                                      )
                                    }
                                  )
                                )
                              )
                              ( SemTag
                                { posn = AlexPn 93 5 15
                                , typeInfo = NodeType ( SymType CharType )
                                }
                              )
                            )
                          )
                          ( SemTag
                            { posn = AlexPn 85 5 7
                            , typeInfo = NodeType ( SymType UnitType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 59 4 5
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                    ( Expr
                      ( BinOpExpr AssignMutableOp
                        ( Expr
                          ( ArrayAccess "r"
                            [ Expr
                              ( ConstExpr "l" )
                              ( SemTag
                                { posn = AlexPn 119 7 7
                                , typeInfo = NodeType ( SymType IntType )
                                }
                              )
                            ]
                          )
                          ( SemTag
                            { posn = AlexPn 117 7 5
                            , typeInfo = NodeType
                              ( SymType ( RefType ( SymType CharType ) ) )
                            }
                          )
                        )
                        ( Expr
                          ( CharCExpr '\0' )
                          ( SemTag
                            { posn = AlexPn 125 7 13
                            , typeInfo = NodeType ( SymType CharType )
                            }
                          )
                        )
                      )
                      ( SemTag
                        { posn = AlexPn 117 7 5
                        , typeInfo = NodeType ( SymType UnitType )
                        }
                      )
                    )
                  )
                  ( SemTag
                    { posn = AlexPn 59 4 5
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
                { posn = AlexPn 17 2 7
                , typeInfo = DefType
                  ( MonoType
                    ( SymType
                      ( FunType
                        ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                        ( SymType
                          ( FunType
                            ( SymType
                              ( ArrayType 1 ( SymType CharType ) )
                            ) ( SymType UnitType )
                          )
                        )
                      )
                    )
                  )
                }
              )
            ]
            ( SemTag { posn = AlexPn 13 2 3, typeInfo = NotTypable } )
          )
          ( LetIn
            ( Let
              [ ArrayDef "p"
                [ Expr
                  ( IntCExpr 20 )
                  ( SemTag
                    { posn = AlexPn 151 9 18
                    , typeInfo = NodeType ( SymType IntType )
                    }
                  )
                ] Nothing
                ( SemTag
                  { posn = AlexPn 140 9 7
                  , typeInfo = DefType
                    ( MonoType
                      ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                    )
                  }
                )
              ]
              ( SemTag { posn = AlexPn 136 9 3, typeInfo = NotTypable } )
            )
            ( Expr
              ( BinOpExpr SemicolonOp
                ( Expr
                  ( FunAppExpr "reverse"
                    [ Expr
                      ( StringCExpr "\n!dlrow olleH" )
                      ( SemTag
                        { posn = AlexPn 169 11 11
                        , typeInfo = NodeType
                          ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                        }
                      )
                    , Expr
                      ( ConstExpr "p" )
                      ( SemTag
                        { posn = AlexPn 186 11 28
                        , typeInfo = NodeType
                          ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                        }
                      )
                    ]
                  )
                  ( SemTag
                    { posn = AlexPn 161 11 3
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
                ( Expr
                  ( FunAppExpr "print_string"
                    [ Expr
                      ( ConstExpr "p" )
                      ( SemTag
                        { posn = AlexPn 204 12 16
                        , typeInfo = NodeType
                          ( SymType ( ArrayType 1 ( SymType CharType ) ) )
                        }
                      )
                    ]
                  )
                  ( SemTag
                    { posn = AlexPn 191 12 3
                    , typeInfo = NodeType ( SymType UnitType )
                    }
                  )
                )
              )
              ( SemTag
                { posn = AlexPn 161 11 3
                , typeInfo = NodeType ( SymType UnitType )
                }
              )
            )
            ( SemTag
              { posn = AlexPn 136 9 3
              , typeInfo = NodeType ( SymType UnitType )
              }
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
