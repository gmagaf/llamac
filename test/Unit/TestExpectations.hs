module Unit.TestExpectations (module Unit.TestExpectations) where

import Common.AST

helloWorldAST :: Program
helloWorldAST =
  [Left $ Let [FunDef "main" []
    (FunAppExpr "print_string" [StringCExpr "Hello world!\\n"])]
  ]

hanoiAST :: Program
hanoiAST =
  [Left (Let [FunDef "main" []
    (LetIn (Let [FunDef "move" [Param "source",Param "target"]
      (BinOpExpr SemicolonOp
        (BinOpExpr SemicolonOp
          (BinOpExpr SemicolonOp
            (BinOpExpr SemicolonOp
              (FunAppExpr "print_string" [StringCExpr "Moving from: "])
              (FunAppExpr "print_string" [FunAppExpr "source" []]))
              (FunAppExpr "print_string" [StringCExpr " to "]))
              (FunAppExpr "print_string" [FunAppExpr "target" []]))
              (FunAppExpr "print_string" [StringCExpr "\\n"]))])
    (LetIn (LetRec [FunDef "hanoi" [Param "rings",Param "source",Param "target",Param "auxil"]
      (IfThenExpr (BinOpExpr GTOp (FunAppExpr "rings" []) (IntCExpr 0))
        (BeginExpr
          (BinOpExpr SemicolonOp
            (BinOpExpr SemicolonOp
              (FunAppExpr "hanoi" [BinOpExpr MinusOp (FunAppExpr "rings" []) (IntCExpr 1),FunAppExpr "source" [],FunAppExpr "auxil" [],FunAppExpr "target" []])
              (FunAppExpr "move" [FunAppExpr "source" [],FunAppExpr "target" []]))
              (FunAppExpr "hanoi" [BinOpExpr MinusOp (FunAppExpr "rings" []) (IntCExpr 1),FunAppExpr "auxil" [],FunAppExpr "target" [],FunAppExpr "source" []]))))])
      (BinOpExpr SemicolonOp (FunAppExpr "print_string" [StringCExpr "Please, give the number of rings: "])
        (LetIn (Let [FunDef "n" [] (FunAppExpr "read_int" [UnitCExpr])])
          (FunAppExpr "hanoi" [FunAppExpr "n" [],StringCExpr "left",StringCExpr "right",StringCExpr "middle"])))))])]

hanoiTypeAST :: Program
hanoiTypeAST =
  [Right (Type [TDef "pile" [Constr "Left" [],Constr "Middle" [],Constr "Right" []]]),
  Left (Let [FunDef "print_pile" [Param "pile"]
    (MatchExpr (FunAppExpr "pile" [])
      [Match (ConstrPattern "Left" []) (FunAppExpr "print_string" [StringCExpr "left"]),
       Match (ConstrPattern "Middle" []) (FunAppExpr "print_string" [StringCExpr "middle"]),
       Match (ConstrPattern "Right" []) (FunAppExpr "print_string" [StringCExpr "right"])])]),
  Left (Let [FunDef "main" []
    (LetIn (Let [FunDef "move" [Param "source",Param "target"]
      (BinOpExpr SemicolonOp
        (BinOpExpr SemicolonOp
          (BinOpExpr SemicolonOp
            (BinOpExpr SemicolonOp
              (FunAppExpr "print_string" [StringCExpr "Moving from: "])
              (FunAppExpr "print_pile" [FunAppExpr "source" []]))
              (FunAppExpr "print_string" [StringCExpr " to "]))
              (FunAppExpr "print_pile" [FunAppExpr "target" []]))
              (FunAppExpr "print_string" [StringCExpr "\\n"]))])
      (LetIn (LetRec [FunDef "hanoi" [Param "rings",Param "source",Param "target",Param "auxil"]
        (IfThenExpr (BinOpExpr GTOp (FunAppExpr "rings" []) (IntCExpr 0))
          (BeginExpr
            (BinOpExpr SemicolonOp
              (BinOpExpr SemicolonOp
                (FunAppExpr "hanoi" [BinOpExpr MinusOp (FunAppExpr "rings" []) (IntCExpr 1),FunAppExpr "source" [],FunAppExpr "auxil" [],FunAppExpr "target" []])
                (FunAppExpr "move" [FunAppExpr "source" [],FunAppExpr "target" []]))
                (FunAppExpr "hanoi" [BinOpExpr MinusOp (FunAppExpr "rings" []) (IntCExpr 1),FunAppExpr "auxil" [],FunAppExpr "target" [],FunAppExpr "source" []]))))])
        (BinOpExpr SemicolonOp
          (FunAppExpr "print_string" [StringCExpr "Please, give the number of rings: "])
          (LetIn (Let [FunDef "n" [] (FunAppExpr "read_int" [UnitCExpr])])
            (FunAppExpr "hanoi" [FunAppExpr "n" [],ConstrAppExpr "Left" [],ConstrAppExpr "Right" [],ConstrAppExpr "Middle" []])))))])]

primesAST :: Program
primesAST =
  [Left (LetRec [FunDef "prime" [Param "n"]
    (IfThenElseExpr (BinOpExpr LTOp (FunAppExpr "n" []) (IntCExpr 0))
      (FunAppExpr "prime" [UnOpExpr MinusUnOp (FunAppExpr "n" [])])
      (IfThenElseExpr (BinOpExpr LTOp (FunAppExpr "n" []) (IntCExpr 2))
        FalseCExpr
        (IfThenElseExpr (BinOpExpr EqOp (FunAppExpr "n" []) (IntCExpr 2))
          TrueCExpr
          (IfThenElseExpr (BinOpExpr EqOp (BinOpExpr ModOp (FunAppExpr "n" []) (IntCExpr 2)) (IntCExpr 0))
          FalseCExpr
          (LetIn (LetRec [FunDef "loop" [Param "i"]
            (IfThenElseExpr (BinOpExpr LEqOp (FunAppExpr "i" []) (BinOpExpr DivOp (FunAppExpr "n" []) (IntCExpr 2)))
              (IfThenElseExpr (BinOpExpr EqOp (BinOpExpr ModOp (FunAppExpr "n" []) (FunAppExpr "i" [])) (IntCExpr 0))
                FalseCExpr
                (FunAppExpr "loop" [BinOpExpr PlusOp (FunAppExpr "i" []) (IntCExpr 2)]))
              TrueCExpr)])
            (FunAppExpr "loop" [IntCExpr 3]))))))]),

  Left (Let [FunDef "main" []
    (BinOpExpr SemicolonOp
      (FunAppExpr "print_string" [StringCExpr "Please, give the upper limit: "])
      (LetIn (Let [FunDef "limit" [] (FunAppExpr "read_int" [UnitCExpr])])
        (BinOpExpr SemicolonOp
          (BinOpExpr SemicolonOp
            (BinOpExpr SemicolonOp
              (FunAppExpr "print_string" [StringCExpr "Prime numbers between 0 and "])
              (FunAppExpr "print_int" [FunAppExpr "limit" []]))
              (FunAppExpr "print_string" [StringCExpr "\\n\\n"]))
              (LetIn (Let [VarDef "counter"])
                (BinOpExpr SemicolonOp
                  (BinOpExpr SemicolonOp
                    (BinOpExpr SemicolonOp
                      (BinOpExpr AssignMutableOp (FunAppExpr "counter" []) (IntCExpr 0))
                      (IfThenExpr (BinOpExpr GEqOp (FunAppExpr "limit" []) (IntCExpr 2))
                        (BinOpExpr SemicolonOp
                          (FunAppExpr "incr" [FunAppExpr "counter" []])
                          (FunAppExpr "print_string" [StringCExpr "2\\n"]))))
                      (IfThenExpr (BinOpExpr GEqOp (FunAppExpr "limit" []) (IntCExpr 3))
                        (BinOpExpr SemicolonOp
                          (FunAppExpr "incr" [FunAppExpr "counter" []])
                          (FunAppExpr "print_string" [StringCExpr "3\\n"]))))
                      (LetIn (LetRec [FunDef "loop" [Param "number"]
                        (IfThenExpr (BinOpExpr LEqOp (FunAppExpr "number" []) (FunAppExpr "limit" []))
                          (BeginExpr
                            (BinOpExpr SemicolonOp
                              (BinOpExpr SemicolonOp
                              (IfThenExpr (FunAppExpr "prime" [BinOpExpr MinusOp (FunAppExpr "number" []) (IntCExpr 1)])
                              (BeginExpr
                                (BinOpExpr SemicolonOp
                                  (BinOpExpr SemicolonOp
                                    (FunAppExpr "incr" [FunAppExpr "counter" []])
                                    (FunAppExpr "print_int" [BinOpExpr MinusOp (FunAppExpr "number" []) (IntCExpr 1)]))
                                    (FunAppExpr "print_string" [StringCExpr "\\n"]))))
                                (IfThenExpr (BinOpExpr AndOp (BinOpExpr NotEqOp (FunAppExpr "number" []) (FunAppExpr "limit" [])) (FunAppExpr "prime" [BinOpExpr PlusOp (FunAppExpr "number" []) (IntCExpr 1)]))
                                  (BeginExpr
                                    (BinOpExpr SemicolonOp
                                      (BinOpExpr SemicolonOp
                                        (FunAppExpr "incr" [FunAppExpr "counter" []])
                                        (FunAppExpr "print_int" [BinOpExpr PlusOp (FunAppExpr "number" []) (IntCExpr 1)]))
                                        (FunAppExpr "print_string" [StringCExpr "\\n"])))))
                                    (FunAppExpr "loop" [BinOpExpr PlusOp (FunAppExpr "number" []) (IntCExpr 6)]))))])
                      (BinOpExpr SemicolonOp
                        (BinOpExpr SemicolonOp
                          (BinOpExpr SemicolonOp
                            (FunAppExpr "loop" [IntCExpr 6])
                            (FunAppExpr "print_string" [StringCExpr "\\n"]))
                            (FunAppExpr "print_int" [UnOpExpr BangOp (FunAppExpr "counter" [])]))
                            (FunAppExpr "print_string" [StringCExpr " prime number(s) were found.\\n"]))))))))])]
