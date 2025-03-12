module Unit.TestExpectations (module Unit.TestExpectations) where

import Common.AST

helloWorldAST :: Program
helloWorldAST =
  [Left $ Let [FunDef "main" []
    (FunAppExpr "print_string" [StringCExpr "Hello world!\\n"])]
  ]
