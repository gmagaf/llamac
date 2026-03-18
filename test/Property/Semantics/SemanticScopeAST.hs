module Property.Semantics.SemanticScopeAST (module Property.Semantics.SemanticScopeAST) where

import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Test.QuickCheck

import Common.Token (Identifier)
import Common.AST
import Common.SymbolType
import Common.PrintAST (pretty)
import Semantics.Utils (SemanticTag (..), TypeInfo (..), hasDuplicates)

import Property.Utils
import Property.Lexer.ArbitraryTokens
    ( arbIdWithLength,
      arbitraryIntConstant,
      arbitraryFloatConstant,
      arbitraryCharConstant )

-- Define data structures for simple scoping
type Scope = M.Map String ConstType
type TypeScope = S.Set PosnId

-- Utils for generating constants
arbId :: Gen Identifier
arbId = ("id_" ++) <$> arbIdWithLength 7

arbFromScope :: Scope -> ConstType -> Gen (Maybe Identifier)
arbFromScope s t = if not $ null res then Just <$> elements res else return Nothing
  where res = map fst $ filter ((t ==) . snd) $ M.toList s

-- Generator for semantically correct programs
-- that define functions

semanticScopesAST :: Gen (AST SemanticTag)
semanticScopesAST = sized $ \n -> do
  k <- choose (0 :: Int, logSize n :: Int)
  AST <$> f M.empty S.empty k <*> arbTag NotTypable where
    f :: Scope -> TypeScope -> Int -> Gen [Either (LetDef SemanticTag) (TypeDef SemanticTag)]
    f _ _ 0 = return []
    f s ts k = do
      (ldef, ls) <- arbLetDef s ts
      (tdef, s', ts') <- arbSimpleTypeDef
      (d, s'', ts'') <- frequency
        [(10, return (Left ldef, ls, ts)),
         (1, return (Right tdef, s', ts'))]
      (d:) <$> f s'' ts'' (k - 1)

-- Utils for generating some basic user defined types
arbSimpleTypeDef :: Gen (TypeDef SemanticTag, Scope, TypeScope)
arbSimpleTypeDef =
    let constrsMap tId cPrefix =
            let t = constUserT tId
            in M.fromList [(cPrefix ++ "1", t)
                         , (cPrefix ++ "2", ConstType (FunType intConstType t))
                         , (cPrefix ++ "3", ConstType (FunType t t))]
        tDefGen tId cPrefix = do
            b1 <- arbTag NotTypable
            b2 <- arbTag NotTypable
            b3 <- arbTag NotTypable
            bInt <- arbTag NotTypable
            bU <- arbTag NotTypable
            bt <- arbTag NotTypable
            TypeDef [TDef tId [ Constr (cPrefix ++ "1") [] b1
                              , Constr (cPrefix ++ "2") [Type IntType bInt] b2
                              , Constr (cPrefix ++ "3") [Type (UserDefinedType tId) bU] b3
                              ] bt] <$> arbTag NotTypable
    in do
        tD1 <- tDefGen "t1" "C"
        tD2 <- tDefGen "t2" "D"
        tD3 <- tDefGen "t3" "E"
        tD4 <- tDefGen "t4" "F"
        elements
            [ (tD1, constrsMap "t1" "C", S.fromList [addPosn "t1"])
            , (tD2, constrsMap "t2" "D", S.fromList [addPosn "t2"])
            , (tD3, constrsMap "t3" "E", S.fromList [addPosn "t3"])
            , (tD4, constrsMap "t4" "F", S.fromList [addPosn "t4"])
            ]

arbSimpleType :: TypeScope -> Gen ConstType
arbSimpleType ts = sized gen where
  user = map UserDefinedType (S.toList ts)
  basicTf = [UnitType, IntType, CharType, BoolType, FloatType] ++ user
  gen 0 = do
    tf <- elements basicTf
    return (ConstType tf)
  gen n = do
    let r = gen (div n 2)
    i <- choose (1, 3) :: Gen Int
    oneof [r,
           ConstType . RefType <$> r,
           ConstType . ArrayType i <$> r,
           ConstType <$> (FunType <$> r <*> r)]

arbUserType :: TypeScope -> Gen (Maybe ConstType)
arbUserType ts = if not $ null ts then Just <$> gen else return Nothing
  where gen = ConstType . UserDefinedType <$> elements (S.toList ts)

-- Generate let definitions
arbLetDef :: Scope -> TypeScope -> Gen (LetDef SemanticTag, Scope)
arbLetDef s ts = sized $ \n -> do
  ids <- boundedListOf (1, logSize n) arbId
  let idc = length ids
  types <- listGen idc (arbSimpleType ts)
  let entries = zip ids types
  let s' = M.union s (M.fromList entries)
  isRec <- elements [True, False]
  b <- arbTag NotTypable
  if isRec
    then do
      -- Let rec -> run with s'
      defs <- mapM (arbDef (s', ts)) entries
      return (LetRec defs b, s')
    else do
      -- Let case -> run with s
      defs <- mapM (arbDef (s, ts)) entries
      return (Let defs b, s')

-- Generate definition in scope
arbDef :: (Scope, TypeScope) -> (Identifier, ConstType) -> Gen (Def SemanticTag)
arbDef (s, ts) (i, t@(ConstType tf)) = do
  let tg = arbTag (DefType . MonoType . constTypeToSymbolType $ t)
  case tf of
    RefType t'       -> do
      tt <- ctToType t'
      frequency [(0, VarDef i Nothing <$> tg), (1, VarDef i (Just tt) <$> tg)]
    ArrayType dim t' -> do
      tt <- ctToType t'
      dims <- listGen dim (arbExpr (s, ts) intConstType)
      frequency [(0, ArrayDef i dims Nothing <$> tg), (1, ArrayDef i dims (Just tt) <$> tg)]
    FunType _ _      -> do
      let argTypes = funToArgs t
          pc       = length argTypes
          outT     = outFunType t
      pids <- suchThat (listGen pc arbId) (not . hasDuplicates)
      let paramsScope = zip pids argTypes
      ps <- mapM arbParam paramsScope
      let s' = M.union s (M.fromList paramsScope)
      e <- arbExpr (s', ts) outT
      tt <- ctToType outT
      frequency [(0, FunDef i ps Nothing e <$> tg), (1, FunDef i ps (Just tt) e <$> tg)]
    _ -> do
      tt <- ctToType t
      e <- arbExpr (s, ts) t
      frequency [(0, FunDef i [] Nothing e <$> tg), (1, FunDef i [] (Just tt) e <$> tg)]

-- Generate param
arbParam :: (Identifier, ConstType) -> Gen (Param SemanticTag)
arbParam (i, t) =
  let tg = arbTag (NodeType . constTypeToSymbolType $ t)
  in do
    tt <- ctToType t
    frequency [(0, Param i <$> tg), (1, TypedParam i tt <$> tg)]

-- Generate expressions in scope of type
arbExpr :: (Scope, TypeScope) -> ConstType -> Gen (Expr SemanticTag)
arbExpr scps@(s, ts) t = sized gen where
  gen 0 = baseExprGen scps t
  gen n = do
    let tg = arbTag (NodeType (constTypeToSymbolType t))
        r scope = resize (div n 2) . arbExpr (scope, ts)
        r' = resize (div n 2) . arbExpr scps
    frequency [ (1, r s t)
              , (2, unOpGen scps r t)
              , (2, binOpGen scps r t)
              , (3, funAppGen s r' t)
              , (2, ifGen s r' t)
              , (2, loopGen s r t)
              , (1, (Expr . BeginExpr <$> r' t) <*> tg)
              , (2, arrayAccGen s r t)
              -- TODO: Add pattern matching generator
              ]

baseExprGen :: (Scope, TypeScope) -> ConstType -> Gen (Expr SemanticTag)
baseExprGen (s, ts) t@(ConstType tf) = do
  -- Prepare tag with arbitrary position
  let tg = arbTag (NodeType (constTypeToSymbolType t))
  -- Create constant generator from scope
  scopeRes <- arbFromScope s t
  let idToConst i | isUpper (head i) = Expr (ConstConstrExpr i) <$> tg
                  | otherwise        = Expr (ConstExpr i) <$> tg
  let constGen = fmap idToConst scopeRes
  -- Create generator for base expressions
  let baseExpr = case tf of
        IntType      -> Expr . IntCExpr <$> arbitraryIntConstant <*> tg
        FloatType    -> Expr . FloatCExpr <$> arbitraryFloatConstant <*> tg
        CharType     -> Expr . CharCExpr <$> arbitraryCharConstant <*> tg
        ArrayType 1 (ConstType CharType)
                     -> Expr (StringCExpr "TEST_STRING") <$> tg
        BoolType     -> Expr <$> elements [TrueCExpr, FalseCExpr] <*> tg
        UnitType     -> Expr UnitCExpr <$> tg
        UserDefinedType ti -> fromMaybe
          (error $ "Impossible to create base expression of user defined type: " ++ pretty ti) constGen
        RefType (ConstType (ArrayType {})) -> do
          ntTg <- arbTag NotTypable
          b <- arbTag (NodeType (constTypeToSymbolType t))
          def <- resize 0 $ arbDef (s, ts) ("id_ref_arr", t)
          LetIn (Let [def] ntTg) (Expr (ConstExpr "id_ref_arr") b) <$> tg
        RefType t'   -> do
          ct' <- ctToType t'
          NewType ct' <$> tg
        ArrayType {} -> do
          ntTg <- arbTag NotTypable
          b <- arbTag (NodeType (constTypeToSymbolType t))
          def <- resize 0 $ arbDef (s, ts) ("id_arr", t)
          LetIn (Let [def] ntTg) (Expr (ConstExpr "id_arr") b) <$> tg
        FunType {}   -> do
          ntTg <- arbTag NotTypable
          b <- arbTag (NodeType (constTypeToSymbolType t))
          def <- resize 0 $ arbDef (s, ts) ("id_fun", t)
          LetIn (Let [def] ntTg) (Expr (ConstExpr "id_fun") b) <$> tg
  case constGen of
    Nothing -> baseExpr
    Just cg -> frequency [(2, cg), (1, baseExpr)]

typeIsRef :: ConstType -> Bool
typeIsRef ct = case ct of
  ConstType (RefType _) -> True
  _                     -> False

typeIsArray :: ConstType -> Bool
typeIsArray ct = case ct of
  ConstType (ArrayType {}) -> True
  _                        -> False

unOpGen :: (Scope, TypeScope) -> (Scope -> ConstType -> Gen (Expr SemanticTag)) -> ConstType -> Gen (Expr SemanticTag)
unOpGen (s, ts) r t@(ConstType tf) = case tf of
  IntType -> do
    op <- elements [PlusUnOp, MinusUnOp]
    e <- r s t
    let arrs = M.filter typeIsArray s
    if null arrs
      then Expr (UnOpExpr op e) <$> tg
      else do
        (arr, arrT) <- elements (M.toList arrs)
        case arrT of
          ConstType (ArrayType dims _) -> do
            d <- elements [1..dims]
            frequency [(1, Expr (ArrayDim arr d) <$> tg), (2, Expr (UnOpExpr op e) <$> tg)]
          _ -> error "Impossible case in unOpGen"
  FloatType -> do
    op <- elements [PlusFloatUnOp, MinusFloatUnOp]
    e <- r s t
    Expr (UnOpExpr op e) <$> tg
  BoolType -> do
    e <- r s t
    Expr (UnOpExpr NotOp e) <$> tg
  UnitType -> do
    refT <- arbSimpleType ts
    refE <- r s (ConstType (RefType refT))
    Expr (DeleteExpr refE) <$> tg
  _ -> do
    e <- r s (ConstType (RefType t))
    (ldef, s') <- arbLetDef s ts
    let letGen = LetIn  ldef <$> r s' t <*> tg
    frequency [(1, Expr (UnOpExpr BangOp e) <$> tg), (1, letGen), (1, r s t)]
  where
    tg = arbTag (NodeType (constTypeToSymbolType t))

binOpGen :: (Scope, TypeScope) -> (Scope -> ConstType -> Gen (Expr SemanticTag)) -> ConstType -> Gen (Expr SemanticTag)
binOpGen (s, ts) r t@(ConstType tf) = case tf of
  IntType -> do
    op <- elements [PlusOp, MinusOp, TimesOp, DivOp, ModOp]
    e1 <- r s t
    e2 <- r s t
    Expr (BinOpExpr op e1 e2) <$> tg
  FloatType -> do
    op <- elements [PlusFloatOp, MinusFloatOp, TimesFloatOp, DivFloatOp, ExpOp]
    e1 <- r s t
    e2 <- r s t
    Expr (BinOpExpr op e1 e2) <$> tg
  BoolType -> do
    op <- elements [AndOp, OrOp]
    e1 <- r s t
    e2 <- r s t
    let bool = Expr (BinOpExpr op e1 e2) <$> tg
    let compTypes = [charConstType, intConstType, floatConstType]
    compT <- elements compTypes
    compOp <- elements [LTOp, GTOp, LEqOp, GEqOp]
    compE1 <- r s compT
    compE2 <- r s compT
    let comp = Expr (BinOpExpr compOp compE1 compE2) <$> tg
    let eqTypes = compTypes ++ [unitConstType, boolConstType] ++ map (ConstType . RefType) compTypes
    eqT <- elements eqTypes
    eqOp <- elements [EqOp, NotEqOp, NatEqOp, NotNatEqOp]
    eqE1 <- r s eqT
    eqE2 <- r s eqT
    let eq = Expr (BinOpExpr eqOp eqE1 eqE2) <$> tg
    oneof [bool, comp, eq]
  UnitType -> do
    e1 <- r s (ConstType (RefType t))
    e2 <- r s t
    Expr (BinOpExpr AssignMutableOp e1 e2) <$> tg
  _ -> do
    e1 <- r s unitConstType
    e2 <- r s t
    (ldef, s') <- arbLetDef s ts
    let letGen = LetIn  ldef <$> r s' t <*> tg
    frequency [(1, Expr (BinOpExpr SemicolonOp e1 e2) <$> tg), (1, letGen), (1, r s t)]
  where
    tg = arbTag (NodeType (constTypeToSymbolType t))

funAppGen :: Scope -> (ConstType -> Gen (Expr SemanticTag)) -> ConstType -> Gen (Expr SemanticTag)
funAppGen s r t =
  let funs = M.filter ((t ==) . outFunType) s
  in if null funs
    then r t
    else do
      tg <- arbTag (NodeType (constTypeToSymbolType t))
      (fun, ct) <- elements (M.toList funs)
      let argTypes = funToArgs ct
      args <- mapM r argTypes
      if isUpper (head fun)
      then if null args then return $ Expr (ConstConstrExpr fun) tg
                        else return $ Expr (ConstrAppExpr fun args) tg
      else if null args then return $ Expr (ConstExpr fun) tg
                        else return $ Expr (FunAppExpr fun args) tg

ifGen :: Scope -> (ConstType -> Gen (Expr SemanticTag)) -> ConstType -> Gen (Expr SemanticTag)
ifGen _ r t@(ConstType tf) = case tf of
  UnitType -> do
    b <- arbTag (NodeType (constTypeToSymbolType t))
    condExpr <- r boolConstType
    e <- r t
    return (Expr (IfThenExpr condExpr e) b)
  _ -> do
    b <- arbTag (NodeType (constTypeToSymbolType t))
    condExpr <- r boolConstType
    e1 <- r t
    e2 <- r t
    return (Expr (IfThenElseExpr condExpr e1 e2) b)

loopGen :: Scope -> (Scope -> ConstType -> Gen (Expr SemanticTag)) -> ConstType -> Gen (Expr SemanticTag)
loopGen s r t@(ConstType tf) = case tf of
  UnitType -> do
    b <- arbTag (NodeType (constTypeToSymbolType t))
    condExpr <- r s boolConstType
    lExpr <- r s intConstType
    uExpr <- r s intConstType
    eWhile <- r s t
    i <- arbId
    eFor <- r (M.insert i intConstType s) t
    elements [Expr (WhileExpr condExpr eWhile) b,
              Expr (ForExpr i lExpr uExpr eFor) b,
              Expr (ForDownExpr i uExpr lExpr eFor) b]
  _ -> do
    b <- arbTag (NodeType (constTypeToSymbolType t))
    e1 <- r s t
    e2 <- r s unitConstType
    return (Expr (BinOpExpr SemicolonOp e2 e1) b)

isArrayOf :: ConstType -> ConstType -> Bool
isArrayOf t (ConstType (ArrayType _ arT)) | t == arT = True
isArrayOf _ _                                        = False

arrayAccGen :: Scope -> (Scope -> ConstType -> Gen (Expr SemanticTag)) -> ConstType -> Gen (Expr SemanticTag)
arrayAccGen s r t@(ConstType tf) = case tf of
  RefType reft -> do
    let arrs = M.filter (isArrayOf reft) s
    if null arrs
      then r s t
      else do
        (arr, arrCt) <- elements (M.toList arrs)
        case arrCt of
          ConstType (ArrayType d _) -> do
            b' <- arbTag (NodeType (constTypeToSymbolType t))
            args <- mapM (const $ r s intConstType) [1..d]
            return (Expr (ArrayAccess arr args) b')
          _ -> error "Impossible case in arrayAccGen"
  _ -> r s t

matchGen :: (Scope, TypeScope) -> (Scope -> ConstType -> Gen (Expr SemanticTag)) -> ConstType -> Gen (Expr SemanticTag)
matchGen (s, ts) r t = sized $ \n -> do
  maybeUt <- arbUserType ts
  case maybeUt of
    Just ut -> do
      me <- r s ut
      clauses <- boundedListOf (1, logSize n) (clauseGen s r ut t)
      MatchExpr me clauses <$> tg
    Nothing -> r s t
  where
    tg = arbTag (NodeType (constTypeToSymbolType t))

clauseGen :: Scope -> (Scope -> ConstType -> Gen (Expr SemanticTag)) -> ConstType -> ConstType -> Gen (Clause SemanticTag)
clauseGen s r pt t = do
  (p, s') <- patternGen s undefined pt
  e <- r s' t
  Match p e <$> arbTag NotTypable

patternGen :: Scope -> (Scope -> ConstType -> Gen (Pattern SemanticTag)) -> ConstType -> Gen (Pattern SemanticTag, Scope)
patternGen = undefined