module Semantics.Analyzer where

import Common.AST
import Common.PrintAST (Pretty(pretty))
import Common.SymbolTable
import Common.Token (Identifier, ConstrIdentifier)
import Parser.ParserM (Parser)

data Tag = TypedTag Type
    deriving Eq


data SemanticError = Error String
    | TypeCheckError String
    | TypeMismach String
    | NotInScope String

class Analyzable a where
    analyze :: a () -> Parser (a Tag)

instance (Analyzable a, Analyzable b) => Analyzable (Either a b) where
    analyze (Left a)  = analyze a
    analyze (Right b) = analyze b

class Analyzable a => Typeable a where
    infer :: a () -> Parser (a Tag)
    typeCheck :: a () -> Type -> Parser (a Tag)
    typeCheck a t = do
        inferedType <- infer a
        if TypedTag t == inferedType
        then return ()
        else throwE $ TypeCheckError $ "Expected: " ++ pretty t ++ " but got: " ++ pretty inferedType

typeCheckParams :: [Expr] -> [Type] -> Sem ()
typeCheckParams [] [] = return ()
typeCheckParams [] _  = throwE $ TypeCheckError $
    "Expected more params. Maybe the function is missing some arguments"
typeCheckParams _ []  = throwE $ TypeCheckError $
    "Expected less params. Maybe the function is applied to too many arguments"
typeCheckParams (e:es) (t:ts) = do
    typeCheck e t
    typeCheckParams es ts

instance Typeable Expr where
    infer e = case e of
        IntCExpr _        -> return IntType (TypedTag IntType)
        FloatCExpr _      -> return FloatType (TypedTag FloatType)
{-
        CharCExpr _       -> return CharType
        StringCExpr v     -> return $ ArrayType (length v) CharType
        TrueCExpr         -> return BoolType
        FalseCExpr        -> return BoolType
        UnitCExpr         -> return UnitType
        UnOpExpr op v     -> inferUnOp op v
        BinOpExpr o v u   -> inferBinOp o v u
        FunAppExpr f ps   -> inferFunApp f ps
        ConstrAppExpr f p -> inferConstrApp f p
        ArrayAccess ar xs -> inferArrayAccess ar xs
        ArrayDim ar i     -> do
            sym <- getSymbols
            case query ar sym of
                Just (ArrayEntry _ n) ->
                    if 0 < i && i <= n then return IntType
                    else throwE $ Error $
                    "Querying for the " ++ show i ++ " dimension of an " ++ show n ++ "-dim array"
                _ -> throwE $ NotInScope $ "Array " ++ ar ++ " not in scope"
        NewType t         -> case t of
            ArrayType _ _ -> throwE $ Error "Cannot create reference to a table"
            _             -> return $ RefType t
        DeleteExpr e      -> do
            t <- infer e
            case t of
                RefType _ -> return UnitType
                _         -> throwE $ Error "Cannot apply delete on a term which is not a reference"
        -- LetIn def e       ->
        BeginExpr e       -> infer e
        IfThenExpr cond e -> do
            ct <- infer cond
            et <- infer e
            if ct /= BoolType then throwE $ Error $ "If condition should be of type " ++ pretty BoolType
            else if et /= UnitType then throwE $ Error $ "If-then expression should be of type " ++ pretty UnitType
                else return UnitType
        IfThenElseExpr c u v -> do
            ct <- infer c
            ut <- infer u
            vt <- infer v
            if ct /= BoolType then throwE $ Error $ "If condition should be of type " ++ pretty BoolType
            else if ut /= vt then throwE $ Error "If-then-else expressions should be of the same type"
                else return ut
        WhileExpr c u     -> do
            ct <- infer c
            ut <- infer u
            if ct /= BoolType then throwE $ Error $ "While condition should be of type " ++ pretty BoolType
            else if ut /= UnitType then throwE $ Error $ "While expression should be of type " ++ pretty UnitType
                else return UnitType
        ForExpr x s t u   -> inferFor x s t u
        ForDownExpr x s t u -> inferFor x s t u
        -- MatchExpr e cs    ->

inferUnOp :: UnOp -> Expr -> Sem Type
inferUnOp op e = do
    t <- infer e
    case (op, t) of
        (PlusUnOp, IntType)         -> return IntType
        (MinusUnOp, IntType)        -> return IntType
        (PlusFloatUnOp, FloatType)  -> return FloatType
        (MinusFloatUnOp, FloatType) -> return FloatType
        (BangOp, RefType s)         -> return s
        (NotOp, BoolType)           -> return BoolType
        (o, s)                      -> semError $ TypeMismach $
            "Unable to apply unary operator '" ++ show o ++ "' to expression of type: " ++ pretty s

inferBinOp :: BinOp -> Expr -> Expr -> Sem Type
inferBinOp op v u = do
    t1 <- infer v
    t2 <- infer u
    let eqCheck (FunType _ _ ) _   = semError $ TypeMismach "Cannot check for equality functions"
        eqCheck (ArrayType _ _ ) _ = semError $ TypeMismach "Cannot check for equality arrays"
        eqCheck _ (FunType _ _ )   = semError $ TypeMismach "Cannot check for equality functions"
        eqCheck _ (ArrayType _ _ ) = semError $ TypeMismach "Cannot check for equality arrays"
        eqCheck s t = if s == t then return BoolType
                      else semError $ TypeMismach "Cannot check for equality terms of unequal types"
    let compCheck t s
          | t /= IntType || t /= FloatType || t /= CharType || s /= IntType || s /= FloatType || s /= CharType
                      = semError $ TypeMismach $
                        "Cannot compare terms of types other than "
                        ++ pretty IntType ++ ", " ++ pretty FloatType ++ " and " ++ pretty CharType
          | t == s    = return BoolType
          | otherwise = semError $ TypeMismach "Cannot compare terms of unequal types"
    case (op, t1, t2) of
        (PlusOp, IntType, IntType)           -> return IntType
        (MinusOp, IntType, IntType)          -> return IntType
        (TimesOp, IntType, IntType)          -> return IntType
        (DivOp, IntType, IntType)            -> return IntType
        (ModOp, IntType, IntType)            -> return IntType
        (PlusFloatOp, FloatType, FloatType)  -> return FloatType
        (MinusFloatOp, FloatType, FloatType) -> return FloatType
        (TimesFloatOp, FloatType, FloatType) -> return FloatType
        (DivFloatOp, FloatType, FloatType)   -> return FloatType
        (ExpOp, FloatType, FloatType)        -> return FloatType
        (EqOp, _, _)                         -> eqCheck t1 t2
        (NotEqOp, _, _)                      -> eqCheck t1 t2
        (NatEqOp, _, _)                      -> eqCheck t1 t2
        (NotNatEqOp, _, _)                   -> eqCheck t1 t2
        (LTOp, _, _)                         -> compCheck t1 t2
        (GTOp, _, _)                         -> compCheck t1 t2
        (LEqOp, _, _)                        -> compCheck t1 t2
        (GEqOp, _, _)                        -> compCheck t1 t2
        (AndOp, BoolType, BoolType)          -> return BoolType
        (OrOp, BoolType, BoolType)           -> return BoolType
        (AssignMutableOp, RefType r, s)      ->
            if r == s then return UnitType
            else semError $ TypeMismach $
            "Cannot assign expression of type " ++ pretty s ++ " to variable of type " ++ pretty r
        (SemicolonOp, _, _)                  -> return t2
        _                                    -> semError $ TypeMismach $
            "Cannot apply binary operator " ++ show op ++ " to expressions of type " ++
            pretty t1 ++ " and " ++ pretty t2

inferFunApp :: Identifier -> [Expr] -> Sem Type
inferFunApp f ps = do
    sym <- getSymbols
    case query f sym of
        Just (FunEntry fType params rType) -> do
            if ps == [] then return fType
            else do
                typeCheckParams ps (map snd params)
                return rType
        _ -> throwE $ NotInScope $ "Variable " ++ f ++ " not in scope"

inferConstrApp :: ConstrIdentifier -> [Expr] -> Sem Type
inferConstrApp f ps = do
    sym <- getSymbols
    case query f sym of
        Just (ConstrEntry fType params rType) -> do
            if ps == [] then return fType
            else do
                typeCheckParams ps params
                return rType
        _ -> throwE $ NotInScope $ "Constructor " ++ f ++ " not in scope"

inferArrayAccess :: Identifier -> [Expr] -> Sem Type
inferArrayAccess ar xs = do
    sym <- getSymbols
    case query ar sym of
        Just (ArrayEntry t i) -> do
            if i /= length xs then throwE $ Error $
                "Trying to access an array with " ++ show i ++ " dimensions with "
                ++ show (length xs) ++ " arguements"
            else do
                typeCheckParams xs (map (const IntType) xs)
                return $ RefType t
        _ -> throwE $ NotInScope $ "Array " ++ ar ++ " not in scope"


inferFor :: Identifier -> Expr -> Expr -> Expr -> ExceptT SemanticError (State SymbolTable) Type
inferFor x s t u = do
    st <- infer s
    tt <- infer t
    if st /= IntType || tt /= IntType then throwE $ Error $ "For bounds should be of type " ++ pretty IntType
    else do
        sym <- getSymbols
        let sym' = insert x (VarEntry IntType) (openScope sym)
        putSymbols sym'
        ut <- infer u
        sym'' <- getSymbols
        putSymbols (closeScope sym'')
        if ut == UnitType then return UnitType
        else throwE $ Error $ "For body should be of type " ++ pretty UnitType
-}