module Common.SymbolTable (SymbolTable,
                           TableEntry(..),
                           typeToSymbolType,
                           emptySymbolTable,
                           query,
                           insert,
                           update,
                           openScope,
                           closeScope,
                           varTypeKey) where

import qualified Data.Map as M
import Common.Token (Identifier, ConstrIdentifier)
import Common.PrintAST (Pretty (pretty))
import Common.SymbolType (SymbolType(TVar), TypeScheme, ConstType,
    typeToSymbolType)
import Data.List (intercalate)

-- This module contains the defintion of the Symbol table for the compiler

type Scope = M.Map

newtype Context k e = Context [Scope k e]
    deriving Show

-- Symbol table operations
emptyContext :: Context k e
emptyContext = Context []

query :: Ord k => k -> Context k e -> Maybe e
query _ (Context []) = Nothing
query k (Context (scope:scopes)) =
    case M.lookup k scope of
        Nothing -> query k (Context scopes)
        Just e  -> Just e

insert :: Ord k => k -> e -> Context k e -> Context k e
insert k e (Context scopes) = case scopes of
    []   -> Context [M.insert k e M.empty]
    s:tl -> Context $ updated:tl where
        updated = M.insert k e s

update :: Ord k => k -> e -> Context k e -> Context k e
update k e (Context s) = Context (aux s) where
    aux [] = []
    aux (scope:scopes) = if M.member k scope
        then M.update (const $ Just e) k scope : scopes
        else scope : aux scopes

openScope :: Context k e -> Context k e
openScope (Context scopes) = Context $ M.empty:scopes

closeScope :: Context k e -> Context k e
closeScope (Context scopes) = Context s where
    s = if null scopes then [] else tail scopes


-- Definitions for symbol table and table entries
type SymbolTable = Context String TableEntry

emptySymbolTable :: SymbolTable
emptySymbolTable = emptyContext

varTypeKey :: Int -> String
varTypeKey v = pretty (TVar v)

data TableEntry
    = MutableEntry SymbolType                     -- Type of the mutable variable
    | ArrayEntry SymbolType Int                   -- Type of the entries, num of dimensions
    | FunEntry TypeScheme [Identifier]            -- TypeScheme of the function, params
    | ParamEntry SymbolType Identifier            -- Type of the param, function of the param
    | TypeEntry [(ConstrIdentifier, [ConstType])] -- Constructors and arguements
    | ConstrEntry ConstType [ConstType] ConstType -- Type of constructor, params, output type
    | TVarEntry SymbolType                        -- This is the principal type of the most general unifier
        deriving Show

-- Pretty printing of symbol table
instance Pretty TableEntry where
    pretty entry = case entry of
        MutableEntry t ->
            "Mutable var of type: " ++ pretty t
        ArrayEntry t dim ->
            show dim ++ "-dimensional array of type: " ++ pretty t
        FunEntry funType [] ->
            "Const of type: " ++ pretty funType
        FunEntry funType params ->
            "Fun of type: " ++ pretty funType ++ " with params: " ++ intercalate ", " params
        ParamEntry t f ->
            "Param of type: " ++ pretty t ++ " of function " ++ f
        TypeEntry constrs ->
            "Type with constrs: " ++ cs where
                f (c, []) = c
                f (c, ps) = c ++ " of " ++ unwords (map pretty ps)
                cs = intercalate ", " (map f constrs)
        ConstrEntry constrType [] outputType ->
            "Constr of " ++ pretty outputType ++
            " with type: " ++ pretty constrType
        ConstrEntry constrType types outputType ->
            "Constr of " ++ pretty outputType ++
            " with type: " ++ pretty constrType ++
            " with params: (" ++ intercalate ", " (map pretty types) ++ ")"
        TVarEntry t -> "Type Var unified to: " ++ pretty t

instance (Show k, Pretty e) => Pretty (Context k e) where
    pretty (Context scopes) =
        let addPadding v l = v ++ replicate (max 0 (l - length v)) ' '
            toString (k, e) = (show k, pretty e)
            toLengths (k, e) = (length k, length e)
            toPaddings (ak, ek) (k, e) = (max ak k, max ek e)
            scopeToString sc = map toString $ M.assocs sc
            stringsToLengths = map toLengths
            lengthsToPaddings = foldl toPaddings (0, 0)
            strings = map scopeToString scopes
            lengths = map stringsToLengths strings
            paddings = map lengthsToPaddings lengths
            (lk', le') = lengthsToPaddings paddings
            (lk, le) = (max lk' $ length "Keys", max le' $ length "Entries")
            totalLength = 1 + 1 + lk + 1 + 1 + 1 + le + 1 + 1
            line = replicate totalLength '-' ++ "\n"
            printLine acc (k, e) = acc ++ "| " ++ addPadding k lk ++ " | " ++ addPadding e le ++ " |\n"
            printScope pdStrs = line ++ foldl printLine "" pdStrs
            scopesTables = map printScope strings
            scps = concat scopesTables
        in printLine line ("Keys", "Entries") ++ scps ++ line
