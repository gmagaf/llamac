{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Common.SymbolTable (Context,
                           SymbolTable,
                           names,
                           types,
                           FullTableEntry,
                           mkFullTableEntry,
                           mkBasicEntry,
                           basicInfo,
                           optInfo,
                           generated,
                           NameSpace,
                           TypeSpace,
                           TableEntry(..),
                           TypeTableEntry(..),
                           emptySymbolTable,
                           query,
                           partialQuery,
                           insert,
                           partialInsert,
                           update,
                           partialUpdate,
                           openScope,
                           closeScope) where

import qualified Data.Map as M
import Data.List (intercalate)
import Data.Bifunctor (bimap)

import Control.Lens.Prism (Prism', prism')
import Control.Lens (Lens', makeLenses, lens, preview)
import Control.Lens.Getter (Getter, to, view)
import qualified LLVM.AST as L (Operand)
import qualified LLVM.AST.Type as L

import Common.Token (Identifier, ConstrIdentifier)
import Common.PrintAST (Pretty (pretty))
import Common.SymbolType (SymbolType, TypeScheme, ConstType)

-- This module contains the defintion of the Symbol table for the compiler

type Scope = M.Map

newtype Context k e = Context [Scope k e]
    deriving (Show, Functor, Foldable, Traversable)

-- Symbol table operations
emptyContext :: Context k e
emptyContext = Context []

partialQuery :: Ord k => Getter e e' -> k -> Context k e -> Maybe e'
partialQuery _ _ (Context []) = Nothing
partialQuery l k (Context (scope:scopes)) =
    case M.lookup k scope of
        Nothing -> partialQuery l k (Context scopes)
        Just e  -> Just (view l e)

query :: Ord k => k -> Context k e -> Maybe e
query = partialQuery (to id)

partialInsert :: Ord k => (e' -> e) -> k -> e' -> Context k e -> Context k e
partialInsert mk k e (Context scopes) = case scopes of
    []   -> Context [M.insert k (mk e) M.empty]
    s:tl -> Context $ updated:tl where
        updated = M.insert k (mk e) s

insert :: Ord k => k -> e -> Context k e -> Context k e
insert = partialInsert id

partialUpdate :: Ord k => (e' -> e -> e) -> k -> e' -> Context k e -> Context k e
partialUpdate f k e' (Context s) = Context (aux s) where
    aux [] = []
    aux (scope:scopes) = if M.member k scope
        then M.update (Just . f e') k scope : scopes
        else scope : aux scopes

update :: Ord k => k -> e -> Context k e -> Context k e
update = partialUpdate const

openScope :: Context k e -> Context k e
openScope (Context scopes) = Context $ M.empty:scopes

closeScope :: Context k e -> Context k e
closeScope (Context scopes) = Context s where
    s = if null scopes then [] else tail scopes

-- Definitions for symbol table and table entries
data FullTableEntry e g = FullTableEntry {
    _basicInfo :: e,
    _optInfo :: Maybe g
    } deriving Show

mkFullTableEntry :: e -> Maybe g -> FullTableEntry e g
mkFullTableEntry = FullTableEntry

mkBasicEntry :: e -> FullTableEntry e g
mkBasicEntry = flip FullTableEntry Nothing

basicInfo :: Lens' (FullTableEntry e g) e
basicInfo = lens _basicInfo (\e b -> e{_basicInfo = b})

optInfo :: Lens' (FullTableEntry e g) (Maybe g)
optInfo = lens _optInfo (\e g -> e{_optInfo = g})

generated :: Prism' (FullTableEntry e g) (e, g)
generated = prism' (\(e, g) -> FullTableEntry e (Just g)) (\(FullTableEntry e g) -> fmap (e,) g)

data TableEntry
    = MutableEntry SymbolType                     -- Type of the mutable variable
    | ArrayEntry SymbolType Int                   -- Type of the entries, num of dimensions
    | FunEntry TypeScheme [Identifier]            -- TypeScheme of the function, params
    | ParamEntry SymbolType Identifier            -- Type of the param, function of the param
    | PatternEntry SymbolType                     -- Type of the pattern
    | ConstrEntry ConstType [ConstType] ConstType -- Type of constructor, params, output type
        deriving Show

newtype TypeTableEntry
    = TypeEntry [(ConstrIdentifier, [ConstType])] -- Constructors and arguements
        deriving Show

type NameSpace = Context String (FullTableEntry TableEntry L.Operand)
type TypeSpace = Context String (FullTableEntry TypeTableEntry L.Type)
data SymbolTable = SymbolTable {
    _names :: NameSpace,
    _types :: TypeSpace
    }
    deriving Show
makeLenses ''SymbolTable

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable emptyContext emptyContext

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
        ConstrEntry constrType [] outputType ->
            "Constr of " ++ pretty outputType ++
            " with type: " ++ pretty constrType
        PatternEntry t ->
            "Pattern of type " ++ pretty t
        ConstrEntry constrType ts outputType ->
            "Constr of " ++ pretty outputType ++
            " with type: " ++ pretty constrType ++
            " with params: (" ++ intercalate ", " (map pretty ts) ++ ")"

instance (Pretty e, Show g) => Pretty (FullTableEntry e g) where
    pretty entry = pretty (view basicInfo entry) ++
                   maybe "" (\(_, i) -> " additional info: " ++ show i) (preview generated entry)

instance Pretty TypeTableEntry where
    pretty entry = case entry of
        TypeEntry constrs ->
            "Type with constrs: " ++ cs where
                f (c, []) = c
                f (c, ps) = c ++ " of " ++ unwords (map pretty ps)
                cs = intercalate ", " (map f constrs)

instance (Show k, Pretty e) => Pretty (Context k e) where
    pretty (Context scopes) =
        let -- Utils for each record
            toString = bimap show pretty
            toLengths = bimap length length
            toPaddings (accK, accE) = bimap (max accK) (max accE)
            addPadding v l = v ++ replicate (max 0 (l - length v)) ' '
            -- Utils for each scope
            scopeToString = M.foldMapWithKey (\k e -> [toString (k, e)])
            stringsToLengths = map toLengths
            lengthsToPaddings = foldr toPaddings (0, 0)
            -- Compute values for all context
            strings = map scopeToString scopes
            lengths = map stringsToLengths strings
            paddings = map lengthsToPaddings lengths
            -- Compute total lengths
            (lk', le') = lengthsToPaddings paddings
            (lk, le) = (max lk' $ length "Keys", max le' $ length "Entries")
            totalLength = 1 + 1 + lk + 1 + 1 + 1 + le + 1 + 1
            line = replicate totalLength '-' ++ "\n"
            -- Utils for printing a line
            printLine (k, e) acc = "| " ++ addPadding k lk ++ " | " ++ addPadding e le ++ " |\n" ++ acc
            printScope scope acc = foldr printLine (line ++ acc) scope
            scopesTables = foldr printScope "" strings
        in line ++ printLine ("Keys", "Entries")  (line ++ scopesTables)

instance Pretty SymbolTable where
    pretty st =
        "Types Namespace\n" ++
        pretty (view types st) ++
        "Names Namespace\n" ++
        pretty (view names st)