module Common.SymbolTable (SymbolTable,
                           TableEntry(..),
                           SymbolType(..),
                           typeToSymbolType,
                           emptySymbolTable,
                           query,
                           insert,
                           update,
                           openScope,
                           closeScope) where

import qualified Data.Map as M
import Common.AST (TypeF, Type (..))
import Common.Token (Identifier, ConstrIdentifier)
import Common.PrintAST (Pretty (pretty))

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

type SymbolTable = Context String TableEntry

emptySymbolTable :: SymbolTable
emptySymbolTable = emptyContext

data SymbolType = VarType Int | SymType (TypeF SymbolType)
    deriving (Show, Eq)

instance Pretty SymbolType where
    pretty (VarType i) = "@" ++ show i
    pretty (SymType t) = pretty t

typeToSymbolType :: Type b -> SymbolType
typeToSymbolType (Type tf _) = SymType (fmap typeToSymbolType tf)

data TableEntry
    = VarEntry SymbolType                                       -- Type of the variable
    | ArrayEntry SymbolType Int                                 -- Type of the entries, num of dimensions
    | FunEntry SymbolType [(Identifier, SymbolType)] SymbolType -- Type of the function, params, output type
    | TypeEntry [(ConstrIdentifier, [SymbolType])]              -- Constructors and arguements
    | ConstrEntry SymbolType [SymbolType] SymbolType            -- Type of constructor, params, output type
    | VarTypeEntry SymbolType                                   -- Index of type var, type constraint
        deriving Show