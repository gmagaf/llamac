module Common.SymbolTable (SymbolTable,
                           TableEntry(..),
                           emptySymbolTable,
                           query,
                           insert,
                           openScope,
                           closeScope) where

import qualified Data.Map as M
import Common.AST (Type)
import Common.Token (Identifier, ConstrIdentifier)

-- This module contains the defintion of the Symbol table for the compiler

newtype Context k e = Context [M.Map k e]
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

openScope :: Context k e -> Context k e
openScope (Context scopes) = Context $ M.empty:scopes

closeScope :: Context k e -> Context k e
closeScope (Context scopes) = Context s where
    s = if null scopes then [] else tail scopes

type SymbolTable = Context String TableEntry

emptySymbolTable :: SymbolTable
emptySymbolTable = emptyContext

data TableEntry
    = VarEntry (Type ())                                    -- Type of the variable
    | ArrayEntry (Type ()) Int                              -- Type of the entries, num of dimensions
    | FunEntry (Type ()) [(Identifier, Type ())] (Type ())  -- Type of the function, params, output type
    | TypeEntry [(ConstrIdentifier, [Type ()])]             -- Constructors and arguements
    | ConstrEntry (Type ()) [Type ()] (Type ())             -- Type of constructor, params, output type
        deriving Show