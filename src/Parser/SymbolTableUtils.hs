module Parser.SymbolTableUtils (module Parser.SymbolTableUtils) where

import Control.Lens (use, (%=), (<~), (.~))
import qualified LLVM.AST as L (Operand)
import qualified LLVM.AST.Type as L (Type)

import Common.SymbolTable (SymbolTable, NameSpace, TypeSpace, names, types, closeScope, openScope, Context, FullTableEntry, TableEntry, TypeTableEntry, partialQuery, partialInsert, partialUpdate, basicInfo, mkBasicEntry, query, insert)
import Parser.ParserM (Parser)
import Parser.ParserState (symbols)

-- Handle the various contexts (namespace, typespace)
getSymbols :: Parser SymbolTable
getSymbols = use symbols

getNames :: Parser NameSpace
getNames = use (symbols . names)

getTypes :: Parser TypeSpace
getTypes = use (symbols . types)

overNames :: (NameSpace -> NameSpace) -> Parser ()
overNames f = do
    symbols . names %= f

overNamesM :: (NameSpace -> Parser NameSpace) -> Parser ()
overNamesM f = do
    symbols . names <~ (getNames >>= f)

overTypes :: (TypeSpace -> TypeSpace) -> Parser ()
overTypes f = do
    symbols . types %= f

-- Query/Insert symbol table utiles
queryName :: String -> Parser (Maybe (FullTableEntry TableEntry L.Operand))
queryName k = query k <$> getNames

queryType :: String -> Parser (Maybe (FullTableEntry TypeTableEntry L.Type))
queryType k = query k <$> getTypes

insertName :: String -> FullTableEntry TableEntry L.Operand -> Parser ()
insertName k entry = overNames $ insert k entry

insertType :: String -> FullTableEntry TypeTableEntry L.Type -> Parser ()
insertType k entry = overTypes $ insert k entry

-- Partial Query/Insert symbol table utiles
queryP :: Ord k => k -> Context k (FullTableEntry e g) -> Maybe e
queryP = partialQuery basicInfo

insertP :: Ord k => k -> e -> Context k (FullTableEntry e g) -> Context k (FullTableEntry e g)
insertP = partialInsert mkBasicEntry

updateP :: Ord k => k -> e -> Context k (FullTableEntry e g) -> Context k (FullTableEntry e g)
updateP = partialUpdate (basicInfo .~)

queryNameP :: String -> Parser (Maybe TableEntry)
queryNameP k = queryP k <$> getNames

queryTypeP :: String -> Parser (Maybe TypeTableEntry)
queryTypeP k = queryP k <$> getTypes

insertNameP :: String -> TableEntry -> Parser ()
insertNameP k entry = overNames $ insertP k entry

insertTypeP :: String -> TypeTableEntry -> Parser ()
insertTypeP k entry = overTypes $ insertP k entry

-- Handle scopes
openScopeInTypes :: Parser ()
openScopeInTypes = overTypes openScope

closeScopeInTypes :: Parser ()
closeScopeInTypes = overTypes closeScope

openScopeInNames :: Parser ()
openScopeInNames = overNames openScope

closeScopeInNames :: Parser ()
closeScopeInNames = overNames closeScope
