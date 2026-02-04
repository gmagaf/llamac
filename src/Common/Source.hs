module Common.Source (Source(..), printSource) where

-- A data type to hold the source of the input code

data Source = ReplIn Int
            | FileIn String
    deriving (Show, Eq, Ord)

printSource :: Source -> String
printSource (ReplIn l) = "<interactive>:" ++ show l
printSource (FileIn f) = f
