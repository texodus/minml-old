------------------------------------------------------------------------------

-- Macros

------------------------------------------------------------------------------

module Forml.AST.Macro where

------------------------------------------------------------------------------

data Macro a = Token String [Macro a] | Arg String [Macro a] | Leaf a
    deriving (Eq, Ord, Show)


merge :: [Macro a] -> [Macro a] -> [Macro a]
merge newMacs oldMacs =
    [ merged | new <- newMacs, merged <- mergeWith' new oldMacs ]
    where
        mergeWith' (Token x xs) (Token y ys : zs) | x == y = 
            Token x (merge xs ys) : zs
        mergeWith' (Arg x xs) (Arg y ys : zs) | x == y =
            Arg x (merge xs ys) : zs
        mergeWith' (Arg x _) (Arg y _ : _) =
            error ("Arg naming conflict: (" ++ x ++ ") and (" ++ y ++ ")")
        mergeWith' (Leaf _) (Leaf _ : _) = 
            error "Notation duplicate"
        mergeWith' x (y : zs) = y : mergeWith' x zs
        mergeWith' x [] = [x]

------------------------------------------------------------------------------
