--------------------------------------------------------------------------------

-- | Types

--   This ensures that any `Type a` will be consistent, and allows us to
--   generate 2 types of `Type a` which share most of their structure.

--   `TypeGen`s are a special `TypeVar` which we use to mark places where
--   we want a type to be polymorphic.

--------------------------------------------------------------------------------

{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleInstances  #-}

module Minml.AST.Type (
    module Minml.AST.Kind,
    TypeSym( .. ),
    TypeAbs( .. ),
    TypeVar( .. ),
    Type( .. )
) where

import Control.Applicative
import GHC.Read
import Data.Ix
import Text.Read.Lex
import Text.ParserCombinators.ReadPrec


import Minml.AST.Kind
import Minml.AST.Record
import Minml.Utils

------------------------------------------------------------------------------

-- | Type Variables

data TypeVar a where
    TypeVarP :: String -> TypeVar ()
    TypeVarT :: Kind -> String -> TypeVar Kind

instance (Fmt a) => Fmt (TypeVar a) where
    fmt (TypeVarP s) = s
    fmt (TypeVarT _ s) = s

deriving instance (Ord a)  => Ord  (TypeVar a)
deriving instance (Eq a)   => Eq   (TypeVar a)
deriving instance (Show a) => Show (TypeVar a)

-- | Type Symbols

data TypeSym a where
    TypeSymP :: String -> TypeSym ()
    TypeSymT :: Kind -> String -> TypeSym Kind

-- | Polymorphic Types, aka Type Abstractions

data TypeAbs a where
    TypeAbsP :: Type () -> TypeAbs ()
    TypeAbsT :: [Kind] -> Type Kind -> TypeAbs Kind

-- | Union type for all types, including `TypeGen` which is only used
--   in typechecking to represent generalized types.

data Type a where
    TypeSym :: TypeSym a -> Type a
    TypeVar :: TypeVar a -> Type a
    TypeApp :: Type a -> Type a -> Type a
    TypeRec :: Record (Type a) -> Type a

    TypeGen :: Int -> Type Kind

instance (Fmt a) => Fmt (TypeSym a) where
    fmt (TypeSymP s) = s
    fmt (TypeSymT _ s) = s

instance (Fmt a) => Fmt (TypeAbs a) where
    fmt (TypeAbsP t)   = "forall ?. " ++ fmt t
    fmt (TypeAbsT k s) = "forall " ++ varNames k ++ ". " ++ fmt s
        where
            varNames = unwords . toTypGen
            toTypGen = map (fmt . TypeGen) . range . (0,) . length

instance (Fmt a) => Fmt (Type a) where
    fmt (TypeSym s) = fmt s
    fmt (TypeVar v) = fmt v
    fmt (TypeApp a b) = "(" ++ fmt a ++ " " ++ fmt b ++ ")"
    fmt (TypeGen i) = "<<" ++ show i ++ ">>"
    fmt (TypeRec m) = fmt m

deriving instance (Ord a) => Ord (Type a)
deriving instance (Ord a) => Ord (TypeSym a)
deriving instance (Ord a) => Ord (TypeAbs a)

deriving instance (Eq a) => Eq (Type a)
deriving instance (Eq a) => Eq (TypeSym a)
deriving instance (Eq a) => Eq (TypeAbs a)

deriving instance (Show a) => Show (Type a)
deriving instance (Show a) => Show (TypeSym a)
deriving instance (Show a) => Show (TypeAbs a)

instance Read (TypeVar ()) where
    readPrec = parens $ prec 10 $ do
        Ident "TypeVarP" <- lexP
        arg1 <- step readPrec
        return $ TypeVarP arg1  

instance Read (TypeVar Kind) where
   readPrec = parens $ prec 10 $ do
        Ident "TypeVarT" <- lexP
        arg1 <- step readPrec
        arg2 <- step readPrec
        return $ TypeVarT arg1 arg2

instance Read (TypeSym ()) where
     readPrec = parens $ prec 10 $ do
        Ident "TypeSymP" <- lexP
        arg1 <- step readPrec
        return $ TypeSymP arg1  
 
instance Read (TypeSym Kind) where
   readPrec = parens $ prec 10 $ do
        Ident "TypeSymT" <- lexP
        arg1 <- step readPrec
        arg2 <- step readPrec
        return $ TypeSymT arg1 arg2

instance Read (TypeAbs ()) where
    readPrec = parens $ prec 10 $ do
        Ident "TypeAbsP" <- lexP
        arg1 <- step readPrec
        return $ TypeAbsP arg1  
 
instance Read (TypeAbs Kind) where
   readPrec = parens $ prec 10 $ do
        Ident "TypeAbsT" <- lexP
        arg1 <- step readPrec
        arg2 <- step readPrec
        return $ TypeAbsT arg1 arg2

instance Read (Type ()) where
    readPrec =  
        typeSym +++ typeVar +++ typeApp +++ typeRec

        where
            typeSym = parens $ prec 10 $ do
                Ident "TypeSym" <- lexP
                TypeSym <$> step readPrec

            typeVar = parens $ prec 10 $ do
                Ident "TypeVar" <- lexP
                TypeVar <$> step readPrec

            typeApp = parens $ prec 10 $ do
                Ident "TypeApp" <- lexP
                arg1 <- step readPrec
                arg2 <- step readPrec
                return $ TypeApp arg1 arg2

            typeRec = parens $ prec 10 $ do
                Ident "TypeRec" <- lexP
                TypeRec <$> step readPrec


instance Read (Type Kind) where
    readPrec = parens $ prec 10 $ 
        typeSym +++ typeVar +++ typeApp +++ typeRec +++ typeGen

        where
            typeSym = parens $ prec 10 $ do
                Ident "TypeSym" <- lexP
                TypeSym <$> step readPrec

            typeVar = parens $ prec 10 $ do
                Ident "TypeVar" <- lexP
                TypeVar <$> step readPrec

            typeApp = parens $ prec 10 $ do
                Ident "TypeApp" <- lexP
                arg1 <- step readPrec
                arg2 <- step readPrec
                return $ TypeApp arg1 arg2

            typeRec = parens $ prec 10 $ do
                Ident "TypeRec" <- lexP
                TypeRec <$> step readPrec

            typeGen = parens $ prec 10 $ do
                Ident "TypeGen" <- lexP
                TypeGen <$> step readPrec

--------------------------------------------------------------------------------
