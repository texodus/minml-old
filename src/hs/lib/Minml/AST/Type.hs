--------------------------------------------------------------------------------

-- | Types

--   This ensures that any `Type a` will be consistent, and allows us to
--   generate 2 types of `Type a` which share most of their structure.

--   `TypeGen`s are a special `TypeVar` which we use to mark places where
--   we want a type to be polymorphic.

--   The `Serialize` and `Read` instances cannot be derived becaues they are
--   GADTs.  Lame.

--------------------------------------------------------------------------------

{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}

module Minml.AST.Type (
    module Minml.AST.Kind,
    TypeSym( .. ),
    TypeAbs( .. ),
    TypeVar( .. ),
    Type( .. )
) where

import Control.Applicative
import Data.Ix
import GHC.Generics
import GHC.Read
import Text.Read.Lex

import qualified Data.Serialize                  as S
import qualified Text.ParserCombinators.ReadPrec as P

import Minml.AST.Kind
import Minml.AST.Record
import Minml.Utils

--------------------------------------------------------------------------------

-- | Type Variables

data TypeVar a where
    TypeVarP :: String -> TypeVar ()
    TypeVarT :: Kind -> String -> TypeVar Kind

--instance Generic (UserTree a) where
--  -- Representation type
--  type Rep (UserTree a) =
--    M1 D D1UserTree (
--          M1 C C1_0UserTree (
--                M1 S NoSelector (K1 P a)
--            :*: M1 S NoSelector (K1 R (UserTree a))
--            :*: M1 S NoSelector (K1 R (UserTree a)))
--      :+: M1 C C1_1UserTree U1)

--  -- Conversion functions
--  from (Node x l r) = M1 (L1 (M1 (M1 (K1 x) :*: M1 (K1 l) :*: M1 (K1 r))))
--  from Leaf         = M1 (R1 (M1 U1))
--  to (M1 (L1 (M1 (M1 (K1 x) :*: M1 (K1 l) :*: M1 (K1 r))))) = Node x l r
--  to (M1 (R1 (M1 U1)))                                      = Leaf

instance Generic (TypeVar ()) where

    type Rep (TypeVar ()) =
        M1 D D1TypeVar (M1 C C1_0TypeVarP (M1 S NoSelector (Rec0 String)))

    from (TypeVarP x) = M1 (M1 (M1 (K1 x)))
    to (M1 (M1 (M1 (K1 x)))) = TypeVarP x

data D1TypeVar
data C1_0TypeVarP

instance Datatype D1TypeSym where
    datatypeName _ = "TypeSym ()"
    moduleName _   = "Minml.AST.Type"

instance Constructor C1_0TypeSymP where
    conName _ = "TypeSymP"

instance Generic (Type ()) where

    type Rep (Type ()) =
        M1 D D1TypeVar (M1 C C1_0TypeVarP (M1 S NoSelector (Rec0 String)))

    from = undefined
    to = undefined

data D1Type
data C1_0Type

instance Datatype D1Type where
    datatypeName _ = "TypeSym ()"
    moduleName _   = "Minml.AST.Type"

instance Constructor C1_0Type where
    conName _ = "TypeSymP"

instance Generic (TypeSym ()) where

    type Rep (TypeSym ()) =
        M1 D D1TypeSym (M1 C C1_0TypeSymP (M1 S NoSelector (Rec0 String)))

    from (TypeSymP x) = M1 (M1 (M1 (K1 x)))
    to (M1 (M1 (M1 (K1 x)))) = TypeSymP x

data D1TypeSym
data C1_0TypeSymP

instance Datatype D1TypeVar where
    datatypeName _ = "TypeVar ()"
    moduleName _   = "Minml.AST.Type"

instance Constructor C1_0TypeVarP where
    conName _ = "TypeVarP"



instance Generic (TypeAbs ()) where

    type Rep (TypeAbs ()) =
        M1 D D1TypeAbs (M1 C C1_0TypeAbsP (M1 S NoSelector (Rec0 (Type ()))))

    from (TypeAbsP x) = M1 (M1 (M1 (K1 x)))
    to (M1 (M1 (M1 (K1 x)))) = TypeAbsP x

data D1TypeAbs
data C1_0TypeAbsP

instance Datatype D1TypeAbs where
    datatypeName _ = "TypeAbs ()"
    moduleName _   = "Minml.AST.Type"

instance Constructor C1_0TypeAbsP where
    conName _ = "TypeAbsP"

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

-- Standalone instances

deriving instance (Ord a) => Ord (Type a)
deriving instance (Ord a) => Ord (TypeSym a)
deriving instance (Ord a) => Ord (TypeAbs a)

deriving instance (Eq a) => Eq (Type a)
deriving instance (Eq a) => Eq (TypeSym a)
deriving instance (Eq a) => Eq (TypeAbs a)

deriving instance (Show a) => Show (Type a)
deriving instance (Show a) => Show (TypeSym a)
deriving instance (Show a) => Show (TypeAbs a)

deriving instance (Ord a)  => Ord  (TypeVar a)
deriving instance (Eq a)   => Eq   (TypeVar a)
deriving instance (Show a) => Show (TypeVar a)

-- Fmt

instance (Fmt a) => Fmt (TypeVar a) where
    fmt (TypeVarP s) = s
    fmt (TypeVarT _ s) = s

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

--------------------------------------------------------------------------------

-- Serialize

instance S.Serialize (TypeVar ()) where
    get = TypeVarP <$> S.get
    put (TypeVarP str) = S.put str

instance S.Serialize (TypeVar Kind) where
    get = TypeVarT <$> S.get <*> S.get
    put (TypeVarT str kind) = S.put str >> S.put kind

instance S.Serialize (TypeSym ()) where
    get = TypeSymP <$> S.get
    put (TypeSymP str) = S.put str

instance S.Serialize (TypeSym Kind) where
    get = TypeSymT <$> S.get <*> S.get
    put (TypeSymT str kind) = S.put str >> S.put kind

instance S.Serialize (TypeAbs ()) where
    get = TypeAbsP <$> S.get
    put (TypeAbsP str) = S.put str

instance S.Serialize (TypeAbs Kind) where
    get = TypeAbsT <$> S.get <*> S.get
    put (TypeAbsT str kind) = S.put str >> S.put kind

instance S.Serialize (Type ()) where

    get = do
        name <- S.get
        case name of
            "TypeSym" -> TypeSym <$> S.get
            "TypeVar" -> TypeVar <$> S.get
            "TypeApp" -> TypeApp <$> S.get <*> S.get
            "TypeRec" -> TypeRec <$> S.get
            _ -> error "Bad serialization"

    put (TypeSym s)   = S.put "TypeSym" >> S.put s
    put (TypeVar s)   = S.put "TypeVar" >> S.put s
    put (TypeApp s x) = S.put "TypeApp" >> S.put s >> S.put x
    put (TypeRec s)   = S.put "TypeRec" >> S.put s

instance S.Serialize (Type Kind) where

    get = do
        name <- S.get
        case name of
            "TypeSym" -> TypeSym <$> S.get
            "TypeVar" -> TypeVar <$> S.get
            "TypeApp" -> TypeApp <$> S.get <*> S.get
            "TypeRec" -> TypeRec <$> S.get
            "TypeGen" -> TypeGen <$> S.get

    put (TypeSym s)   = S.put "TypeSym" >> S.put s
    put (TypeVar s)   = S.put "TypeVar" >> S.put s
    put (TypeApp s x) = S.put "TypeApp" >> S.put s >> S.put x
    put (TypeRec s)   = S.put "TypeRec" >> S.put s
    put (TypeGen s)   = S.put "TypeGen" >> S.put s

-- Read

instance Read (TypeVar ()) where
    readPrec = parens $ P.prec 10 $ do
        Ident "TypeVarP" <- lexP
        arg1 <- P.step readPrec
        return $ TypeVarP arg1

instance Read (TypeVar Kind) where
   readPrec = parens $ P.prec 10 $ do
        Ident "TypeVarT" <- lexP
        arg1 <- P.step readPrec
        arg2 <- P.step readPrec
        return $ TypeVarT arg1 arg2

instance Read (TypeSym ()) where
     readPrec = parens $ P.prec 10 $ do
        Ident "TypeSymP" <- lexP
        arg1 <- P.step readPrec
        return $ TypeSymP arg1

instance Read (TypeSym Kind) where
   readPrec = parens $ P.prec 10 $ do
        Ident "TypeSymT" <- lexP
        arg1 <- P.step readPrec
        arg2 <- P.step readPrec
        return $ TypeSymT arg1 arg2

instance Read (TypeAbs ()) where
    readPrec = parens $ P.prec 10 $ do
        Ident "TypeAbsP" <- lexP
        arg1 <- P.step readPrec
        return $ TypeAbsP arg1

instance Read (TypeAbs Kind) where
   readPrec = parens $ P.prec 10 $ do
        Ident "TypeAbsT" <- lexP
        arg1 <- P.step readPrec
        arg2 <- P.step readPrec
        return $ TypeAbsT arg1 arg2

instance Read (Type ()) where
    readPrec =
        typeSym P.+++ typeVar P.+++ typeApp P.+++ typeRec

        where
            typeSym = parens $ P.prec 10 $ do
                Ident "TypeSym" <- lexP
                TypeSym <$> P.step readPrec

            typeVar = parens $ P.prec 10 $ do
                Ident "TypeVar" <- lexP
                TypeVar <$> P.step readPrec

            typeApp = parens $ P.prec 10 $ do
                Ident "TypeApp" <- lexP
                arg1 <- P.step readPrec
                arg2 <- P.step readPrec
                return $ TypeApp arg1 arg2

            typeRec = parens $ P.prec 10 $ do
                Ident "TypeRec" <- lexP
                TypeRec <$> P.step readPrec


instance Read (Type Kind) where
    readPrec = parens $ P.prec 10 $
        typeSym P.+++ typeVar P.+++ typeApp P.+++ typeRec P.+++ typeGen

        where
            typeSym = parens $ P.prec 10 $ do
                Ident "TypeSym" <- lexP
                TypeSym <$> P.step readPrec

            typeVar = parens $ P.prec 10 $ do
                Ident "TypeVar" <- lexP
                TypeVar <$> P.step readPrec

            typeApp = parens $ P.prec 10 $ do
                Ident "TypeApp" <- lexP
                arg1 <- P.step readPrec
                arg2 <- P.step readPrec
                return $ TypeApp arg1 arg2

            typeRec = parens $ P.prec 10 $ do
                Ident "TypeRec" <- lexP
                TypeRec <$> P.step readPrec

            typeGen = parens $ P.prec 10 $ do
                Ident "TypeGen" <- lexP
                TypeGen <$> P.step readPrec

--------------------------------------------------------------------------------
