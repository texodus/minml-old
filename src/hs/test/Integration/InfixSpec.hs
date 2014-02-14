{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Integration.InfixSpec where

import Test.Hspec
import Text.InterpolatedString.Perl6

import Unit.Source


spec :: Spec
spec =

    describe "Infix expressions" $ do

        sample "Trivial" $ [q|

            `(a) +++ (b)` = a + b
            2 +++ 2

        |]

        sample "Demonstrating left associativity" $ [q|

            `(a) +++ (b)` = a + b
            2 +++ 2 +++ 2 +++ 2

        |]

        pendingSample "Demonstrating mixed associativity" $ [q|

            `(a) +++ (b)` = a + b
            `(a) -+- (b)` = a - b
            2 +++ 2 -+- 3 +++ 2

        |]

        sample "Non-ambiguous (no associativity)" $ [q|

            `(a) <- (b) -< (c)` = a + b + c
            2 <- 2 <- 2 -< 2 -< 2

        |]

