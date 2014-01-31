{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Regression.ParseSpec where

import Test.Hspec
import Text.InterpolatedString.Perl6

import Unit.Source


spec :: Spec
spec =

    describe "Parsing regressions" $

        sample "Funciton definition and application with 2 arguments" $ [q|
          
            f a b = a + b
            f 1 2

        |]
 
