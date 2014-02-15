{-# LANGUAGE QuasiQuotes #-}

module Unit.AssSpec where

import Test.Hspec

import Unit.Source

import Text.InterpolatedString.Perl6


spec :: Spec
spec =

    describe "Assumptions (from template)" $ do
        
        sample "a trivial example"

            "   x :: Double \n\
            \   x = 1       \n\
            \   x + 2       \n"

        sample "a trivial example, with a type error"

            "   x :: Double    \n\
            \   x = \"string\" \n\
            \   x              \n"

        sample "A generic function"

            "   add :: Double -> Double -> Double \n\
            \   add x y = ``x + y``               \n\
            \   add 2 2                           \n"

        sample "A generic function, with a type error"

            "   add :: Double -> String -> Double \n\
            \   add x y = ``x + y``               \n\
            \   add 2 2                           \n"

        sample "A generic macro" [q|

            `(x) @ (y)` =
                x :: Double
                y :: Double
                ``x + y``

            4 @ 3
            
        |]

        sample "A generic macro, with a type error" [q|

            `(x) @ (y)` =
                x :: Double
                y :: Double
                ``x + y``

            4 @ "string"
            
        |]

        sample "A generic macro, with a return annotation" [q|

            `(x) @ (y)` =
                x :: Double
                y :: Double
                ``x + y`` :: Double

            4 @ 4 @ 4
            
        |]

        sample "A generic macro, with a return annotation and a type error" [q|

            `(x) @ (y)` =
                x :: Double
                y :: Double
                ``x + y`` :: String

            4 @ 4 @ 4
            
        |]

        sample "A generic macro, with a return annotation and a type error in an inline argument" [q|

            add :: Bool -> Double
            add x = x

            `(x) @ (y)` =
                x :: Double
                y :: Double
                ``x + y`` :: String

            add "test" @ 4 @ 4
            
        |]

------------------------------------------------------------------------------
