module Unit.Source where

import Control.Monad
import Control.Applicative
import Data.Char
import Test.Hspec
import Test.HUnit

import Minml.Compile
import Minml.Prelude
import Minml.TypeCheck
import Minml.Javascript
import Minml.RenderText

genData :: IO ()
genData = case mapM parse' (sources `zip` [0 ..]) of
    Left x-> error (show x)
    Right asts -> do
        writeFile "src/hs/test/Unit/Asts.hs" (header "Asts" ++ show asts)
        writeFile "src/hs/test/Unit/Types.hs" (header "Types" ++ show (fmap typeCheck asts))
        writeFile "src/hs/test/Unit/Scripts.hs"
            (header "Scripts" ++ show (fmap (join . fmap renderText . generateJs) asts))

        assertEqual "GENERATED DATA! rerun" True False

    where
        parse' (a, n) = head . tail . fst <$> foldM parse ([], emptyState) [("Prelude", prelude), ("Test Case " ++ show n, a)]
        header name = "module Unit." ++ name ++ " where\n\nimport Data.Map\nimport Minml.AST\n"++fmap toLower name++" = "

sources :: [String]
sources = [

    "   2 + 2.0 == 4.0   ",

    "   4 + \"whoops\"   ",

    "   let x = 1.0;     \
    \   x + 1.0 == 2.0   ",

    "   x   ",

    "   (fun x -> x + 4) 2 == 6   ",
 
    "   let g = fun x -> x + 4;   \
    \   g 2 == 6                  ",

    "   let fib = fun n ->                      \
    \       match n with                        \
    \       0 -> 0;                             \
    \       1 -> 1;                             \
    \       n -> fib (n - 1) + fib (n - 2);;    \
    \   fib 7                                   ",

    "   x   ",

    "   let fib = fun n ->                      \n\
    \       match n with                        \n\
    \       0 -> 0;                             \n\
    \       1 -> 1;                             \n\
    \       n -> fib (n - 1) + fib (n - 2);     \n\
    \   fib 7                                   \n",

    "   match 3 + 3 with \"gotcha\" -> false;   ",

    "   data Cons: a -> List a -> List a;        \
    \   data Nil: List a;                        \
    \                                            \
    \   let length = fun n ->                    \
    \       match n with                         \
    \           Nil -> 0;                        \
    \           (Cons _ xs) -> 1 + length xs;;   \
    \                                            \
    \   length (Cons 1 (Cons 2 Nil))             ",

    "   data Just: a -> Maybe a;          \
    \   data None: Maybe a;               \
    \                                     \
    \   match Just 5 with                 \
    \       (Just \"blammo\") -> false;   ",

    "   let letly = 4;                    \
    \   (let func = fun x -> x + ((2));   \
    \   func letly)                       ",

    "   let x = 3;                 \
    \   let f = fun y -> y + x;    \
    \   let x = 2;                 \
    \   f 3 == 6                   ",

    "   let x = {x: 4; y: 6}   \n\  
    \   x                      \n",

    "   let x = {x: 4; y: 6}   \n\
    \   let y = {x: 4; z: 3}   \n\
    \   x == y                 \n",

    "   x   ",

    "   f       \n\
    \       4     ",

    "   let letly = 4                    \n\
    \   let func = fun x -> x + ((2))    \n\
    \   func letly                       \n",

    "   let letly =       \n\
    \       let func =    \n\
    \           5         \n\
    \       4             \n\
    \   func 5            \n",

    "   let fib = fun n ->                     \n\
    \       match n with                       \n\
    \       0 -> 0;                            \n\
    \       1 -> 1;                            \n\
    \       n -> fib (n - 1) + fib (n - 2);    \n\
    \   fib 7                                  \n",

    "   let fib = fun n ->                     \n\
    \       match n with                       \n\
    \       0 -> 0                             \n\
    \       1 -> 1                             \n\
    \       n -> fib (n - 1) + fib (n - 2)     \n\
    \   fib 7                                  \n",

    "   data Just: a -> Maybe a           \n\
    \   data None: Maybe a                \n\
    \                                     \n\
    \   match Just 5 with                 \n\
    \       (Just \"blammo\") -> false    \n",

    "   letly = 4                        \n\
    \   func = fun x -> x + ((2))        \n\
    \   func letly                       \n",

    "   fib = fun n ->                         \n\
    \       match n with                       \n\
    \       0 -> 0                             \n\
    \       1 -> 1                             \n\
    \       n -> fib (n - 1) + fib (n - 2)     \n\
    \   fib 7                                  \n",

    "   let g = fun x = x + 4    \n\
    \   g 2 == 6                 \n",

    "   letly = 4                   \n\
    \   func = fun x = x + ((2))    \n\
    \   func letly                  \n",

    "   fib = fun n =                          \n\
    \       match n with                       \n\
    \       0 -> 0                             \n\
    \       1 -> 1                             \n\
    \       n -> fib (n - 1) + fib (n - 2)     \n\
    \   fib 7                                  \n",

    "   fib = fun n =                          \n\
    \       match n with                       \n\
    \       0 = 0                              \n\
    \       1 = 1                              \n\
    \       n = fib (n - 1) + fib (n - 2)      \n\
    \   fib 7                                  \n",
 
    "   let g = λ x = x + 4      \n\
    \   g 2 == 6                 \n",


    "   letly = 4                   \n\
    \   func = λ x = x + ((2))      \n\
    \   func letly                  \n",

    "   fib = λ n =                            \n\
    \       match n with                       \n\
    \       0 -> 0                             \n\
    \       1 -> 1                             \n\
    \       n -> fib (n - 1) + fib (n - 2)     \n\
    \   fib 7                                  \n",

    "   Cons: a -> List a -> List a            \n\
    \   Nil: List a                            \n\
    \                                          \n\
    \   length n =                             \n\
    \       match n with                       \n\
    \           Nil -> 0                       \n\
    \           (Cons _ xs) -> 1 + length xs   \n\
    \                                          \n\
    \   length (Cons 1 (Cons 2 Nil))           \n",

    "   f = {x: 3}       \n\
    \   match f with     \n\
    \       {x: x} = x   \n\
    \       _ = 0        \n" ]


toSpec title f =

    describe (title ++ " correct cases (from template)") $ do
        
        it "should parse a trivial example " $ f 0

        it "should parse simple type errors" $ f 1

        it "should parse let expressions" $ f 2

        it "should parse anonymous functions and application" $ f 4

        it "should parse anonymous functions in let bindings" $ f 5

        it "should parse match expressions" $ f 6

        it "should parse match mixed semicolon with newlines" $ f 8

        it "should parse match expressions with type errors" $ f 9

        it "should parse user data types" $ f 10

        it "should parse with generic type errors" $f 11

        it "should parse partial keywords" $ f 12

        it "should parse binding tricks" $ f 13

        it "should parse records" $ f 14

        it "should parse record Equality" $ f 15

        describe "function application whitespace" $

            it "should parse function application on separate lines indented" $ f 17

        describe "whitespace let" $ do

            it "should parse whitespace statement sep" $ f 18

            it "should parse whitespace statement sep with proper scope" $ f 19

            it "should parse match expressions with mixed whitespace" $ f 20

        describe "whitespace match" $

            it "should parse match expressions" $ f 21

        describe "whitespace data" $

            it "should parse with generic type errors" $ f 22

        describe "without `let` keyword" $ do

            it "should parse whitespace statement sep" $ f 23

            it "should parse match expressions" $ f 24

        describe "without `=` reserved operator" $ do

            it "should parse anonymous functions in let bindings" $ f 25

            it "should parse whitespace statement sep" $ f 26

            it "should parse match expressions with -> cases" $ f 27

            it "should parse match expressions with = cases" $ f 28

        describe "with `λ` reserved operator" $ do

            it "should parse anonymous functions in let bindings" $ f 29

            it "should parse whitespace statement sep" $ f 30

            it "should parse match expressions" $ f 31

        describe "without `data` keyword" $

            it "should parse user data types" $ f 32

        describe "Records" $

            it "should compile & run basic record patterns" $ f 33

------------------------------------------------------------------------------
