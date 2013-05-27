module Unit.Javascript.ExprSpec where

import Test.Hspec
import Test.HUnit

import Forml.AST
import Forml.Javascript
import Forml.RenderText

assertGenerate :: Expr -> Either Err String -> Assertion
assertGenerate a b = assertEqual "" gen b
    where
        gen = case generateJs a of
            Left x -> Left x
            Right x -> renderText x

spec :: Spec
spec = do

    describe "Forml.Javascript.Expr" $ do

        describe "generateJs" $ do

            it "should generate js for a trivial example " $ assertGenerate

                (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                  (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                    (VarExpr (LitVal (NumLit 2.0))))
                                           (VarExpr (LitVal (NumLit 2.0)))))
                         (VarExpr (LitVal (NumLit 4.0))))

                $ Right "var jmId_0;jmId_0 = ((2.0+2.0)==4.0);console.log(jmId_0);"

            it "should generate js for simple type errors" $ assertGenerate

                (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                  (VarExpr (LitVal (NumLit 4.0))))
                         (VarExpr (LitVal (StrLit "whoops"))))

                $ Right "var jmId_0;jmId_0 = (4.0+\"whoops\");console.log(jmId_0);"

            it "should generate js for let expressions" $ assertGenerate

                (LetExpr (Sym "x")
                         (VarExpr (LitVal (NumLit 1.0)))
                         (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                           (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                             (VarExpr (SymVal (Sym "x"))))
                                                    (VarExpr (LitVal (NumLit 1.0)))))
                                  (VarExpr (LitVal (NumLit 2.0)))))

                $ Right "var jmId_0;jmId_0 = (function(jmId_1){var x;x = 1.0;return ((x+1.0)==2.0);})();console.log(jmId_0);"

            it "should generate js for anonymous functions and application" $ assertGenerate

                (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                  (AppExpr (AbsExpr (Sym "x")
                                                    (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                                      (VarExpr (SymVal (Sym "x"))))
                                                             (VarExpr (LitVal (NumLit 4.0)))))
                                           (VarExpr (LitVal (NumLit 2.0)))))
                         (VarExpr (LitVal (NumLit 6.0))))

                $ Right "var jmId_0;jmId_0 = ((function(jmId_1){var x;x = jmId_1;return (x+4.0);})(2.0)==6.0);console.log(jmId_0);"

            it "should generate js for anonymous functions in let bindings" $ assertGenerate

                (LetExpr (Sym "g")
                         (AbsExpr (Sym "x")
                                  (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                    (VarExpr (SymVal (Sym "x"))))
                                           (VarExpr (LitVal (NumLit 4.0)))))
                         (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                           (AppExpr (VarExpr (SymVal (Sym "g")))
                                                    (VarExpr (LitVal (NumLit 2.0)))))
                                  (VarExpr (LitVal (NumLit 6.0)))))

                $ Right "var jmId_0;jmId_0 = (function(jmId_1){var g;g = (function(jmId_2){var x;x = jmId_2;return (x+4.0);});return (g(2.0)==6.0);})();console.log(jmId_0);"

            it "should generate js for match expressions" $ assertGenerate

                (LetExpr (Sym "fib")
                         (AbsExpr (Sym "n")
                                  (MatExpr (VarExpr (SymVal (Sym "n")))
                                           [ (ValPatt (LitVal (NumLit 0.0)), VarExpr (LitVal (NumLit 0.0)))
                                           , (ValPatt (LitVal (NumLit 1.0)), VarExpr (LitVal (NumLit 1.0)))
                                           , (ValPatt (SymVal (Sym "n")),    AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                                                              (AppExpr (VarExpr (SymVal (Sym "fib")))
                                                                                                       (AppExpr (AppExpr (VarExpr (SymVal (Sym "-")))
                                                                                                                         (VarExpr (SymVal (Sym "n"))))
                                                                                                                (VarExpr (LitVal (NumLit 1.0))))))
                                                                                     (AppExpr (VarExpr (SymVal (Sym "fib")))
                                                                                              (AppExpr (AppExpr (VarExpr (SymVal (Sym "-")))
                                                                                                                (VarExpr (SymVal (Sym "n"))))
                                                                                                       (VarExpr (LitVal (NumLit 2.0))))))]))
                         (AppExpr (VarExpr (SymVal (Sym "fib")))
                                  (VarExpr (LitVal (NumLit 7.0)))))

                $ Right "var jmId_0;jmId_0 = (function(jmId_1){var fib;fib = (function(jmId_2){var n;n = jmId_2;return (function(){var jmId_3;jmId_3 = n;if((0.0==jmId_3)){return 0.0;};return (function(){if((1.0==jmId_3)){return 1.0;};return (function(){var n;n = jmId_3;if(true){return (fib((n-1.0))+fib((n-2.0)));};return (function(){throw(\"Pattern Match Exhausted\");})();})();})();})();});return fib(7.0);})();console.log(jmId_0);"

            it "should generate js for match expressions with type errors" $ assertGenerate

                (MatExpr (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                           (VarExpr (LitVal (NumLit 3.0))))
                                  (VarExpr (LitVal (NumLit 3.0))))
                         [(ValPatt (LitVal (StrLit "gotcha")), VarExpr (SymVal (Sym "false")))])

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = (3.0+3.0);if((\"gotcha\"==jmId_1)){return false;};return (function(){throw(\"Pattern Match Exhausted\");})();})();console.log(jmId_0);"

            it "should generate js for user data types" $ assertGenerate

                (TypExpr (TypeSymP "Cons")
                         (TypeAbsP (TypeApp (TypeApp (TypeSym (TypeSymP "->"))
                                                     (TypeVar (TypeVarP "a")))
                                            (TypeApp (TypeApp (TypeSym (TypeSymP "->"))
                                                              (TypeApp (TypeSym (TypeSymP "List"))
                                                                       (TypeVar (TypeVarP "a"))))
                                                     (TypeApp (TypeSym (TypeSymP "List"))
                                                              (TypeVar (TypeVarP "a"))))))
                         (TypExpr (TypeSymP "Nil")
                                  (TypeAbsP (TypeApp (TypeSym (TypeSymP "List"))
                                                     (TypeVar (TypeVarP "a"))))
                                  (LetExpr (Sym "length")
                                           (AbsExpr (Sym "n")
                                                    (MatExpr (VarExpr (SymVal (Sym "n")))
                                                             [ (ValPatt (ConVal (TypeSym (TypeSymP "Nil"))), VarExpr (LitVal (NumLit 0.0)))
                                                             , (ConPatt (TypeSymP "Cons")
                                                                        [ ValPatt (SymVal (Sym "_"))
                                                                        , ValPatt (SymVal (Sym "xs")) ]
                                                             , AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                                                (VarExpr (LitVal (NumLit 1.0))))
                                                                       (AppExpr (VarExpr (SymVal (Sym "length")))
                                                                                (VarExpr (SymVal (Sym "xs")))))]))
                                           (AppExpr (VarExpr (SymVal (Sym "length")))
                                                    (AppExpr (AppExpr (VarExpr (ConVal (TypeSym (TypeSymP "Cons"))))
                                                                      (VarExpr (LitVal (NumLit 1.0))))
                                                             (AppExpr (AppExpr (VarExpr (ConVal (TypeSym (TypeSymP "Cons"))))
                                                                               (VarExpr (LitVal (NumLit 2.0))))
                                                                      (VarExpr (ConVal (TypeSym (TypeSymP "Nil"))))))))))

                $ Right "var jmId_0;jmId_0 = (function(){var Cons;var jmId_1;jmId_1 = (function($0,$1){this[0] = $0;this[1] = $1;});Cons = (function(jmId_2){return (function(jmId_3){return new jmId_1(jmId_2,jmId_3);});});Cons.__type__ = jmId_1;return (function(){var Nil;var jmId_4;jmId_4 = (function(){});Nil = new jmId_4();Nil.__type__ = jmId_4;return (function(jmId_5){var length;length = (function(jmId_6){var n;n = jmId_6;return (function(){var jmId_7;jmId_7 = n;if((jmId_7 instanceof Nil.__type__)){return 0.0;};return (function(){var _;_ = jmId_7[0];var xs;xs = jmId_7[1];if(((jmId_7 instanceof Cons.__type__)&&(true&&true))){return (1.0+length(xs));};return (function(){throw(\"Pattern Match Exhausted\");})();})();})();});return length(Cons(1.0)(Cons(2.0)(Nil)));})();})();})();console.log(jmId_0);"

            it "should generate js for with generic type errors" $ assertGenerate

                (TypExpr (TypeSymP "Just")
                         (TypeAbsP (TypeApp (TypeApp (TypeSym (TypeSymP "->"))
                                                     (TypeVar (TypeVarP "a")))
                                            (TypeApp (TypeSym (TypeSymP "Maybe"))
                                                     (TypeVar (TypeVarP "a")))))
                         (TypExpr (TypeSymP "None")
                                  (TypeAbsP (TypeApp (TypeSym (TypeSymP "Maybe"))
                                                     (TypeVar (TypeVarP "a"))))
                                  (MatExpr (AppExpr (VarExpr (ConVal (TypeSym (TypeSymP "Just"))))
                                                    (VarExpr (LitVal (NumLit 5.0))))
                                           [ (ConPatt (TypeSymP "Just") [ValPatt (LitVal (StrLit "blammo"))], VarExpr (SymVal (Sym "false")))])))

                $ Right "var jmId_0;jmId_0 = (function(){var Just;var jmId_1;jmId_1 = (function($0){this[0] = $0;});Just = (function(jmId_2){return new jmId_1(jmId_2);});Just.__type__ = jmId_1;return (function(){var None;var jmId_3;jmId_3 = (function(){});None = new jmId_3();None.__type__ = jmId_3;return (function(){var jmId_4;jmId_4 = Just(5.0);if(((jmId_4 instanceof Just.__type__)&&(\"blammo\"==jmId_4[0]))){return false;};return (function(){throw(\"Pattern Match Exhausted\");})();})();})();})();console.log(jmId_0);"

            it "should generate js for partial keywords" $ assertGenerate

                (LetExpr (Sym "letly")
                         (VarExpr (LitVal (NumLit 4.0)))
                         (LetExpr (Sym "func")
                                  (AbsExpr (Sym "x")
                                           (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                             (VarExpr (SymVal (Sym "x"))))
                                                    (VarExpr (LitVal (NumLit 2.0)))))
                                  (AppExpr (VarExpr (SymVal (Sym "func")))
                                           (VarExpr (SymVal (Sym "letly"))))))

                $ Right "var jmId_0;jmId_0 = (function(jmId_1){var letly;letly = 4.0;return (function(jmId_2){var func;func = (function(jmId_3){var x;x = jmId_3;return (x+2.0);});return func(letly);})();})();console.log(jmId_0);"

            it "should generate js for binding tricks" $ assertGenerate

                (LetExpr (Sym "x")
                         (VarExpr (LitVal (NumLit 3.0)))
                         (LetExpr (Sym "f")
                                  (AbsExpr (Sym "y")
                                           (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                             (VarExpr (SymVal (Sym "y"))))
                                                    (VarExpr (SymVal (Sym "x")))))
                                  (LetExpr (Sym "x")
                                           (VarExpr (LitVal (NumLit 2.0)))
                                           (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                                             (AppExpr (VarExpr (SymVal (Sym "f")))
                                                                      (VarExpr (LitVal (NumLit 3.0)))))
                                                    (VarExpr (LitVal (NumLit 6.0)))))))

                $ Right "var jmId_0;jmId_0 = (function(jmId_1){var x;x = 3.0;return (function(jmId_2){var f;f = (function(jmId_3){var y;y = jmId_3;return (y+x);});return (function(jmId_4){var x;x = 2.0;return (f(3.0)==6.0);})();})();})();console.log(jmId_0);"

------------------------------------------------------------------------------