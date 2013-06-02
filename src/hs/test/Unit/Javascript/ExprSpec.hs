module Unit.Javascript.ExprSpec where

import qualified Data.Map as M
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

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = 1.0;return ((jmId_1+1.0)==2.0);})();console.log(jmId_0);"

            it "should generate js for anonymous functions and application" $ assertGenerate

                (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                  (AppExpr (AbsExpr (Sym "x")
                                                    (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                                      (VarExpr (SymVal (Sym "x"))))
                                                             (VarExpr (LitVal (NumLit 4.0)))))
                                           (VarExpr (LitVal (NumLit 2.0)))))
                         (VarExpr (LitVal (NumLit 6.0))))

                $ Right "var jmId_0;jmId_0 = ((function(jmId_1){return (jmId_1+4.0);})(2.0)==6.0);console.log(jmId_0);"

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

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = (function(jmId_3){return (jmId_3+4.0);});return (jmId_1(2.0)==6.0);})();console.log(jmId_0);"

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

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = (function(jmId_3){var jmId_5;jmId_5 = jmId_3;if((0.0==jmId_5)){return 0.0;};if((1.0==jmId_5)){return 1.0;};return (jmId_1((jmId_5-1.0))+jmId_1((jmId_5-2.0)));});return jmId_1(7.0);})();console.log(jmId_0);"

            it "should generate js for match expressions with type errors" $ assertGenerate

                (MatExpr (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                           (VarExpr (LitVal (NumLit 3.0))))
                                  (VarExpr (LitVal (NumLit 3.0))))
                         [(ValPatt (LitVal (StrLit "gotcha")), VarExpr (SymVal (Sym "false")))])

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = (3.0+3.0);if((\"gotcha\"==jmId_1)){return false;};throw(\"Pattern Match Exhausted\");})();console.log(jmId_0);"

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

                $ Right "var jmId_0;jmId_0 = (function(){var Cons;var jmId_1;jmId_1 = (function($0,$1){this[0] = $0;this[1] = $1;});Cons = (function(jmId_2){return (function(jmId_3){return new jmId_1(jmId_2,jmId_3);});});Cons.__type__ = jmId_1;var Nil;var jmId_4;jmId_4 = (function(){});Nil = new jmId_4();Nil.__type__ = jmId_4;var jmId_5;jmId_5 = (function(jmId_7){var jmId_9;jmId_9 = jmId_7;if((jmId_9 instanceof Nil.__type__)){return 0.0;};if((jmId_9 instanceof Cons.__type__)){return (1.0+jmId_5(jmId_9[1]));};throw(\"Pattern Match Exhausted\");});return jmId_5(Cons(1.0)(Cons(2.0)(Nil)));})();console.log(jmId_0);"

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

                $ Right "var jmId_0;jmId_0 = (function(){var Just;var jmId_1;jmId_1 = (function($0){this[0] = $0;});Just = (function(jmId_2){return new jmId_1(jmId_2);});Just.__type__ = jmId_1;var None;var jmId_3;jmId_3 = (function(){});None = new jmId_3();None.__type__ = jmId_3;var jmId_4;jmId_4 = Just(5.0);if(((jmId_4 instanceof Just.__type__)&&(\"blammo\"==jmId_4[0]))){return false;};throw(\"Pattern Match Exhausted\");})();console.log(jmId_0);"

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

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = 4.0;var jmId_4;jmId_4 = (function(jmId_6){return (jmId_6+2.0);});return jmId_4(jmId_1);})();console.log(jmId_0);"

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

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = 3.0;var jmId_4;jmId_4 = (function(jmId_6){return (jmId_6+jmId_1);});var jmId_9;jmId_9 = 2.0;return (jmId_4(3.0)==6.0);})();console.log(jmId_0);"

            it "should type check records" $ assertGenerate

                (LetExpr (Sym "x")
                         (RecExpr (Record (M.fromList [ ("x", VarExpr (LitVal (NumLit 4.0)))
                                                      , ("y", VarExpr (LitVal (NumLit 6.0))) ])))
                         (VarExpr (SymVal (Sym "x"))))

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = { 'x': 4.0,'y': 6.0};return jmId_1;})();console.log(jmId_0);"

            it "should type check records that don't unify" $ assertGenerate

                (LetExpr (Sym "x")
                         (RecExpr (Record (M.fromList [ ("x", VarExpr (LitVal (NumLit 4.0)))
                                                      , ("y", VarExpr (LitVal (NumLit 6.0))) ])))
                         (LetExpr (Sym "y")
                                  (RecExpr (Record (M.fromList [ ("x", VarExpr (LitVal (NumLit 4.0)))
                                                               , ("z", VarExpr (LitVal (NumLit 3.0))) ])))
                                  (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                                    (VarExpr (SymVal (Sym "x")))) 
                                           (VarExpr (SymVal (Sym "y"))))))

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = { 'x': 4.0,'y': 6.0};var jmId_4;jmId_4 = { 'x': 4.0,'z': 3.0};return (jmId_1==jmId_4);})();console.log(jmId_0);"

            it "should type check records that do unify" $ assertGenerate

                (LetExpr (Sym "x")
                         (RecExpr (Record (M.fromList [ ("x", VarExpr (LitVal (NumLit 4.0)))
                                                      , ("y", VarExpr (LitVal (NumLit 6.0))) ])))
                         (LetExpr (Sym "y")
                                  (RecExpr (Record (M.fromList [ ("x", VarExpr (LitVal (NumLit 4.0)))
                                                               , ("y", VarExpr (LitVal (NumLit 3.0))) ])))
                                  (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                                    (VarExpr (SymVal (Sym "x")))) 
                                           (VarExpr (SymVal (Sym "y"))))))

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = { 'x': 4.0,'y': 6.0};var jmId_4;jmId_4 = { 'x': 4.0,'y': 3.0};return (jmId_1==jmId_4);})();console.log(jmId_0);"


            it "should type check record patterns" $ assertGenerate

                (LetExpr (Sym "f") (RecExpr (Record (M.fromList [("x",VarExpr (LitVal (NumLit 3.0)))]))) (MatExpr (VarExpr (SymVal (Sym "f"))) [(RecPatt (Record (M.fromList [("x",ValPatt (SymVal (Sym "x")))])),VarExpr (SymVal (Sym "x"))),(ValPatt (SymVal (Sym "_")),VarExpr (LitVal (NumLit 0.0)))]))

                $ Right "var jmId_0;jmId_0 = (function(){var jmId_1;jmId_1 = { 'x': 3.0};var jmId_4;jmId_4 = jmId_1;return jmId_4[\"x\"];})();console.log(jmId_0);"




------------------------------------------------------------------------------