module Unit.TypeCheck.ExprSpec where

import Test.Hspec
import Test.HUnit

import Forml.TypeCheck
import Forml.AST

assertCheck :: Expr -> Maybe Err -> Assertion
assertCheck a b = assertEqual "" checked b
    where
        checked = case typeCheck a of
            Left x -> Just x
            Right _ -> Nothing

spec :: Spec
spec = do

    describe "Forml.type checkr" $ do

    	describe "type checkOhml" $ do

            it "should type check a trivial example " $ assertCheck
              
                (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                  (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                    (VarExpr (LitVal (NumLit 2.0))))
                                           (VarExpr (LitVal (NumLit 2.0)))))
                         (VarExpr (LitVal (NumLit 4.0))))

                Nothing

            it "should type check simple type errors" $ assertCheck

                (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                  (VarExpr (LitVal (NumLit 4.0))))
                         (VarExpr (LitVal (StrLit "whoops"))))

                $ Just (Err "Types do not unify\n  TypeSym (TypeSymT Star \"Double\") (Star) and TypeSym (TypeSymT Star \"String\") (Star)")

            it "should type check let expressions" $ assertCheck

                (LetExpr (Sym "x")
                         (VarExpr (LitVal (NumLit 1.0)))
                         (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                           (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                             (VarExpr (SymVal (Sym "x"))))
                                                    (VarExpr (LitVal (NumLit 1.0)))))
                                  (VarExpr (LitVal (NumLit 2.0)))))

                Nothing

            it "should type check anonymous functions and application" $ assertCheck

                (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                  (AppExpr (AbsExpr (Sym "x")
                                                    (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                                      (VarExpr (SymVal (Sym "x"))))
                                                             (VarExpr (LitVal (NumLit 4.0)))))
                                           (VarExpr (LitVal (NumLit 2.0)))))
                         (VarExpr (LitVal (NumLit 6.0))))

                Nothing

            it "should type check anonymous functions in let bindings" $ assertCheck
             
                (LetExpr (Sym "g")
                         (AbsExpr (Sym "x")
                                  (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                    (VarExpr (SymVal (Sym "x"))))
                                           (VarExpr (LitVal (NumLit 4.0))))) 
                         (AppExpr (AppExpr (VarExpr (SymVal (Sym "==")))
                                           (AppExpr (VarExpr (SymVal (Sym "g")))
                                                    (VarExpr (LitVal (NumLit 2.0)))))
                                  (VarExpr (LitVal (NumLit 6.0)))))

                Nothing

            it "should type check match expressions" $ assertCheck

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

                Nothing

            it "should type check match expressions with type errors" $ assertCheck

                (MatExpr (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                           (VarExpr (LitVal (NumLit 3.0))))
                                  (VarExpr (LitVal (NumLit 3.0))))
                         [(ValPatt (LitVal (StrLit "gotcha")), VarExpr (SymVal (Sym "false")))])

                $ Just (Err "Types do not unify\n  TypeSym (TypeSymT Star \"String\") (Star) and TypeSym (TypeSymT Star \"Double\") (Star)")

            it "should type check user data types" $ assertCheck

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

                Nothing

            it "should type check with generic type errors" $ assertCheck

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

                $ Just (Err "Types do not unify\n  TypeSym (TypeSymT Star \"String\") (Star) and TypeSym (TypeSymT Star \"Double\") (Star)")

            it "should type check partial keywords" $ assertCheck

                (LetExpr (Sym "letly")
                         (VarExpr (LitVal (NumLit 4.0)))
                         (LetExpr (Sym "func")
                                  (AbsExpr (Sym "x")
                                           (AppExpr (AppExpr (VarExpr (SymVal (Sym "+")))
                                                             (VarExpr (SymVal (Sym "x"))))
                                                    (VarExpr (LitVal (NumLit 2.0)))))
                                  (AppExpr (VarExpr (SymVal (Sym "func")))
                                           (VarExpr (SymVal (Sym "letly"))))))

                Nothing

            it "should type check binding tricks" $ assertCheck

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

                Nothing

------------------------------------------------------------------------------
