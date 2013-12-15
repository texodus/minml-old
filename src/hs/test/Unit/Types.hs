module Unit.Types where

import Data.Map
import Minml.AST
types = [Right (AppExpr (AppExpr (VarExpr (SymVal (Sym "=="))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (LitVal (NumLit 2.0)))) (VarExpr (LitVal (NumLit 2.0))))) (VarExpr (LitVal (NumLit 4.0)))),Left (Err "Types do not unify\n  Double and String"),Right (LetExpr (Sym "x") (VarExpr (LitVal (NumLit 1.0))) (Just (AppExpr (AppExpr (VarExpr (SymVal (Sym "=="))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 1.0))))) (VarExpr (LitVal (NumLit 2.0)))))),Left (Err "Unbound identifier: x"),Right (AppExpr (AppExpr (VarExpr (SymVal (Sym "=="))) (AppExpr (AbsExpr (Sym "x") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 4.0))))) (VarExpr (LitVal (NumLit 2.0))))) (VarExpr (LitVal (NumLit 6.0)))),Right (LetExpr (Sym "g") (AbsExpr (Sym "x") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 4.0))))) (Just (AppExpr (AppExpr (VarExpr (SymVal (Sym "=="))) (AppExpr (VarExpr (SymVal (Sym "g"))) (VarExpr (LitVal (NumLit 2.0))))) (VarExpr (LitVal (NumLit 6.0)))))),Right (LetExpr (Sym "fib") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (LitVal (NumLit 0.0)),VarExpr (LitVal (NumLit 0.0))),(ValPatt (LitVal (NumLit 1.0)),VarExpr (LitVal (NumLit 1.0))),(ValPatt (SymVal (Sym "n")),AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 1.0)))))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 2.0))))))])) (Just (AppExpr (VarExpr (SymVal (Sym "fib"))) (VarExpr (LitVal (NumLit 7.0)))))),Left (Err "Unbound identifier: x"),Right (LetExpr (Sym "fib") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (LitVal (NumLit 0.0)),VarExpr (LitVal (NumLit 0.0))),(ValPatt (LitVal (NumLit 1.0)),VarExpr (LitVal (NumLit 1.0))),(ValPatt (SymVal (Sym "n")),AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 1.0)))))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 2.0))))))])) (Just (AppExpr (VarExpr (SymVal (Sym "fib"))) (VarExpr (LitVal (NumLit 7.0)))))),Left (Err "Types do not unify\n  String and Double"),Right (TypExpr (TypeSymP "Cons") (TypeAbsP (TypeApp (TypeApp (TypeSym (TypeSymP "->")) (TypeVar (TypeVarP "a"))) (TypeApp (TypeApp (TypeSym (TypeSymP "->")) (TypeApp (TypeSym (TypeSymP "List")) (TypeVar (TypeVarP "a")))) (TypeApp (TypeSym (TypeSymP "List")) (TypeVar (TypeVarP "a")))))) (Just (TypExpr (TypeSymP "Nil") (TypeAbsP (TypeApp (TypeSym (TypeSymP "List")) (TypeVar (TypeVarP "a")))) (Just (LetExpr (Sym "length") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (ConVal (TypeSym (TypeSymP "Nil"))),VarExpr (LitVal (NumLit 0.0))),(ConPatt (TypeSymP "Cons") [ValPatt (SymVal (Sym "_")),ValPatt (SymVal (Sym "xs"))],AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (LitVal (NumLit 1.0)))) (AppExpr (VarExpr (SymVal (Sym "length"))) (VarExpr (SymVal (Sym "xs")))))])) (Just (AppExpr (VarExpr (SymVal (Sym "length"))) (AppExpr (AppExpr (VarExpr (ConVal (TypeSym (TypeSymP "Cons")))) (VarExpr (LitVal (NumLit 1.0)))) (AppExpr (AppExpr (VarExpr (ConVal (TypeSym (TypeSymP "Cons")))) (VarExpr (LitVal (NumLit 2.0)))) (VarExpr (ConVal (TypeSym (TypeSymP "Nil"))))))))))))),Left (Err "Types do not unify\n  String and Double"),Right (LetExpr (Sym "letly") (VarExpr (LitVal (NumLit 4.0))) (Just (LetExpr (Sym "func") (AbsExpr (Sym "x") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 2.0))))) (Just (AppExpr (VarExpr (SymVal (Sym "func"))) (VarExpr (SymVal (Sym "letly")))))))),Right (LetExpr (Sym "x") (VarExpr (LitVal (NumLit 3.0))) (Just (LetExpr (Sym "f") (AbsExpr (Sym "y") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "y")))) (VarExpr (SymVal (Sym "x"))))) (Just (LetExpr (Sym "x") (VarExpr (LitVal (NumLit 2.0))) (Just (AppExpr (AppExpr (VarExpr (SymVal (Sym "=="))) (AppExpr (VarExpr (SymVal (Sym "f"))) (VarExpr (LitVal (NumLit 3.0))))) (VarExpr (LitVal (NumLit 6.0)))))))))),Right (LetExpr (Sym "x") (RecExpr (Record (fromList [("x",VarExpr (LitVal (NumLit 4.0))),("y",VarExpr (LitVal (NumLit 6.0)))]))) (Just (VarExpr (SymVal (Sym "x"))))),Left (Err "Types do not unify\n  {x: Double, y: Double} and {x: Double, z: Double}"),Left (Err "Unbound identifier: x"),Left (Err "Unbound identifier: f"),Right (LetExpr (Sym "letly") (VarExpr (LitVal (NumLit 4.0))) (Just (LetExpr (Sym "func") (AbsExpr (Sym "x") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 2.0))))) (Just (AppExpr (VarExpr (SymVal (Sym "func"))) (VarExpr (SymVal (Sym "letly")))))))),Left (Err "Unbound identifier: func"),Right (LetExpr (Sym "fib") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (LitVal (NumLit 0.0)),VarExpr (LitVal (NumLit 0.0))),(ValPatt (LitVal (NumLit 1.0)),VarExpr (LitVal (NumLit 1.0))),(ValPatt (SymVal (Sym "n")),AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 1.0)))))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 2.0))))))])) (Just (AppExpr (VarExpr (SymVal (Sym "fib"))) (VarExpr (LitVal (NumLit 7.0)))))),Right (LetExpr (Sym "fib") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (LitVal (NumLit 0.0)),VarExpr (LitVal (NumLit 0.0))),(ValPatt (LitVal (NumLit 1.0)),VarExpr (LitVal (NumLit 1.0))),(ValPatt (SymVal (Sym "n")),AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 1.0)))))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 2.0))))))])) (Just (AppExpr (VarExpr (SymVal (Sym "fib"))) (VarExpr (LitVal (NumLit 7.0)))))),Left (Err "Types do not unify\n  String and Double"),Right (LetExpr (Sym "letly") (VarExpr (LitVal (NumLit 4.0))) (Just (LetExpr (Sym "func") (AbsExpr (Sym "x") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 2.0))))) (Just (AppExpr (VarExpr (SymVal (Sym "func"))) (VarExpr (SymVal (Sym "letly")))))))),Right (LetExpr (Sym "fib") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (LitVal (NumLit 0.0)),VarExpr (LitVal (NumLit 0.0))),(ValPatt (LitVal (NumLit 1.0)),VarExpr (LitVal (NumLit 1.0))),(ValPatt (SymVal (Sym "n")),AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 1.0)))))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 2.0))))))])) (Just (AppExpr (VarExpr (SymVal (Sym "fib"))) (VarExpr (LitVal (NumLit 7.0)))))),Right (LetExpr (Sym "g") (AbsExpr (Sym "x") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 4.0))))) (Just (AppExpr (AppExpr (VarExpr (SymVal (Sym "=="))) (AppExpr (VarExpr (SymVal (Sym "g"))) (VarExpr (LitVal (NumLit 2.0))))) (VarExpr (LitVal (NumLit 6.0)))))),Right (LetExpr (Sym "letly") (VarExpr (LitVal (NumLit 4.0))) (Just (LetExpr (Sym "func") (AbsExpr (Sym "x") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 2.0))))) (Just (AppExpr (VarExpr (SymVal (Sym "func"))) (VarExpr (SymVal (Sym "letly")))))))),Right (LetExpr (Sym "fib") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (LitVal (NumLit 0.0)),VarExpr (LitVal (NumLit 0.0))),(ValPatt (LitVal (NumLit 1.0)),VarExpr (LitVal (NumLit 1.0))),(ValPatt (SymVal (Sym "n")),AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 1.0)))))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 2.0))))))])) (Just (AppExpr (VarExpr (SymVal (Sym "fib"))) (VarExpr (LitVal (NumLit 7.0)))))),Right (LetExpr (Sym "fib") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (LitVal (NumLit 0.0)),VarExpr (LitVal (NumLit 0.0))),(ValPatt (LitVal (NumLit 1.0)),VarExpr (LitVal (NumLit 1.0))),(ValPatt (SymVal (Sym "n")),AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 1.0)))))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 2.0))))))])) (Just (AppExpr (VarExpr (SymVal (Sym "fib"))) (VarExpr (LitVal (NumLit 7.0)))))),Right (LetExpr (Sym "g") (AbsExpr (Sym "x") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 4.0))))) (Just (AppExpr (AppExpr (VarExpr (SymVal (Sym "=="))) (AppExpr (VarExpr (SymVal (Sym "g"))) (VarExpr (LitVal (NumLit 2.0))))) (VarExpr (LitVal (NumLit 6.0)))))),Right (LetExpr (Sym "letly") (VarExpr (LitVal (NumLit 4.0))) (Just (LetExpr (Sym "func") (AbsExpr (Sym "x") (AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (SymVal (Sym "x")))) (VarExpr (LitVal (NumLit 2.0))))) (Just (AppExpr (VarExpr (SymVal (Sym "func"))) (VarExpr (SymVal (Sym "letly")))))))),Right (LetExpr (Sym "fib") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (LitVal (NumLit 0.0)),VarExpr (LitVal (NumLit 0.0))),(ValPatt (LitVal (NumLit 1.0)),VarExpr (LitVal (NumLit 1.0))),(ValPatt (SymVal (Sym "n")),AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 1.0)))))) (AppExpr (VarExpr (SymVal (Sym "fib"))) (AppExpr (AppExpr (VarExpr (SymVal (Sym "-"))) (VarExpr (SymVal (Sym "n")))) (VarExpr (LitVal (NumLit 2.0))))))])) (Just (AppExpr (VarExpr (SymVal (Sym "fib"))) (VarExpr (LitVal (NumLit 7.0)))))),Right (TypExpr (TypeSymP "Cons") (TypeAbsP (TypeApp (TypeApp (TypeSym (TypeSymP "->")) (TypeVar (TypeVarP "a"))) (TypeApp (TypeApp (TypeSym (TypeSymP "->")) (TypeApp (TypeSym (TypeSymP "List")) (TypeVar (TypeVarP "a")))) (TypeApp (TypeSym (TypeSymP "List")) (TypeVar (TypeVarP "a")))))) (Just (TypExpr (TypeSymP "Nil") (TypeAbsP (TypeApp (TypeSym (TypeSymP "List")) (TypeVar (TypeVarP "a")))) (Just (LetExpr (Sym "length") (AbsExpr (Sym "n") (MatExpr (VarExpr (SymVal (Sym "n"))) [(ValPatt (ConVal (TypeSym (TypeSymP "Nil"))),VarExpr (LitVal (NumLit 0.0))),(ConPatt (TypeSymP "Cons") [ValPatt (SymVal (Sym "_")),ValPatt (SymVal (Sym "xs"))],AppExpr (AppExpr (VarExpr (SymVal (Sym "+"))) (VarExpr (LitVal (NumLit 1.0)))) (AppExpr (VarExpr (SymVal (Sym "length"))) (VarExpr (SymVal (Sym "xs")))))])) (Just (AppExpr (VarExpr (SymVal (Sym "length"))) (AppExpr (AppExpr (VarExpr (ConVal (TypeSym (TypeSymP "Cons")))) (VarExpr (LitVal (NumLit 1.0)))) (AppExpr (AppExpr (VarExpr (ConVal (TypeSym (TypeSymP "Cons")))) (VarExpr (LitVal (NumLit 2.0)))) (VarExpr (ConVal (TypeSym (TypeSymP "Nil"))))))))))))),Right (LetExpr (Sym "f") (RecExpr (Record (fromList [("x",VarExpr (LitVal (NumLit 3.0)))]))) (Just (MatExpr (VarExpr (SymVal (Sym "f"))) [(RecPatt (Record (fromList [("x",ValPatt (SymVal (Sym "x")))])),VarExpr (SymVal (Sym "x"))),(ValPatt (SymVal (Sym "_")),VarExpr (LitVal (NumLit 0.0)))])))]