module Evaluate where

import Part1Grammar

evaluateExpr :: Expr -> Int
evaluateExpr (Expr t e') = evaluateTerm t + evaluateExpr' e'

evaluateExpr' :: Expr' -> Int
evaluateExpr' Expr'Epsilon = 0
evaluateExpr' (Expr'Plus t e') = evaluateTerm t + evaluateExpr' e'

evaluateTerm :: Term -> Int
evaluateTerm (Term a t') = evaluateAtom a * evaluateTerm' t'

evaluateTerm' :: Term' -> Int
evaluateTerm' Term'Epsilon = 1
evaluateTerm' (Term'Mul a t') = evaluateAtom a * evaluateTerm' t'

evaluateAtom :: Atom -> Int
evaluateAtom (Atom e) = evaluateExpr e
evaluateAtom (AtomInt i) = i
