module PrettyPrint (prettyFormatMaybeExpr, prettyFormatExpr) where

import Part1Grammar

prettyFormatMaybeExpr :: Maybe Expr -> String
prettyFormatMaybeExpr (Nothing) = ""
prettyFormatMaybeExpr (Just e) = prettyFormatExpr e

prettyFormatExpr :: Expr -> String
prettyFormatExpr (Expr t e') =
     prettyFormatTerm t ++ prettyFormatExpr' e'

prettyFormatExpr' :: Expr' -> String
prettyFormatExpr' (Expr'Plus t e') = " + " ++ prettyFormatTerm t ++  prettyFormatExpr' e'
prettyFormatExpr' (Expr'Epsilon) = ""

prettyFormatTerm :: Term -> String
prettyFormatTerm (Term a t') = prettyFormatAtom a ++ prettyFormatTerm' t'

prettyFormatTerm' :: Term' -> String
prettyFormatTerm' (Term'Mul a t') = " * " ++ prettyFormatAtom a ++ prettyFormatTerm' t'
prettyFormatTerm' (Term'Epsilon) = ""

prettyFormatAtom :: Atom -> String
prettyFormatAtom (Atom e) = "(" ++ prettyFormatExpr e ++ ")"
prettyFormatAtom (AtomInt i) = show i
