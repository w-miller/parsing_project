module Grammar where

-- The top-level production.
data Sentence =
      SenE Expr
    | SenT Term
    | SenA Atom
        deriving (Show, Eq, Ord)

-- Use the non-left-recursive version of the grammar for recursive descent.
data Expr =
      Expr Term Expr'
        deriving (Show, Eq, Ord)

data Expr' =
      Expr'Plus Term Expr'
    | Expr'Epsilon
        deriving (Show, Eq, Ord)

data Term =
      Term Atom Term'
        deriving (Show, Eq, Ord)

data Term' =
      Term'Mul Atom Term'
    | Term'Epsilon
        deriving (Show, Eq, Ord)

data Atom =
      Atom Expr
    | AtomInt Int
        deriving (Show, Eq, Ord)
