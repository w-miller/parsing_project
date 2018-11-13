module RecDesc where

import MonadicParser
import Tokeniser

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
    | Unit
        deriving (Show, Eq, Ord)

sentence :: Parser Sentence
sentence = do x <- expr
              return $ SenE x
           <|>
           do t <- term
              return $ SenT t
           <|>
           do a <- atom
              return $ SenA a

expr :: Parser Expr
expr = do t <- term
          x <- expr'
          return $ Expr t x

expr' :: Parser Expr'
expr' = do _ <- sat isPlus
           t <- term
           x <- expr'
           return $ Expr'Plus t x
        <|>
        return Expr'Epsilon

term :: Parser Term
term = do a <- atom
          t <- term'
          return $ Term a t

term' :: Parser Term'
term' = do _ <- sat isMul
           a <- atom
           t <- term'
           return $ Term'Mul a t
        <|>
        return Term'Epsilon

atom :: Parser Atom
atom = do _ <- sat isLParen
          x <- expr
          _ <- sat isRParen
          return $ Atom x
       <|>
       do _ <- sat isA
          return Unit

doParse' :: [Lexeme] -> Maybe Sentence
doParse' ls = case parse sentence ls of
              Just (a, []) -> Just a
              _ -> Nothing

doParse :: String -> Maybe Sentence
doParse = doParse' . tokenise
