module RecDesc where

import Part1Grammar
import IntTokeniser
import MonadicParser

-- Parsers for each part of the grammar.
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
       do (AInt i) <- sat isAInt
          return $ AtomInt i

-- The main entry point.
recDescParse :: String -> Maybe Expr
recDescParse = parseEntryPoint expr . tokenise
