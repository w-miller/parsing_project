module Predictive where

import Part1Grammar
import IntTokeniser
import MonadicParser

-- Parsers for each part of the grammar.
expr :: Parser Expr
expr = peek >>= \x -> exprAux x
exprAux :: Lexeme -> Parser Expr
exprAux l | l `lexEq` LParen ||
            l `lexEq` AInt undefined =
            do t <- term
               x <- expr'
               return $ Expr t x
          | otherwise = P (\_ -> Nothing)

expr' :: Parser Expr'
expr' =
    (peek >>= \x -> expr'Aux x)
    -- Allow empty lookahead since we have an ε-rule.
    <|> return Expr'Epsilon
expr'Aux :: Lexeme -> Parser Expr'
expr'Aux l | l `lexEq` Plus =
             do _ <- sat isPlus
                t <- term
                x <- expr'
                return $ Expr'Plus t x
           | l `lexEq` RParen = return Expr'Epsilon
           | otherwise = P (\_ -> Nothing)

term :: Parser Term
term = peek >>= \x -> termAux x
termAux :: Lexeme -> Parser Term
termAux l | l `lexEq` LParen ||
            l `lexEq` AInt undefined =
            do a <- atom
               t <- term'
               return $ Term a t
           | otherwise = P (\_ -> Nothing)

term' :: Parser Term'
term' =
    (peek >>= \x -> term'Aux x)
    -- Allow empty lookahead since we have an ε-rule.
    <|> return Term'Epsilon
term'Aux :: Lexeme -> Parser Term'
term'Aux l | l `lexEq` Mul =
             do _ <- sat isMul
                a <- atom
                t <- term'
                return $ Term'Mul a t
           | l `lexEq` Plus ||
             l `lexEq` RParen = return Term'Epsilon
           | otherwise = P (\_ -> Nothing)

atom :: Parser Atom
atom = peek >>= \x -> atomAux x
atomAux :: Lexeme -> Parser Atom
atomAux l | l `lexEq` LParen =
            do _ <- sat isLParen
               x <- expr
               _ <- sat isRParen
               return $ Atom x
          | l `lexEq` AInt undefined =
            do (AInt i) <- sat isAInt
               return $ AtomInt i
          | otherwise = P (\_ -> Nothing)

predictiveParse :: String -> Maybe Expr
predictiveParse = parseEntryPoint expr . tokenise
