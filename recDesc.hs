module RecDesc (module RecDesc, module Control.Applicative) where

import Control.Applicative

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

newtype Parser a = P ([Lexeme] -> Maybe (a, [Lexeme]))

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                           Nothing       -> Nothing
                           Just (v, out) -> Just (g v, out))

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> Just (v,inp))

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             Nothing       -> Nothing
                             Just (g, out) -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           Nothing       -> Nothing
                           Just (v, out) -> parse (f v) out)


instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\_ -> Nothing)

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           Nothing    -> parse q inp
                           j@(Just _) -> j)

parse :: Parser a -> [Lexeme] -> Maybe (a, [Lexeme])
parse (P p) inp = p inp

item :: Parser Lexeme
item = P (\inp -> case inp of
                     [] -> Nothing
                     (l:ls)  -> Just (l, ls))

sat :: (Lexeme -> Bool) -> Parser Lexeme
sat p = do x <- item
           if p x then return x else empty

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
