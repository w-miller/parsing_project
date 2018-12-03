module MonadicParser (module MonadicParser, module Control.Applicative) where

import IntTokeniser

import Control.Applicative

-- Generic parser to extract tokens from a list of lexemes.
newtype Parser a = P ([Lexeme] -> Maybe (a, [Lexeme]))

-- Required instantiations for monadic parsing.
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

-- Attempt to use a parser to extract a token from the list, returning Just a
-- if successful.
parse :: Parser a -> [Lexeme] -> Maybe (a, [Lexeme])
parse (P p) inp = p inp

-- Extract a single lexeme from the list.
item :: Parser Lexeme
item = P (\inp -> case inp of
                     [] -> Nothing
                     (l:ls)  -> Just (l, ls))

-- Parser to test whether or not a lexeme satisfies a predicate.
sat :: (Lexeme -> Bool) -> Parser Lexeme
sat p = do x <- item
           if p x then return x else empty
