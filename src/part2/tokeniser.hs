module Tokeniser where

import Data.Char

-- The potential lexemes we'll find in the source input.
data Lexeme =
      LexPlus
    | LexMinus
    | LexMul
    | LexDiv
    | LexLParen
    | LexRParen
    | LexInt Int
    | LexId String
    deriving (Show, Eq, Ord)

-- Take a token from the string if possible, and return the rest of the string.
takeToken :: String -> (Maybe Lexeme, String)
takeToken ""       = (Nothing, "")
takeToken (' ':s)  = (Nothing, s)
takeToken ('\n':s) = (Nothing, s)
takeToken ('\t':s) = (Nothing, s)
takeToken ('+':s)  = (Just LexPlus, s)
takeToken ('-':s)  = (Just LexMinus, s)
takeToken ('*':s)  = (Just LexMul, s)
takeToken ('/':s)  = (Just LexDiv, s)
takeToken ('(':s)  = (Just LexLParen, s)
takeToken (')':s)  = (Just LexRParen, s)
takeToken cs@(c:_) | isDigit c = (Just $ LexInt $ read t, s)
-- IDs must start with alphabetical character.
                   | isAlpha c = (Just $ LexId t, s)
    where (t, s) = span (not . isSpace) cs
takeToken e = error $ e ++ " does not start with a valid token."

-- Tokenises a string by mapping substrings to lexemes.
tokenise :: String -> [Lexeme]
tokenise "" = []
tokenise s  =
    case takeToken s of
        (Just t, s')  -> (t:tokenise s')
        (Nothing, s') -> (tokenise s')
