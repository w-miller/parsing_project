module IntTokeniser where

import Data.Char

-- The potential lexemes we'll find in the source input.
data Lexeme =
      Plus
    | Mul
    | LParen
    | RParen
    | AInt Int
        deriving (Show, Eq, Ord)
-- Define these explicitly to avoid Derive TH library dependency.
isPlus, isMul, isLParen, isRParen, isAInt ::  Lexeme -> Bool
isPlus Plus = True
isPlus _    = False
isMul Mul = True
isMul _    = False
isLParen LParen = True
isLParen _    = False
isRParen RParen = True
isRParen _    = False
isAInt (AInt _) = True
isAInt _ = False

-- Take a token from the string if possible, and return the rest of the string.
takeToken :: String -> (Maybe Lexeme, String)
takeToken ""       = (Nothing, "")
takeToken (' ':s)  = (Nothing, s)
takeToken ('\n':s) = (Nothing, s)
takeToken ('\t':s) = (Nothing, s)
takeToken ('+':s)  = (Just Plus, s)
takeToken ('*':s)  = (Just Mul, s)
takeToken ('(':s)  = (Just LParen, s)
takeToken (')':s)  = (Just RParen, s)
takeToken (c:s) | isDigit c =
    case takeToken s of
        ((Just (AInt i)), s') -> (Just $ AInt $ read (c:(show i)), s')
        _                    -> (Just $ AInt $ read [c], s)
takeToken e = error $ e ++ " does not start with a valid token."

-- Tokenises a string by mapping substrings to lexemes.
tokenise :: String -> [Lexeme]
tokenise "" = []
tokenise s  =
    case takeToken s of
        (Just t, s')  -> (t:tokenise s')
        (Nothing, s') -> (tokenise s')
