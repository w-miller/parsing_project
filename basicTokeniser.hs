module BasicTokeniser where

-- The potential lexemes we'll find in the source input.
data Lexeme =
      Plus
    | Mul
    | LParen
    | RParen
    | A
        deriving (Show, Eq, Ord)
-- Define these explicitly to avoid Derive TH library dependency.
isPlus, isMul, isLParen, isRParen, isA ::  Lexeme -> Bool
isPlus Plus = True
isPlus _    = False
isMul Mul = True
isMul _    = False
isLParen LParen = True
isLParen _    = False
isRParen RParen = True
isRParen _    = False
isA A = True
isA _    = False

-- Defines a mapping from characters found in the source string to lexemes.
tokenMap :: Char -> Maybe Lexeme
tokenMap '+'  = Just Plus
tokenMap '*'  = Just Mul
tokenMap '('  = Just LParen
tokenMap ')'  = Just RParen
tokenMap 'a'  = Just A
tokenMap ' '  = Nothing
tokenMap '\n' = Nothing
tokenMap '\t' = Nothing
tokenMap e = error $ e:" is not a valid token."

-- Maps a function returning Maybe monads, placing the values of 'Just' cons
-- in the output list, and skipping over any 'Nothing' cons.
maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap f (x:xs)
    | Just c <- f x = c:(maybeMap f xs)
    | otherwise     = maybeMap f xs
maybeMap _ [] = []

-- Tokenises a string by mapping characters to lexemes.
tokenise :: String -> [Lexeme]
tokenise = maybeMap tokenMap
