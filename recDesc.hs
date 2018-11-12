data Lexeme =
      Plus
    | Mul
    | LParen
    | RParen
    | A
        deriving (Show, Eq)

data Sentence =
      SenE Expr
    | SenT Term
    | SenA Atom
        deriving (Show, Eq)

data Expr =
      Expr `ExprPlus` Term
    | ExprT Term
        deriving (Show, Eq)

data Term =
      Term `TermMul` Atom
    | TermA Atom
        deriving (Show, Eq)

data Atom =
      AtomExpr Expr
    | Unit
        deriving (Show, Eq)

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

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap f (x:xs)
    | Just c <- f x = c:(maybeMap f xs)
    | otherwise     = maybeMap f xs
maybeMap _ [] = []

tokenise :: String -> [Lexeme]
tokenise = maybeMap tokenMap

parseSentence :: [Lexeme] -> (Maybe Sentence, [Lexeme])
parseSentence ls | (Just e) <- tok = (Just $ SenE e, rest)
                 | otherwise       = (Nothing, ls)
    where (tok, rest) = parseExpr ls

parseExpr :: [Lexeme] -> (Maybe Expr, [Lexeme])
parseExpr ls | (Just t) <- tok = (Just $ ExprT t, rest)
             | otherwise       = (Nothing, ls)
    where (tok, rest) = parseTerm ls

parseTerm :: [Lexeme] -> (Maybe Term, [Lexeme])
parseTerm ls | (Just a) <- tok = (Just $ TermA a, rest)
             | otherwise       = (Nothing, ls)
    where (tok, rest) = parseAtom ls

parseAtom :: [Lexeme] -> (Maybe Atom, [Lexeme])
parseAtom (A:ls) = (Just Unit, ls)
parseAtom ls     = (Nothing, ls)

parse' :: [Lexeme] -> Maybe Sentence
parse' ls | (s@(Just _), []) <- (sen, rest) = s
         | otherwise                       = Nothing
    where (sen, rest) = parseSentence ls

parse :: String -> Maybe Sentence
parse = parse' . tokenise
