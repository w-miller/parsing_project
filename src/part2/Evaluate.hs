module Evaluate where

import Data.Map hiding (map)
import Prelude hiding (lookup)
import System.Environment

import BottomUp hiding (main)
import Tokeniser

-- The symbol table storing variable mappings.
type SymTable = Map String Int

-- The main evaluation entry point.
evaluateE :: SymTable -> E -> Int
evaluateE s (Plus e t)  = evaluateE s e + evaluateT s t
evaluateE s (Minus e t) = evaluateE s e - evaluateT s t
evaluateE s (T t)       = evaluateT s t

evaluateT :: SymTable -> T -> Int
evaluateT s (Mul t f) = evaluateT s t * evaluateF s f
evaluateT s (Div t f) = evaluateT s t `div` evaluateF s f
evaluateT s (F f)     = evaluateF s f

evaluateF :: SymTable -> F -> Int
evaluateF s (Parens e) = evaluateE s e
evaluateF _ (Int i)    = i
evaluateF s (Id k)     =
    case lookup k s of
        Just v  -> v
        Nothing -> error $ "The symbol '" ++ k ++ "' was not defined."

-- Convert a string like "k=10" to ("k", 10)
splitKV :: String -> (String, Int)
splitKV (c:c':cs) | c' == '=' = ([c], read cs)
                  | otherwise = ((c:k), v)
    where (k, v) = splitKV $ c':cs
splitKV e = error $ "Cannot split " ++ e

main :: IO ()
main = do
    args <- getArgs
    -- Construct a symbol table mapping variable names to integers.
    let symTable = fromList $ map splitKV args
    -- Take expression string from input (usually stdin), and variable bindings
    -- from arguments, in the format x=1.
    getContents >>= print . evaluateE symTable . bottomUp . tokenise
