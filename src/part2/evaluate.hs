module Evaluate where

import Data.Map hiding (map)
import Data.List.Split
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

-- Convert a list like ["a", "1"] to a tuple like ("a", 1).
toStringIntPair :: [String] -> (String, Int)
toStringIntPair [x, y] = (x, read y)
toStringIntPair _      = error "Can't convert list without two elements."

main :: IO ()
main = do
    args <- getArgs
    -- Construct a symbol table mapping variable names to integers.
    let symTable = fromList $ map (toStringIntPair . splitOn "=") args
    -- Take expression string from input (usually stdin), and variable bindings
    -- from arguments, in the format x=1.
    getContents >>= print . evaluateE symTable . bottomUp . tokenise
