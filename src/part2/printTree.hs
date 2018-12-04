module PrintTree where

import Data.Tree

import BottomUp hiding (main)
import Tokeniser


eToTree :: E -> Tree String
eToTree (Plus e t)  = Node "E: +" [eToTree e, tToTree t]
eToTree (Minus e t) = Node "E: -" [eToTree e, tToTree t]
eToTree (T t)       = Node "E" [tToTree t]

tToTree :: T -> Tree String
tToTree (Mul t f) = Node "T: *" [tToTree t, fToTree f]
tToTree (Div t f) = Node "T: /" [tToTree t, fToTree f]
tToTree (F f)     = Node "T" [fToTree f]

fToTree :: F -> Tree String
fToTree (Parens e) = Node "F: ()" [eToTree e]
fToTree (Int i)    = Node ("F: " ++ show i) []
fToTree (Id s)     = Node ("F: " ++ s) []

main :: IO ()
main = getContents >>= putStrLn . drawTree . eToTree . bottomUp. tokenise
