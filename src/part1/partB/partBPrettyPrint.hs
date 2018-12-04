module PartBPrettyPrint where

import Predictive hiding (main)
import PrettyPrint

main :: IO ()
main = getContents >>= putStrLn . prettyFormatMaybeExpr . predictiveParse
