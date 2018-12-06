module PartAPrettyPrint where

import PrettyPrint
import RecDesc hiding (main)

main :: IO ()
main = getContents >>= putStrLn . prettyFormatMaybeExpr . recDescParse
