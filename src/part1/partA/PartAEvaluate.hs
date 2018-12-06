module PartAEvaluate where

import Evaluate
import RecDesc hiding (main)

main :: IO ()
main = do
    inp <- getContents
    case recDescParse inp of
        Just e  -> putStrLn $ show $ evaluateExpr e
        Nothing -> error "Couldn't evaluate"
