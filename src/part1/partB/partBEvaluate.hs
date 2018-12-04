module PartBEvaluate where

import Evaluate
import Predictive hiding (main)

main :: IO ()
main = do
    inp <- getContents
    case predictiveParse inp of
        Just e  -> putStrLn $ show $ evaluateExpr e
        Nothing -> error "Couldn't evaluate"
