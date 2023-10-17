module Main where

import qualified Types.PopulationComputer as PC (OutputValues(..), PopulationComputer, PopulationProtocol)
import Types.Predicates
import PresburgerMapper.DoublePhi
import PresburgerMapper.Remainder
import PresburgerMapper.Threshold
import PresburgerMapper.Union
import Transformer.Preprocess
import Transformer.Binarise

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet
import InputOutput.MapInput (stringToPredicate)

main :: IO ()
main = do
    putStrLn "Please enter the predicate you want to transform:"
    putStrLn "(The predicate must be in the following format:"
    putStrLn "    - Modulo predicates: [a1,..,as];m;c"
    putStrLn "    - Threshold predicates: [a1,..,as];c"
    putStrLn "    - Operator AND: AND"
    putStrLn "    - Operator OR: OR"
    putStrLn "    - Operator NOT: NOT"
    putStrLn "    - Parenthesis: (...)"
    putStrLn "    - coefficents must be seperated by a single `,` (no additional spaces)"
    putStrLn "      the coefficients and each constant must be seperated by a single `;` (no additional spaces either)"
    putStrLn "      predicates and Operators must be separated by at least one space"
    putStrLn "    - there may be no spaces within a predicate"
    putStrLn "      more than one spaces between a predicate and an operator are accepted"
    putStrLn "      multiple spaces around parenthesis are accepted (not around brackets)"
    putStrLn "  e. g. `([8,5,1];7;2 AND [-2,1];5) OR [9,4,3];2;1`"
    putStrLn "  Note:"
    putStrLn "    - AND has a higher precedence than OR"
    putStrLn "    - NOT refers to the following single predicate or parenthesis"
    putStrLn ")"
    putStrLn "Input: "

    predicateString <- getLine

    putStrLn "Do you want to perform the preprocessing step? (y/n)"

    perfPreproc <- getLine

    putStrLn ""
    putStrLn "Output: "

    print $ binarise $ (if perfPreproc == "y" then preprocess else id) $ constructPC $ doublePredicate $ stringToPredicate predicateString

    -- putStrLn ""
    -- putStrLn "--------------------------------------"
    -- putStrLn ""

    -- print $ constructRemainderPC (RP [8, 4, 1] 11 4)
    -- print $ constructThresholdPC (TP [-2, 1] 5) 1
    -- print $ constructPC (NodeP (Leaf (Left (RP [[8], [5]] 11 4))) OR (Leaf (Right (TP [[negate 2], [1]] 5))))


-- we also have to get a path to a destination file for our output (or print it to the command line which is a bit unfriendly; or add both options)

-- possible extension: direct link to popsim
