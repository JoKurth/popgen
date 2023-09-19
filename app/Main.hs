module Main where

import qualified Types.PopulationComputer as PC (OutputValues(..), PopulationComputer, PopulationProtocol)
import Types.Predicates
import PresburgerMapper.Remainder
import PresburgerMapper.Threshold
import PresburgerMapper.Union

import Data.List.Split (splitOn)
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet
import InputOutput.MapInput (stringToPredicate)

main :: IO ()
main = do
    putStrLn "Please enter the predicate you want to transform:"
    putStrLn "(The predicate must be in disjunctive normal form using the following format:"
    putStrLn "    - Modulo predicates: [a1 .. as] m c"
    putStrLn "    - Threshold predicates: [a1 .. as] c"
    putStrLn "    - Operator AND: &&"
    putStrLn "    - Operator OR: ||"
    putStrLn "    - Operator NOT: !"
    putStrLn "  e. g. `[8, 5, 1] 7 2 && [-2, 1] 5 || [9, 4, 3] 2 1`"
    putStrLn ")"

    predicateString <- getLine
    let conjunctions = splitOn "||" predicateString
    let splittetPredicate = map (splitOn "&&") conjunctions

    putStrLn ""
    putStrLn "Output: "

    print $ constructPC $ stringToPredicate splittetPredicate

    -- putStrLn ""
    -- putStrLn "--------------------------------------"
    -- putStrLn ""

    -- print $ constructRemainderPC (RP [8, 4, 1] 11 4)
    -- print $ constructThresholdPC (TP [-2, 1] 5) 1
    -- print $ constructPC (NodeP (Leaf (Left (RP [[8], [5]] 11 4))) OR (Leaf (Right (TP [[negate 2], [1]] 5))))


-- we also have to get a path to a destination file for our output (or print it to the command line which is a bit unfriendly; or add both options)

-- possible extension: direct link to popsim
