module PresburgerMapper.Remainder (
    constructRemainderPC
) where

import Helper.Math
import Helper.List
import qualified Types.PopulationComputer as PC (OutputValues(..), PopulationComputer(..))
import qualified Types.Predicates as Predicates (RemainderPredicate(..))

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet


-- Export

-- | We apply the same restrictions to the inputs as done in the paper.
constructRemainderPC :: Predicates.RemainderPredicate -> PC.PopulationComputer Int
constructRemainderPC (Predicates.RP coefficients m c) = PC.PC {
    PC.states = Set.fromList stateList,
    PC.delta = Set.fromList transitions,
    PC.input = Set.fromList $ flatten coefficients,
    -- PC.output = outputFunction,
    PC.helpers = MultiSet.insertMany 0 (3*d) MultiSet.empty
}
    where
        d = ceilBinLog m
        stateList = 0 : [2^i | i <- [0 .. d]]
        transitions =    [(MultiSet.fromList [2^i, 2^i], MultiSet.fromList [2^(i+1), 0]) | i <- [0 .. d-1]] -- combine
                      -- TODO: clean up the following line
                      ++ [(MultiSet.fromList $ flatten [[2^d], map (0 *) [1 .. len-1]], MultiSet.fromList $ map (2^) bin) | let bin = intToBinary (2^d - m), let len = length bin] -- modulo
                      -- TODO: try if \_ -> can be replaced with const
                      ++ [(MultiSet.fromList $ map (\_ -> 2^d) [1 .. d], MultiSet.fromList $ map (2^) bin ++ map (0*) [1 .. d-len]) | let bin = intToBinary (d * 2^d `mod` m), let len = length bin] -- fast modulo
        -- outputFunction config = if sum (multiSetSupport config) `mod` m == c
        --                             then PC.T
        --                             else PC.F
