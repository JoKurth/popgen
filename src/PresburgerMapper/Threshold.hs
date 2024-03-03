module PresburgerMapper.Threshold (
    constructThresholdPC
) where

import Helper.Math
import Helper.List
import Helper.BooleanCircuit (buildThresholdCircuit)
import qualified Types.PopulationComputer as PC (OutputValues(..), PopulationComputer(..), Gate(..), Edge(..), BooleanCircuit(..))
import qualified Types.Predicates as Predicates (ThresholdPredicate(..))

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet


-- Export

-- | Constructs a population computer for a given threshold predicate.
--   We apply the same restrictions to the inputs as done by Czerner et al.
constructThresholdPC :: Predicates.ThresholdPredicate -> Int -> PC.PopulationComputer Int
constructThresholdPC (Predicates.TP coefficients c) s = PC.PCB {         -- s has the same meaning as in the paper (page 18)
    PC.states = Set.fromList stateList,
    PC.delta = Set.fromList transitions,
    PC.input = Set.fromList $ flatten coefficients,
    PC.output = outputFunction,
    PC.helpers = MultiSet.insertMany 0 d MultiSet.empty
}
    where
        d = 4 + max (1 + ceilBinLog c) (ceilBinLog $ s * maximum (map abs $ flatten coefficients))
        stateList = 0 : [2^i | i <- [0, 1 .. d]] ++ [-2^i | i <- [0, 1 .. d]]
        transitions =    [(MultiSet.fromList [2^i, 2^i], MultiSet.fromList [0, 2^(i+1)]) | i <- [0 .. d-1]]                             -- combine
                      ++ [(MultiSet.fromList [negate $ 2^i, negate $ 2^i], MultiSet.fromList [0, negate $ 2^(i+1)]) | i <- [0 .. d-1]]  -- combine
                      ++ [(MultiSet.fromList [negate $ 2^i, 2^i], MultiSet.fromList [0, 0]) | i <- [0 .. d-1]]                          -- cancel
                      ++ [(MultiSet.fromList [negate $ 2^d, 2^d], MultiSet.fromList [0, 0])]                                            -- cancel
                      ++ [(MultiSet.fromList [2^d, negate $ 2^(d-1)], MultiSet.fromList [0, 2^(d-1)])]                                  -- cancel 2nd highest
                      ++ [(MultiSet.fromList [negate $ 2^d, 2^(d-1)], MultiSet.fromList [0, negate $ 2^(d-1)])]                         -- cancel 2nd highest
        outputFunction = buildThresholdCircuit c d
