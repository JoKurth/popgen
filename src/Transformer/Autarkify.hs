module Transformer.Autarkify (
    autarkify
) where

import qualified Types.PopulationComputer as PC (PopulationComputer(..))

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set

autarkify :: PC.PopulationComputer String -> PC.PopulationComputer String
autarkify pc = PC.PCL {
    PC.states = Set.union (PC.states pc) (Set.fromList qHelper),
    PC.delta = Set.union (Set.fromList transitions) (PC.delta pc),
    PC.input = Set.fromList [initialInput !! i | i <- [0 .. length initialInput - 1], even i],
    PC.outputOL = PC.outputOL pc,
    PC.helpers = MultiSet.empty
}
    where
        initialInput = Set.toList $ PC.input pc
        numHelpers = MultiSet.size $ PC.helpers pc
        upState n = "up_" ++ show n
        downState n = if n == 1 then hState 0 else "down_" ++ show n
        hStates = MultiSet.toList $ PC.helpers pc
        hState = (!!) hStates
        qHelper = [upState i | i <- [0 .. numHelpers]] ++ [downState i | i <- [2 .. numHelpers]] -- we do not generate the state down_0 because it is not reachable -> deviation from the paper
        transitions = [(MultiSet.fromList [x, x], MultiSet.fromList [x', upState 1]) |
                            i <- [j | j <- [0 .. length initialInput - 1], even j],
                            let x = initialInput !! i, let x' = initialInput !! (i+1)] ++ -- double
                      [(MultiSet.fromList [upState i, upState j], MultiSet.fromList [upState (i+j), upState 0]) |
                            i <- [1 .. numHelpers - 1],
                            j <- [1 .. numHelpers - 1],
                            i+j < numHelpers] ++ -- helper
                      [(MultiSet.fromList [upState i, upState j], MultiSet.fromList [downState numHelpers, upState (i + j - numHelpers)]) |
                            i <- [1 .. numHelpers - 1],
                            j <- [1 .. numHelpers - 1],
                            i+j >= numHelpers] ++
                      [(MultiSet.fromList [downState (i + 1), upState 0], MultiSet.fromList [downState i, hState i]) |
                            i <- [1 .. numHelpers - 1],
                            j <- [1 .. numHelpers - 1]]
