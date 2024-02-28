{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}

module Transformer.Binarise (
    binarise
) where

import Helper.List
import qualified Types.PopulationComputer as PC (PopulationComputer(..), Gate(..), BooleanCircuit, getQFromTransition, getPFromTransition)

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set


renameTransitionsForStates :: (MultiSet.MultiSet String, MultiSet.MultiSet String) -> [Char]
renameTransitionsForStates t = "{(" ++ show (fst t) ++ "," ++ show (snd t) ++ ")}"

buildStateName :: [Char] -> Int -> [Char]
buildStateName q number = "(" ++ q ++ "," ++ show number ++ ")"

data StateType = Q | P
    deriving Eq
buildTStates state number transition = "(" ++ (if state == Q then PC.getQFromTransition else PC.getPFromTransition) transition ++ "," ++ show number ++ "," ++ renameTransitionsForStates transition ++ ")"

buildIdentifiedTStates :: (MultiSet.MultiSet String, MultiSet.MultiSet String) -> Int -> [Char]
buildIdentifiedTStates transition i
    | i == 1 = buildTStates Q 0 transition
    | i == l = buildSIdentifiedState (MultiSet.toList (snd transition) !! (l - 1))
    | otherwise = "(" ++ renameTransitionsForStates transition ++ "," ++ show i ++ ")"
        where
            l = MultiSet.size (snd transition)

buildSIdentifiedState state = buildStateName state 1


-- | index = the index of the gate we are currently processing
buildOutputFunction :: PC.BooleanCircuit String -> Int -> PC.BooleanCircuit String
buildOutputFunction ([], edges) _ = ([], edges)
buildOutputFunction ((PC.Input "0") : gates, edges) index = (PC.Input "(0,1)" : fst nextCircuit, snd nextCircuit)
    where
        nextCircuit = buildOutputFunction (gates, edges) (index + 1)
buildOutputFunction ((PC.Input x) : gates, edges) index = (PC.OR : fst nextCircuit ++ [PC.Input $ buildStateName x 1, PC.Input $ buildStateName x 0], (index, (baseNewIndex + 1, baseNewIndex + 2)) : snd nextCircuit)
    where
        nextCircuit = buildOutputFunction (gates, edges) (index + 1)
        baseNewIndex = index + length (fst nextCircuit)
buildOutputFunction (gate:xs, edges) index = (gate : fst nextCircuit, snd nextCircuit)
    where
        nextCircuit = buildOutputFunction (xs, edges) (index + 1)


binarise :: PC.PopulationComputer String -> PC.PopulationComputer String
binarise pc = PC.PCB {
    PC.states = Set.fromList states,
    PC.delta = Set.fromList transitions,
    PC.input = Set.map (`buildStateName` 1) (PC.input pc),
    PC.output = buildOutputFunction (PC.output pc) 0,
    PC.helpers = MultiSet.map (`buildStateName` 1) $ PC.helpers pc
}
    where
        oldStates = Set.toList $ PC.states pc
        oldTransitions = Set.toList $ PC.delta pc
        multiplicity q = maximum $ map (\(input, _) -> MultiSet.occur q input) oldTransitions
        states = [buildStateName q i | q <- oldStates,
                                       i <- [0 .. multiplicity q]] ++                       -- stack
                 [buildTStates Q i t | t <- oldTransitions,
                                     let q = PC.getQFromTransition t,
                                     i <- [0 .. multiplicity q]] ++                         -- commit
                 [buildIdentifiedTStates t i | t <- oldTransitions,
                                               i <- [1 .. MultiSet.size (fst t)]]           -- execute
        stackTransitions = [(MultiSet.fromList [buildStateName q i, buildStateName q j], MultiSet.fromList [buildStateName q (i + j), buildStateName q 0]) |
                                q <- oldStates,
                                i <- [1 .. multiplicity q - 1],
                                j <- [1 .. multiplicity q - 1],
                                i + j <= multiplicity q] ++                                 -- stack
                           [(MultiSet.fromList [buildStateName q i, buildStateName q j], MultiSet.fromList [buildStateName q (multiplicity q), buildStateName q (i + j - multiplicity q)]) |
                                q <- oldStates,
                                i <- [1 .. multiplicity q - 1],
                                j <- [1 .. multiplicity q - 1],
                                i + j >= multiplicity q]                                    -- stack
        commitTransitions = [(MultiSet.fromList [buildStateName q i, buildStateName p j], MultiSet.fromList [buildTStates Q (i - numOfQ) t, buildStateName p (j - numOfP)]) |
                                t <- oldTransitions,
                                let q = PC.getQFromTransition t,
                                let p = PC.getPFromTransition t,
                                q /= p,
                                let numOfQ = MultiSet.occur q (fst t),
                                let numOfP = MultiSet.occur p (fst t),
                                i <- [0 .. multiplicity q],
                                i >= numOfQ,
                                j <- [0 .. multiplicity p],
                                j >= numOfP] ++                                             -- commit
                            [(MultiSet.fromList [buildStateName q i, buildStateName q j], MultiSet.fromList [buildTStates Q (i + j - numOfQ) t, buildStateName q 0]) |
                                t <- oldTransitions,
                                let q = PC.getQFromTransition t,
                                let p = PC.getPFromTransition t,
                                q == p,
                                let numOfQ = MultiSet.occur q (fst t),
                                i <- [0 .. multiplicity q],
                                j <- [0 .. multiplicity q],
                                i + j >= numOfQ,
                                i + j - numOfQ <= multiplicity q] ++                        -- commit
                            [(MultiSet.fromList [buildStateName q i, buildStateName q j], MultiSet.fromList [buildTStates Q (i + j - numOfQ - multiplicity q) t, buildStateName q (multiplicity q)]) |
                                t <- oldTransitions,
                                let q = PC.getQFromTransition t,
                                let p = PC.getPFromTransition t,
                                q == p,
                                let numOfQ = MultiSet.occur q (fst t),
                                i <- [0 .. multiplicity q],
                                j <- [0 .. multiplicity q],
                                i + j >= numOfQ,
                                i + j - numOfQ > multiplicity q]                            -- commit
        transferTransitions = [(MultiSet.fromList [buildTStates Q i t, buildStateName q 0], MultiSet.fromList [buildTStates Q 0 t, buildStateName q i]) |
                                t <- oldTransitions,
                                let q = PC.getQFromTransition t,
                                i <- [1 .. multiplicity q]]                                 -- transfer
        executeTransitions = [(MultiSet.fromList [buildIdentifiedTStates t i, buildStateName p 0], MultiSet.fromList [buildIdentifiedTStates t (i+1), buildSIdentifiedState (s !! (i - 1))]) |
                                t <- oldTransitions,
                                let q = PC.getQFromTransition t,
                                let p = PC.getPFromTransition t,
                                let s = MultiSet.toList (snd t),
                                i <- [1 .. length s - 1],
                                i <= MultiSet.occur p (fst t)] ++
                             [(MultiSet.fromList [buildIdentifiedTStates t i, buildStateName q 0], MultiSet.fromList [buildIdentifiedTStates t (i+1), buildSIdentifiedState (s !! (i - 1))]) |
                                t <- oldTransitions,
                                let q = PC.getQFromTransition t,
                                let p = PC.getPFromTransition t,
                                let s = MultiSet.toList (snd t),
                                i <- [1 .. length s - 1],
                                i > MultiSet.occur p (fst t)]                               -- execute
        transitions = filter (not . containsCommitTs) stackTransitions ++
                      commitTransitions ++
                      filter (not . containsCommitTs) transferTransitions ++
                      filter (not . containsCommitTs) executeTransitions
            where
                containsCommitTs = (`containsSameHelper` commitTransitions)
                containsSameHelper :: (MultiSet.MultiSet String, MultiSet.MultiSet String) -> [(MultiSet.MultiSet String, MultiSet.MultiSet String)] -> Bool
                containsSameHelper _ [] = False
                containsSameHelper t (x:xs) = (fst t == fst x) || containsSameHelper t xs
