module Transformer.Binarise (
    binarise
) where

import qualified Types.PopulationComputer as PC (PopulationComputer(..))
import Helper.List

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set

binarise :: PC.PopulationComputer String -> PC.PopulationComputer String
binarise pc = PC.PC {
    PC.states = Set.fromList states,
    PC.delta = Set.fromList transitions,
    PC.input = Set.map (`buildStateName` 1) (PC.input pc),
    -- PC.output = 
    PC.helpers = MultiSet.map (`buildStateName` 1) $ PC.helpers pc
}
    where
        renameTransitionsForStates t = "{(" ++ show (fst t) ++ "," ++ show (snd t) ++ ")}" -- maybe wie want to switch to a map for an easy and faster lookup
        buildStateName oldName number = "(" ++ oldName ++ "," ++ show number ++ ")"
        buildTStates oldName number transition = "(" ++ oldName ++ "," ++ show number ++ "," ++ renameTransitionsForStates transition ++ ")"
        buildIdentifiedTStates transition i l
            | i == 1 = buildTStates blub 0 transition  -- which state is meant here? the q state from the given transition
            | i == l = buildSIdentifiedState (MultiSet.toList (snd transition) !! l)
            | otherwise = "(" ++ renameTransitionsForStates transition ++ "," ++ show i ++ ")"
        buildSIdentifiedState state = buildStateName state 1
        m q = maxFromList $ map (\(input, _) -> MultiSet.occur q input) $ Set.toList $ PC.delta pc     -- the naming follows the paper; maybe rename to multiplicity or something similar
        states = [buildStateName q i | q <- Set.toList (PC.states pc),
                                       i <- [0 .. m q]] ++                          -- stack
                 [buildTStates q i t | q <- Set.toList (PC.states pc),
                                       i <- [0 .. m q],
                                       t <- Set.toList (PC.delta pc),
                                       MultiSet.member q (fst t)] ++                  -- commit
                 [buildIdentifiedTStates t i l | t <- Set.toList (PC.delta pc),
                                                 let l = MultiSet.size (snd t),
                                                 i <- [1 .. MultiSet.size (fst t)]] -- execute
        stackTransitions = [(MultiSet.fromList [buildStateName q i, buildStateName q j], MultiSet.fromList [buildStateName q (i + j), buildStateName q 0]) |
                                q <- Set.toList (PC.states pc),
                                i <- [0 .. m q - 1],
                                j <- [0 .. m q - 1],
                                i + j <= m q] ++                 -- stack
                           [(MultiSet.fromList [buildStateName q i, buildStateName q j], MultiSet.fromList [buildStateName q (m q), buildStateName q (i + j - m q)]) |
                                q <- Set.toList (PC.states pc),
                                i <- [0 .. m q - 1],
                                j <- [0 .. m q - 1],
                                i + j >= m q]       -- stack
        commitTransitions = [(MultiSet.fromList [buildStateName q i, buildStateName p j], MultiSet.fromList [buildTStates q (i - MultiSet.occur q (fst t)) t, buildTStates p (j - MultiSet.occur p (fst t)) t]) |
                                q <- Set.toList (PC.states pc),
                                p <- Set.toList (PC.states pc),
                                q /= p,
                                t <- Set.toList (PC.delta pc),
                                MultiSet.member q (fst t),
                                MultiSet.member p (fst t),
                                i <- [0 .. m q],
                                i >= MultiSet.occur q (fst t),
                                j <- [0 .. m q],
                                j >= MultiSet.occur p (fst t)] ++      -- commit
                                  -- commit
                            [(MultiSet.fromList [buildStateName q i, buildStateName q j], MultiSet.fromList [buildTStates q (i + j - MultiSet.occur q (fst t)) t, buildStateName q 0]) |
                                q <- Set.toList (PC.states pc),
                                t <- Set.toList (PC.delta pc),
                                MultiSet.member q (fst t),
                                MultiSet.distinctSize (fst t) == 1,
                                i <- [0 .. m q],
                                j <- [0 .. m q],
                                i + j >= MultiSet.occur q (fst t),
                                i + j - MultiSet.occur q (fst t) <= m q] ++                                                    -- commit
                            [(MultiSet.fromList [buildStateName q i, buildStateName q j], MultiSet.fromList [buildTStates q (i + j - MultiSet.occur q (fst t) - m q) t, buildStateName q (m q)]) |
                                q <- Set.toList (PC.states pc),
                                t <- Set.toList (PC.delta pc),
                                MultiSet.member q (fst t),
                                MultiSet.distinctSize (fst t) == 1,
                                i <- [0 .. m q],
                                j <- [0 .. m q],
                                i + j >= MultiSet.occur q (fst t),
                                i + j - MultiSet.occur q (fst t) > m q]                                           -- commit
        transferTransitions = [(MultiSet.fromList [buildTStates q i t, buildStateName q 0], MultiSet.fromList [buildTStates q 0 t, buildStateName q i]) |
                                q <- Set.toList (PC.states pc),
                                i <- [1 .. m q],
                                t <- Set.toList (PC.delta pc),
                                MultiSet.member q (fst t)]            -- transfer
        executeTransitions = [(MultiSet.fromList [buildIdentifiedTStates t i (length s), buildStateName p 0], MultiSet.fromList [buildIdentifiedTStates t (i+1) (length s), buildSIdentifiedState (s !! i)]) |
                                q <- Set.toList (PC.states pc),
                                p <- Set.toList (PC.states pc),
                                t <- Set.toList (PC.delta pc),
                                MultiSet.member q (fst t),
                                MultiSet.member p (fst t),
                                let s = MultiSet.toList (snd t),
                                i <- [1 .. length s - 1],
                                i <= MultiSet.occur p (fst t)] ++
                             [(MultiSet.fromList [buildIdentifiedTStates t i (length s), buildStateName q 0], MultiSet.fromList [buildIdentifiedTStates t (i+1) (length s), buildSIdentifiedState (s !! i)]) |
                                q <- Set.toList (PC.states pc),
                                p <- Set.toList (PC.states pc),
                                t <- Set.toList (PC.delta pc),
                                MultiSet.member q (fst t),
                                MultiSet.member p (fst t),
                                let s = MultiSet.toList (snd t),
                                i <- [1 .. length s - 1],
                                i > MultiSet.occur p (fst t)]           -- execute
        transitions = filter (not . containsSameR) stackTransitions ++ commitTransitions ++ filter (not . containsSameR) transferTransitions ++ filter (not . containsSameR) executeTransitions
            where
                -- this is a very inefficient way to do this. maybe we have to optimize it
                containsSameR = (`containsSameRHelper` commitTransitions)
                containsSameRHelper :: (MultiSet.MultiSet String, MultiSet.MultiSet String) -> [(MultiSet.MultiSet String, MultiSet.MultiSet String)] -> Bool
                containsSameRHelper _ [] = False
                containsSameRHelper t (x:xs) = (fst t == fst x) || containsSameRHelper t xs
