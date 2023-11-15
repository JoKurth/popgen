{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}

module InputOutput.MapOutput
where

import Helper.List (mapWithIndex)
import qualified Types.PopulationComputer as PC

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet
import qualified Data.HashMap.Lazy as HashMap
import Data.List (foldl1')

-- do not use these functions if multiple (more than two) agents in the same state can participate in a transaction
getQFromTransition t = MultiSet.findMin $ fst t    -- todo auslagern
getPFromTransition t = MultiSet.findMax $ fst t
getQ'FromTransition t = MultiSet.findMin $ snd t    -- todo auslagern
getP'FromTransition t = MultiSet.findMax $ snd t


stringPcToIntPc :: PC.PopulationComputer String -> PC.PopulationComputer Int
stringPcToIntPc (PC.PCB states delta input output helper) = PC.PCB {
    PC.states  = Set.fromList [1 .. Set.size states],
    PC.delta   = Set.fromList mappedTransitions,
    PC.output  = mapOutput output,
    PC.input   = mappedInputs,
    PC.helpers = MultiSet.map (stateMapper HashMap.!) helper
}
    where
        stateMapper = HashMap.fromList $ mapWithIndex (\i x -> (x, i)) $ Set.toList states
        -- m = (stateMapper HashMap.!)
        mappedInputs = Set.map (stateMapper HashMap.!) input
        mappedTransitions = map mapTransition $ Set.toList delta
            where
                mapTransition t = (MultiSet.fromList [q, p], MultiSet.fromList [q', p'])
                    where
                        q = (stateMapper HashMap.!) $ getQFromTransition t
                        p = (stateMapper HashMap.!) $ getPFromTransition t
                        q' = (stateMapper HashMap.!) $ getQ'FromTransition t
                        p' = (stateMapper HashMap.!) $ getP'FromTransition t
        mapOutput :: PC.BooleanCircuit String -> PC.BooleanCircuit Int
        mapOutput ([], edges) = ([], edges)
        mapOutput ((PC.Input x):gates, edges) = (PC.Input (stateMapper HashMap.! x) : fst nextCircuit, snd nextCircuit)
            where
                nextCircuit = mapOutput (gates, edges)
        mapOutput (g:gates, edges) = (PC.stringGateToIntGate g : fst nextCircuit, snd nextCircuit)
            where
                nextCircuit = mapOutput (gates, edges)
stringPcToIntPc (PC.PCL states delta input output helper) = PC.PCL {
    PC.states   = Set.fromList [1 .. Set.size states],
    PC.delta    = Set.fromList mappedTransitions,
    PC.outputOL = mapOutput output,
    PC.input    = mappedInputs,
    PC.helpers  = MultiSet.map (stateMapper HashMap.!) helper
}
    where
        stateMapper = HashMap.fromList $ mapWithIndex (\i x -> (x, i)) $ Set.toList states
        -- m = (stateMapper HashMap.!)
        mappedInputs = Set.map (stateMapper HashMap.!) input
        mappedTransitions = map mapTransition $ Set.toList delta
            where
                mapTransition t = (MultiSet.fromList [q, p], MultiSet.fromList [q', p'])
                    where
                        q = (stateMapper HashMap.!) $ getQFromTransition t
                        p = (stateMapper HashMap.!) $ getPFromTransition t
                        q' = (stateMapper HashMap.!) $ getQ'FromTransition t
                        p' = (stateMapper HashMap.!) $ getP'FromTransition t
        mapOutput :: PC.OutputLists String -> PC.OutputLists Int
        mapOutput (PC.Output true false) = PC.Output {
            PC.true = map (stateMapper HashMap.!) true,
            PC.false = map (stateMapper HashMap.!) false
        }



popCompOverview :: PC.PopulationComputer a -> String
popCompOverview pc = "PComputer:\n" ++
                     show (length states) ++ " " ++ show (length input) ++ " " ++ show (length transitions) ++ " " ++ show (length helper)
    where
        states = PC.states pc
        transitions = PC.delta pc
        input = PC.input pc
        helper = PC.helpers pc


-- populationComputerToPopSim :: Show a => PC.PopulationComputer a -> String
populationComputerToPopSim pc = "PComputer:\n" ++
                                show (length states) ++ " " ++ show (length input) ++ " " ++ show (length transitions) ++ " " ++ show (length helper) ++ "\n\n" ++
                                foldl (\str x -> str ++ " " ++ show x ++ ":a_i") "" mappedInputs ++ "\n" ++
                                foldr1 (\str x ->  str ++ "\n" ++ x) mappedTransitions ++ "\n"
                                -- "Output:\n" ++
                                -- "    as Circuit:\n" ++
                                -- show outputBC ++ "\n"
                                -- "    as Lists:\n" ++
                                -- show outputOL
    where
        states = Set.toList $ PC.states pc
        transitions = Set.toList $ PC.delta pc
        input = Set.toList $ PC.input pc
        -- outputBC = PC.output pc
        outputOL = PC.outputOL pc
        helper = PC.helpers pc
        stateMapper = HashMap.fromList $ mapWithIndex (\i x -> (x, i)) states
        mappedInputs = map (stateMapper HashMap.!) input
        mappedTransitions = map mapTransition transitions
            where
                mapTransition t = show q ++ ":" ++ show p ++ " " ++ show q' ++ ":" ++ show p'
                    where
                        q = (stateMapper HashMap.!) $ getQFromTransition t
                        p = (stateMapper HashMap.!) $ getPFromTransition t
                        q' = (stateMapper HashMap.!) $ getQ'FromTransition t
                        p' = (stateMapper HashMap.!) $ getP'FromTransition t


populationProtocolToPopSim :: PC.PopulationProtocol String -> String
populationProtocolToPopSim pp = show (length states) ++ " " ++ show (length input) ++ " " ++ show (length transitions) ++ "\n" ++
                                foldl (\str x -> str ++ " " ++ show x ++ ":a_i") "" mappedInputs ++ "\n" ++
                                foldr1 (\str x ->  str ++ "\n" ++ x) mappedTransitions ++ "\n"
    where
        states = PC.statesPP pp
        transitions = PC.deltaPP pp
        input = Set.toList $ PC.inputPP pp
        output = PC.outputPP pp
        stateMapper = HashMap.fromList $ mapWithIndex (\i x -> (x, i)) states
        -- m = (stateMapper HashMap.!)
        mappedInputs = map (stateMapper HashMap.!) input
        mappedTransitions = map mapTransition transitions
            where
                mapTransition t = show q ++ ":" ++ show p ++ " " ++ show q' ++ ":" ++ show p'
                    where
                        q = (stateMapper HashMap.!) $ getQFromTransition t
                        p = (stateMapper HashMap.!) $ getPFromTransition t
                        q' = (stateMapper HashMap.!) $ getQ'FromTransition t
                        p' = (stateMapper HashMap.!) $ getP'FromTransition t
