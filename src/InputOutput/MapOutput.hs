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
import Data.Maybe (isJust, isNothing)


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
        mappedInputs = Set.map (stateMapper HashMap.!) input
        mappedTransitions = map mapTransition $ Set.toList delta
            where
                mapTransition t = (MultiSet.fromList [q, p], MultiSet.fromList [q', p'])
                    where
                        q = (stateMapper HashMap.!) $ PC.getQFromTransition t
                        p = (stateMapper HashMap.!) $ PC.getPFromTransition t
                        q' = (stateMapper HashMap.!) $ PC.getQ'FromTransition t
                        p' = (stateMapper HashMap.!) $ PC.getP'FromTransition t
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
        mappedInputs = Set.map (stateMapper HashMap.!) input
        mappedTransitions = map mapTransition $ Set.toList delta
            where
                mapTransition t = (MultiSet.fromList [q, p], MultiSet.fromList [q', p'])
                    where
                        q = (stateMapper HashMap.!) $ PC.getQFromTransition t
                        p = (stateMapper HashMap.!) $ PC.getPFromTransition t
                        q' = (stateMapper HashMap.!) $ PC.getQ'FromTransition t
                        p' = (stateMapper HashMap.!) $ PC.getP'FromTransition t
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


populationComputerToPopSimBc :: PC.PopulationComputer String -> Maybe [Int] -> (String, PC.BooleanCircuit Int, HashMap.HashMap String Int)
populationComputerToPopSimBc pc vars = (show (length states) ++ " " ++ show (length input + MultiSet.distinctSize helper) ++ " " ++ show (length transitions) ++ "\n" ++
                                 foldl1 (\str x -> str ++ " " ++ x) (map (\(x, val) -> show x ++ val) mappedInputs) ++ (if MultiSet.distinctSize helper > 0 then foldl (\str (x, n) -> str ++ " " ++ show x ++ ":" ++ show n) "" mappedHelper else "") ++ "\n" ++
                                 foldr1 (\str x ->  str ++ "\n" ++ x) mappedTransitions ++ "\n",
                                 mapOutput $ PC.output pc,
                                 stateMapper)
    where
        states = Set.toList $ PC.states pc
        transitions = Set.toList $ PC.delta pc
        input = Set.toList $ PC.input pc
        helper = PC.helpers pc
        stateMapper = HashMap.fromList $ mapWithIndex (\i x -> (x, i)) states
        mappedVars = case vars of
                        Just x -> map Just x
                        Nothing -> [Nothing | i <- [1 .. length input]]
        mappedInputs = zipWith zipper input mappedVars
        zipper i (Just v) = (stateMapper HashMap.! i, ":" ++ show v)
        zipper i Nothing = (stateMapper HashMap.! i, ":a_i")
        mappedHelper = map (\(x, n) -> (stateMapper HashMap.! x, n)) $ MultiSet.toOccurList helper
        mappedTransitions = map mapTransition transitions
            where
                mapTransition t = show q ++ ":" ++ show p ++ " " ++ show q' ++ ":" ++ show p'
                    where
                        q = (stateMapper HashMap.!) $ PC.getQFromTransition t
                        p = (stateMapper HashMap.!) $ PC.getPFromTransition t
                        q' = (stateMapper HashMap.!) $ PC.getQ'FromTransition t
                        p' = (stateMapper HashMap.!) $ PC.getP'FromTransition t
        mapOutput output = (mapGates $ fst output, snd output)
            where
                mapGate (PC.Input x) = PC.Input $ stateMapper HashMap.! x
                mapGate g = PC.stringGateToIntGate g
                mapGates [] = []
                mapGates [g] = [mapGate g]
                mapGates (g:gates) = mapGate g : mapGates gates


populationComputerToPopSim :: PC.PopulationComputer String -> Maybe [Int] -> (String, PC.OutputLists Int, HashMap.HashMap String Int)
populationComputerToPopSim pc vars = (show (length states) ++ " " ++ show (length input + MultiSet.distinctSize helper) ++ " " ++ show (length transitions) ++ "\n" ++
                                 foldl1 (\str x -> str ++ " " ++ x) (map (\(x, val) -> (show x) ++ val) mappedInputs) ++ (if MultiSet.distinctSize helper > 0 then foldl (\str (x, n) -> str ++ " " ++ show x ++ ":" ++ show n) "" mappedHelper else "") ++ "\n" ++
                                 foldr1 (\str x ->  str ++ "\n" ++ x) mappedTransitions ++ "\n",
                                 mapOutput pc,
                                 stateMapper)
    where
        states = Set.toList $ PC.states pc
        transitions = Set.toList $ PC.delta pc
        input = Set.toList $ PC.input pc
        helper = PC.helpers pc
        stateMapper = HashMap.fromList $ mapWithIndex (\i x -> (x, i)) states
        mappedVars = case vars of
                        Just x -> map Just x
                        Nothing -> [Nothing | i <- [1 .. length input]]
        mappedInputs = zipWith zipper input mappedVars
        zipper i (Just v) = (stateMapper HashMap.! i, ":" ++ show v)
        zipper i Nothing = (stateMapper HashMap.! i, ":a_i")
        mappedHelper = map (\(x, n) -> (stateMapper HashMap.! x, n)) $ MultiSet.toOccurList helper
        mappedTransitions = map mapTransition transitions
            where
                mapTransition t = show q ++ ":" ++ show p ++ " " ++ show q' ++ ":" ++ show p'
                    where
                        q = (stateMapper HashMap.!) $ PC.getQFromTransition t
                        p = (stateMapper HashMap.!) $ PC.getPFromTransition t
                        q' = (stateMapper HashMap.!) $ PC.getQ'FromTransition t
                        p' = (stateMapper HashMap.!) $ PC.getP'FromTransition t
        mapOutput (PC.PCL {PC.outputOL = (PC.Output true false)}) = PC.Output {
                                                                        PC.true = map (stateMapper HashMap.!) true,
                                                                        PC.false = map (stateMapper HashMap.!) false
                                                                    }


populationProtocolToPopSim :: PC.PopulationProtocol Int -> Maybe [Int] -> (String, PC.OutputLists Int)
populationProtocolToPopSim pp vars = (show (length states) ++ " " ++ show (length input) ++ " " ++ show (length transitions) ++ "\n" ++
                                     foldl1 (\str x -> str ++ " " ++ x) (map (\(x, val) -> (show x) ++ val) mappedInputs) ++ "\n" ++
                                     foldr1 (\str x ->  str ++ "\n" ++ x) mappedTransitions ++ "\n",
                                     mappedOutput)
    where
        states = PC.statesPP pp
        transitions = PC.deltaPP pp
        input = Set.toList $ PC.inputPP pp
        output = PC.outputPP pp
        stateMapper = HashMap.fromList $ mapWithIndex (\i x -> (x, i)) states
        mappedVars = case vars of
                        Just x -> map Just x
                        Nothing -> [Nothing | i <- [1 .. length input]]
        mappedInputs = zipWith zipper input mappedVars
        zipper i (Just v) = (stateMapper HashMap.! i, ":" ++ show v)
        zipper i Nothing = (stateMapper HashMap.! i, ":a_i")
        mappedTransitions = map mapTransition transitions
            where
                mapTransition t = show q ++ ":" ++ show p ++ " " ++ show q' ++ ":" ++ show p'
                    where
                        q = (stateMapper HashMap.!) $ PC.getQFromTransition t
                        p = (stateMapper HashMap.!) $ PC.getPFromTransition t
                        q' = (stateMapper HashMap.!) $ PC.getQ'FromTransition t
                        p' = (stateMapper HashMap.!) $ PC.getP'FromTransition t
        mappedOutput = mapOutput output
        mapOutput :: PC.OutputLists Int -> PC.OutputLists Int
        mapOutput (PC.Output true false) = PC.Output {
            PC.true = map (stateMapper HashMap.!) true,
            PC.false = map (stateMapper HashMap.!) false
        }
