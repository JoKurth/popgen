module Helper.Debug
where

import qualified Types.PopulationComputer as PC

import Data.Set as Set
import Data.MultiSet as MultiSet

import Debug.Trace


transitionsToDot :: Show a => Set.Set (MultiSet.MultiSet a, MultiSet.MultiSet a) -> Set.Set (MultiSet.MultiSet a, MultiSet.MultiSet a)
transitionsToDot transitions = trace (mapTransitionsToDotFormat transitions) transitions
    where
        transitionsToDot [] = ""
        transitionsToDot ((left, right):transitions) = "\t\"" ++ show left ++ "\" -> \"" ++ show right ++ "\";\n" ++ transitionsToDot transitions
        mapTransitionsToDotFormat transitions = "\n------------------------------------------\n" ++
                                                    "digraph Transitions\n{\n" ++ transitionsToDot (Set.toList transitions) ++ "}" ++
                                                    "\n------------------------------------------\n"

transitionsToDotList :: Show a => [(MultiSet.MultiSet a, MultiSet.MultiSet a)] -> [(MultiSet.MultiSet a, MultiSet.MultiSet a)]
transitionsToDotList transitions = trace (mapTransitionsToDotFormat transitions) transitions
    where
        transitionsToDot [] = ""
        transitionsToDot ((left, right):transitions) = "\t\"" ++ show left ++ "\" -> \"" ++ show right ++ "\";\n" ++ transitionsToDot transitions
        mapTransitionsToDotFormat transitions = "\n------------------------------------------\n" ++
                                                    "digraph Transitions\n{\n" ++ transitionsToDot transitions ++ "}" ++
                                                    "\n------------------------------------------\n"


booleanCircuitToDot :: Show a => PC.BooleanCircuit a -> PC.BooleanCircuit a
booleanCircuitToDot bc = trace (mapBcToDotFormat bc) bc
    where
        edgesToDot (_, []) = ""
        edgesToDot (gates, e:edges) = "\t" ++ showGate gateIndex ++ " -> " ++ showGate leftGateIndex ++ ";\n\t" ++ showGate gateIndex ++ " -> " ++ showGate rightGateIndex ++ ";\n" ++ edgesToDot (gates, edges)
            where
                gateIndex = fst e
                leftGateIndex = fst $ snd e
                rightGateIndex = snd $ snd e
                showGate ex = "\"" ++ show (gates !! ex) ++ "_" ++ show ex ++ "\""
        mapBcToDotFormat bc = "\n------------------------------------------\n" ++
                                    "digraph BooleanCircuit\n{\n" ++ edgesToDot bc ++ "}" ++
                                    "\n------------------------------------------\n"


traceLength desc list = trace (desc ++ " " ++ show (length list)) list
