module Transformer.Focalise (
    focalise
) where

import qualified Types.PopulationComputer as PC (PopulationComputer(..), Gate(..), Edge, OutputLists(..), isInputGate, evalGate)

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import Data.List (sortBy)


flags = ["-", "+"]
indicators = ["0", "1", "!"]
boolVals = ["0", "1", "undef"]


getQFromTransition t = MultiSet.findMin $ fst t    -- todo auslagern
getPFromTransition t = MultiSet.findMax $ fst t
getQ'FromTransition t = MultiSet.findMin $ snd t    -- todo auslagern
getP'FromTransition t = MultiSet.findMax $ snd t


gateToGateStateName g e = "g(" ++ show g ++ "," ++ show e ++ ")"


buildOrigState oldState flag = "(" ++ oldState ++ "," ++ flag ++ ")"
buildSuppState oldState indicator = "(" ++ oldState ++ "," ++ indicator ++ ")"
buildGateState gate val1 val2 val3 = "(" ++ gate ++ "," ++ val1 ++ "," ++ val2 ++ "," ++ val3 ++ ")"
trueState = buildGateState "True" "1" "1" "1"
falseState = buildGateState "False" "0" "0" "0"

-- | the second parameter is the index of the gate. it is used to find its corresponding edge
buildGateTransitionQState :: PC.Gate String -> Int -> [PC.Edge] -> Int -> [String]
buildGateTransitionQState (PC.Input "0") _ _ _ = [falseState] -- the value of this state does not matter
buildGateTransitionQState (PC.Input q) _ _ b = [buildSuppState q (show b)]
buildGateTransitionQState PC.ConstT _ _ _ = [trueState]
buildGateTransitionQState PC.ConstF _ _ _ = [falseState]
buildGateTransitionQState g index edges b = [buildGateState (gateToGateStateName g (findE index edges)) (show b) i j | i <- ["0", "1"], j <- ["0", "1"]] -- we could probably optimize a lot here
    where
        findE i [] = error "Error during creation of gate transitions: could not find given edge."
        findE i (e:edges)                                                                                                  -- inefficient
            | i == fst e = e
            | otherwise = findE i edges
        inputs = case g of
                    PC.AND -> if b == 0 then [("0", "0"), ("0", "1"), ("1", "0")] else [("1", "1")]
                    PC.OR -> if b == 0 then [("0", "0")] else [("0", "1"), ("1", "0"), ("1", "1")]
                    PC.NOT -> if b == 0 then [("1", "0")] else [("0", "0")]



sortEdges :: PC.Edge -> PC.Edge -> Ordering
sortEdges e1 e2
    | fst e1 > fst e2 = GT
    | fst e1 < fst e2 = LT
    | otherwise       = EQ


focalise :: PC.PopulationComputer String -> PC.PopulationComputer String
focalise pc = PC.PCL {
    PC.states = Set.fromList states,
    PC.delta = Set.fromList transitions,
    PC.input = Set.map (`buildOrigState` "-") (PC.input pc),
    PC.outputOL = outputFunction,
    PC.helpers = MultiSet.unions [MultiSet.map (`buildOrigState` "-") $ PC.helpers pc,
                                    MultiSet.fromList [buildSuppState q "0" | q <- oldStates],
                                    MultiSet.fromList [buildGateState (gateToGateStateName g e) "undef" "undef" "undef" | e <- edges, let g = gates !! fst e],
                                    MultiSet.fromList [trueState, falseState],
                                    MultiSet.fromList ["0"]]
}
    where
        oldStates = Set.toList $ PC.states pc
        gates = fst $ {- booleanCircuitToDot $ -} PC.output pc
        edges = snd $ PC.output pc
        boolGates = filter (not . PC.isInputGate) gates
        sortedEdges = sortBy sortEdges edges
        qOrig = [buildOrigState oldState flag | oldState <- oldStates, flag <- flags]
        qSupp = [buildSuppState oldState indicator | oldState <- oldStates, indicator <- indicators]
        qGate = [buildGateState (gateToGateStateName g e) val1 val2 val3 | e <- edges, let g = gates !! fst e, val1 <- boolVals, val2 <- boolVals, val3 <- boolVals] ++ [trueState, falseState] -- they are use to modell input-states (i.e. states without edges). thus we have to add them manually
        qReset = [show i | i <- [0 .. length oldStates + length boolGates]]
        states = qOrig ++ qSupp ++ qGate ++ qReset
        executeTransitions = [(MultiSet.fromList [buildOrigState q flag1, buildOrigState p flag2], MultiSet.fromList [buildOrigState q' "+", buildOrigState p' "-"]) |
                                    t <- Set.toList (PC.delta pc),
                                    let q = getQFromTransition t,
                                    let p = getPFromTransition t,
                                    let q' = getQ'FromTransition t,
                                    let p' = getP'FromTransition t,
                                    flag1 <- flags,
                                    flag2 <- flags]                        -- execute
        denotifyTransitions = [(MultiSet.fromList [buildOrigState q "+", buildOrigState p "+"], MultiSet.fromList [buildOrigState q "+", buildOrigState p "-"]) |
                                    q <- oldStates,
                                    p <- oldStates]              -- denotify
        detectTransitions = [(MultiSet.fromList [buildSuppState q "0", buildOrigState q "-"], MultiSet.fromList [buildSuppState q "!", buildOrigState q "-"]) |
                                q <- oldStates]              -- detect
        gateTransitions = [(MultiSet.fromList [buildGateState (gateToGateStateName g e) "undef" "undef" "undef", q], MultiSet.fromList [buildGateState (gateToGateStateName g e) "undef" (show b) "undef", q]) |
                                e <- edges, let g = gates !! fst e,
                                let e1 = gates !! fst (snd e),
                                b <- [0, 1],
                                q <- buildGateTransitionQState e1 (fst $ snd e) edges b] ++               -- gate
                          [(MultiSet.fromList [buildGateState (gateToGateStateName g e) "undef" (show i) "undef", q], MultiSet.fromList [buildGateState (gateToGateStateName g e) (show $ PC.evalGate g i b) (show i) (show b), q]) |
                                e <- edges, let g = gates !! fst e,
                                let e2 = gates !! snd (snd e),
                                i <- [0, 1],
                                b <- case e2 of
                                        (PC.Input "0") -> [0]
                                        PC.ConstT -> [1]
                                        PC.ConstF -> [0]
                                        _ -> [0, 1],
                                q <- buildGateTransitionQState e2 (snd $ snd e) edges b]               -- gate
        resetTransitions = [(MultiSet.fromList [show i, buildSuppState qi b], MultiSet.fromList [show (i + 1), buildSuppState qi "0"]) |
                                i <- [0 .. length oldStates - 1],
                                let qi = oldStates !! i,
                                b <- indicators] ++             -- reset     -- devitaiont: i starts at 0 instead of 1, because it is easier for the indices (because out indices start at 0 instead of 1 as in the paper). therefore we use i+1 later
                            [(MultiSet.fromList [show (length oldStates + i), buildGateState (gateToGateStateName g e) b1 b2 b3], MultiSet.fromList [show (length oldStates + i + 1), buildGateState (gateToGateStateName g e) "undef" "undef" "undef"]) |
                                i <- [0 .. length boolGates - 1],
                                let e = sortedEdges !! i, let g = gates !! fst e,
                                b1 <- boolVals,
                                b2 <- boolVals,
                                b3 <- boolVals]             -- reset
        initResetTransitions = [(MultiSet.fromList [buildOrigState q "+", i], MultiSet.fromList [buildOrigState q "-", "0"]) |
                                    q <- oldStates, i <- qReset] ++             -- init-reset
                                [(MultiSet.fromList [buildSuppState q "!", i], MultiSet.fromList [buildOrigState q "1", show $ min (read i) (length oldStates)]) |
                                    q <- oldStates, i <- qReset] ++             -- init-reset
                                [(MultiSet.fromList [i, j], MultiSet.fromList ["0", buildOrigState qh "-"]) |
                                    i <- qReset, j <- qReset]              -- init-reset
                                        where
                                            qh = head $ MultiSet.toList $ PC.helpers pc
        leaderTransitions = [(MultiSet.fromList [buildSuppState q i1, buildSuppState q i2], MultiSet.fromList [buildSuppState q i1, "0"]) |
                                q <- oldStates, i1 <- indicators, i2 <- indicators] ++             -- leader
                            [(MultiSet.fromList [buildGateState (gateToGateStateName g e) v11 v12 v13, buildGateState (gateToGateStateName g e) v21 v22 v23], MultiSet.fromList [buildGateState (gateToGateStateName g e) v11 v12 v13, "0"]) |
                                e <- edges, let g = gates !! fst e,
                                v11 <- boolVals,
                                v12 <- boolVals,
                                v13 <- boolVals,
                                v21 <- boolVals,
                                v22 <- boolVals,
                                v23 <- boolVals]                -- leader
        transitions = executeTransitions ++
                      filter (\t -> not $ containsSameAs t executeTransitions) denotifyTransitions ++
                      detectTransitions ++
                      gateTransitions ++
                      resetTransitions ++
                      initResetTransitions ++
                      leaderTransitions
            where
                -- this is a very inefficient way to do this. maybe we have to optimize it
                containsSameAs :: (MultiSet.MultiSet String, MultiSet.MultiSet String) -> [(MultiSet.MultiSet String, MultiSet.MultiSet String)] -> Bool
                containsSameAs _ [] = False
                containsSameAs t (x:xs) = fst t == fst x || containsSameAs t xs
        outputFunction = PC.Output {
            PC.true = [buildGateState outputGate "1" i j | i <- ["0", "1"], j <- ["0", "1"]], -- deviation: Q0 is defined with 1 in as the second element in the paper
            PC.false = [buildGateState outputGate "0" i j | i <- ["0", "1"], j <- ["0", "1"]]
        }
            where
                outputEdge = last edges
                outputGate = gateToGateStateName (gates !! fst outputEdge) outputEdge
