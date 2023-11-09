module Transformer.Focalise (
    focalise
) where

import qualified Types.PopulationComputer as PC (PopulationComputer(..), Gate(..), Edge, OutputLists(..), isInputGate, evalGate)

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import Data.List (sortBy)


flags = ["-", "+"]
boolVals = ["0", "1", "undef"]


getQFromTransition t = MultiSet.findMin $ fst t    -- todo auslagern
getPFromTransition t = MultiSet.findMax $ fst t
getQ'FromTransition t = MultiSet.findMin $ snd t    -- todo auslagern
getP'FromTransition t = MultiSet.findMax $ snd t


gateToGateStateName g e = "g(" ++ show g ++ "," ++ show e ++ ")"


buildOrigState oldState flag = "(" ++ oldState ++ "," ++ flag ++ ")"
buildSuppState oldState indicator = "(" ++ oldState ++ "," ++ indicator ++ ")"
buildGateState gate val1 val2 val3 = "(" ++ gate ++ "," ++ val1 ++ "," ++ val2 ++ "," ++ val3 ++ ")"

buildGateTransitionQState :: PC.Gate String -> PC.Edge -> Int -> [String]
buildGateTransitionQState (PC.Input "0") _ _ = [buildGateState "False" "0" "0" "0"]
buildGateTransitionQState (PC.Input q) _ b = [buildSuppState q (show b)]
buildGateTransitionQState PC.ConstT _ _ = [buildGateState "True" "1" "1" "1"]       -- const gates are considered as input-gates but how do i handle them actually
buildGateTransitionQState PC.ConstF _ _ = [buildGateState "False" "0" "0" "0"]
buildGateTransitionQState g e b = [buildGateState (gateToGateStateName g e) (show b) i j | i <- ["0", "1"], j <- ["0", "1"]]


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
    PC.helpers = MultiSet.unions [MultiSet.map (`buildOrigState` "-") $ PC.helpers pc, MultiSet.fromList [buildSuppState q "0" | q <- Set.toList $ PC.states pc],
                                    MultiSet.fromList [buildGateState (gateToGateStateName g e) "undef" "undef" "undef" | e <- edges, let g = gates !! fst e],
                                    MultiSet.fromList ["0"]]
}
    where
        oldStates = Set.toList $ PC.states pc
        gates = fst $ PC.output pc
        edges = snd $ PC.output pc
        sortedEdges = sortBy sortEdges edges
        qOrig = [buildOrigState oldState flag | oldState <- oldStates, flag <- flags]
        qSupp = [buildSuppState oldState indicator | oldState <- oldStates, indicator <- ["0", "1", "!"]]
        qGate = [buildGateState (gateToGateStateName g e) val1 val2 val3 | e <- edges, let g = gates !! fst e, val1 <- boolVals, val2 <- boolVals, val3 <- boolVals] ++ [buildGateState "True" "1" "1" "1", buildGateState "False" "0" "0" "0"] -- they are use to modell input-states (i.e. states without edges). thus we have to add them manually
        qReset = [show i | i <- [0 .. Set.size (PC.states pc) + length (filter (not . PC.isInputGate) gates)]]
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
                                e <- edges,
                                let g = gates !! fst e,
                                let e1 = gates !! fst (snd e),
                                b <- [0, 1],
                                q <- buildGateTransitionQState e1 e b] ++               -- gate
                          [(MultiSet.fromList [buildGateState (gateToGateStateName g e) "undef" (show i) "undef", q], MultiSet.fromList [buildGateState (gateToGateStateName g e) (show $ PC.evalGate g i b) (show i) (show b), q]) |
                                e <- edges,
                                let g = gates !! fst e,
                                let e2 = gates !! snd (snd e),
                                i <- [0, 1],
                                b <- [0, 1],
                                q <- buildGateTransitionQState e2 e b]               -- gate
        resetTransitions = [(MultiSet.fromList [show i, buildSuppState qi (show b)], MultiSet.fromList [show (i + 1), buildSuppState qi "0"]) |
                                i <- [0 .. length qSupp - 1], let qi = qSupp !! i, b <- [0, 1]] ++             -- reset     -- devitaiont: i starts at 0 instead of 1, because it is easier for the indices (because out indices start at 0 instead of 1 as in the paper). therefore we use i+1 later
                            [(MultiSet.fromList [show (Set.size (PC.states pc) + i), buildGateState (gateToGateStateName g e) b1 b2 b3], MultiSet.fromList [show (Set.size (PC.states pc) + i + 1), buildGateState (gateToGateStateName g e) "undef" "undef" "undef"]) |
                            --  we have to substract 3 because: 1 as the difference between length and max index; and another 2 for the two extra const gate states; div by 27 because each gate is mapped to 27 states. this is very hacky
                                i <- [0 .. length qGate - 3], let e = sortedEdges !! (i `div` 27), let g = gates !! fst e, b1 <- boolVals, b2 <- boolVals, b3 <- boolVals]             -- reset        -- optimizaiton: iterate smarter over i and e; deviation: above
        initResetTransitions = [(MultiSet.fromList [buildOrigState q "+", i], MultiSet.fromList [buildOrigState q "-", "0"]) |
                                    q <- oldStates, i <- qReset] ++             -- init-reset
                                [(MultiSet.fromList [buildOrigState q "!", i], MultiSet.fromList [buildOrigState q "1", show $ min (read i) (length $ PC.states pc)]) |
                                    q <- oldStates, i <- qReset] ++             -- init-reset
                                [(MultiSet.fromList [i, j], MultiSet.fromList ["0", buildOrigState qh "-"]) |
                                    i <- qReset, j <- qReset]              -- init-reset
                                        where
                                            qh = head $ MultiSet.toList $ PC.helpers pc
        leaderTransitions = [(MultiSet.fromList [buildSuppState q i1, buildSuppState q i2], MultiSet.fromList [buildSuppState q i1, "0"]) |
                                q <- oldStates, i1 <- ["0", "1", "!"], i2 <- ["0", "1", "!"]] ++             -- leader
                            [(MultiSet.fromList [buildGateState (gateToGateStateName g e) v11 v12 v13, buildGateState (gateToGateStateName g e) v21 v22 v23], MultiSet.fromList [buildGateState (gateToGateStateName g e) v11 v12 v13, "0"]) |
                                e <- edges, let g = gates !! fst e, v11 <- boolVals, v12 <- boolVals, v13 <- boolVals, v21 <- boolVals, v22 <- boolVals, v23 <- boolVals]                -- leader
        transitions = executeTransitions ++ filter (\t -> not $ containsSameAs t executeTransitions) denotifyTransitions ++ detectTransitions ++ gateTransitions ++ resetTransitions ++ initResetTransitions ++ leaderTransitions
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
        -- for \exists q0 and \exists q1 we output false eventhough it is specified as undefined
        -- outputFunction = ([PC.Input 0, PC.Input q01, PC.Input q02, PC.Input q03, PC.Input q04, PC.OR, PC.OR, PC.OR, PC.NOT, PC.Input q11, PC.Input q12, PC.Input q13,PC.Input q14, PC.OR, PC.OR, PC.OR, PC.AND],
        --                   [(5, (1, 2)), (6, (5, 3)), (7, (6, 4)), (8, (7, 0)), (13, (9, 10)), (14, (13, 11)), (15, (14, 12)), (16, (8, 15))])
        --                         where
        --                             outputEdge = last edges
        --                             outputGate = gateToGateStateName (gates !! fst outputEdge) outputEdge
        --                             q01 = buildGateState outputGate 0 0 0
        --                             q02 = buildGateState outputGate 0 0 1
        --                             q03 = buildGateState outputGate 0 1 0
        --                             q04 = buildGateState outputGate 0 1 1
        --                             q11 = buildGateState outputGate 1 0 0
        --                             q12 = buildGateState outputGate 1 0 1
        --                             q13 = buildGateState outputGate 1 1 0
        --                             q14 = buildGateState outputGate 1 1 1
