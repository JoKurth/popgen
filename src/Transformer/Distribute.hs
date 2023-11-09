module Transformer.Distribute (
    distribute
) where

import Helper.List
import qualified Types.PopulationComputer as PC (PopulationComputer(..), PopulationProtocol(..), OutputLists (..))

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set


-- das ist sehr langsam. lief jetzt über 1,5 stunden und war immer noch nicht fertig.
-- optimierungen: keine konvertierung in sets. stattdessen die listen zurückgeben. durch die umbenennung sollten doppelte dann auch sowieso wegfallen
--                das filtern umschreiben (die variante mit den hashmaps und dann alle auf einmal filtern) => bringt das runter von > 1,5 std auf 2 min 10 bis 20 sek

-- wie gehts weiter: memory-usage einsparen. nach jeder (erfolgreichen) optimierung commiten (jetzt auch) und kurz notieren, dass es besser läuft (screenshot)
-- potentielle weitere optimierung: strictness -> googlen


getQFromTransition t = MultiSet.findMin $ fst t    -- todo auslagern
getPFromTransition t = MultiSet.findMax $ fst t
getQ'FromTransition t = MultiSet.findMin $ snd t    -- todo auslagern
getP'FromTransition t = MultiSet.findMax $ snd t

-- inefficient!!
getTransitionWithQAndP q p [] = (q, p)
getTransitionWithQAndP q p (t:ts) = if MultiSet.member q (fst t) && MultiSet.member p (fst t)
                                        then (getQ'FromTransition t, getP'FromTransition t)
                                        else getTransitionWithQAndP q p ts

buildState q i t = "(" ++ q ++ "," ++ show i ++ "," ++ show t ++ ")"


distribute :: PC.PopulationComputer String-> PC.PopulationProtocol String
distribute pc = PC.PP {
    PC.statesPP = Set.fromList states,
    PC.deltaPP = Set.fromList transitions,
    PC.inputPP = Set.map (\q -> buildState q 0 0) (PC.input pc),
    PC.outputPP = output
}
    where
        states = [buildState q opinion token | q <- Set.toList (PC.states pc), opinion <- [0, 1], token <- [0, 1]]
        oldTransitions = Set.toList (PC.delta pc)
        transitionsForBuilding = [((q, p), (q', p')) | q <- Set.toList (PC.states pc), p <- Set.toList (PC.states pc), let t = getTransitionWithQAndP q p oldTransitions, let q' = fst t, let p' = snd t]
        certifyTransitions = [(MultiSet.fromList [buildState q i1 t1, buildState p i2 t2], MultiSet.fromList [buildState q' i 1, buildState p' i 1]) |
                                t <- transitionsForBuilding,
                                let q = fst $ fst t,
                                let p = snd $ fst t,
                                let q' = fst $ snd t,
                                let p' = snd $ snd t,
                                q' `notElem` PC.true (PC.outputOL pc),
                                q' `notElem` PC.false (PC.outputOL pc),
                                p' `notElem` PC.true (PC.outputOL pc),
                                p' `notElem` PC.false (PC.outputOL pc),
                                i1 <- [0, 1],
                                t1 <- [0, 1],
                                i2 <- [0, 1],
                                t2 <- [0, 1],
                                i <- [0, 1]]
        convinceTransitions = [(MultiSet.fromList [buildState q i 1, buildState p (1 - i) 0], MultiSet.fromList [buildState q' i 0, buildState p' i 0]) |
                                t <- transitionsForBuilding,
                                let q = fst $ fst t,
                                let p = snd $ fst t,
                                let q' = fst $ snd t,
                                let p' = snd $ snd t,
                                i <- [0, 1]]
        dropTransitions = [(MultiSet.fromList [buildState q i 1, buildState p (1 - i) 1], MultiSet.fromList [buildState q' i 0, buildState p' (1 - i) 0]) |
                                t <- transitionsForBuilding,
                                let q = fst $ fst t,
                                let p = snd $ fst t,
                                let q' = fst $ snd t,
                                let p' = snd $ snd t,
                                i <- [0, 1]]
        noopTransitions = [(MultiSet.fromList [buildState q i1 t1, buildState p i2 t2], MultiSet.fromList [buildState q' i1 t1, buildState p' i2 t2]) |
                                t <- transitionsForBuilding,
                                let q = fst $ fst t,
                                let p = snd $ fst t,
                                let q' = fst $ snd t,
                                let p' = snd $ snd t,
                                q /= q',
                                p /= p',
                                i1 <- [0, 1],
                                t1 <- [0, 1],
                                i2 <- [0, 1],
                                t2 <- [0, 1]]
        transitions = certifyTransitions ++
                      filterFromOtherList convinceTransitions certifyTransitions ++
                      filterFromOtherList dropTransitions certifyTransitions ++
                      filterFromOtherList (filterFromOtherList (filterFromOtherList noopTransitions certifyTransitions) convinceTransitions) dropTransitions
        output = PC.Output {
            PC.true = [buildState q 1 token | q <- Set.toList (PC.states pc), token <- [0, 1]],
            PC.false = [buildState q 0 token | q <- Set.toList (PC.states pc), token <- [0, 1]]
        }
