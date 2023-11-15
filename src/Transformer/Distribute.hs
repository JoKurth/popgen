{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}

module Transformer.Distribute (
    distribute
) where

import Helper.List
import qualified Types.PopulationComputer as PC (PopulationComputer(..), PopulationProtocol(..), OutputLists (..))

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap


-- das ist sehr langsam. lief jetzt über 1,5 stunden und war immer noch nicht fertig.
-- optimierungen: keine konvertierung in sets. stattdessen die listen zurückgeben. durch die umbenennung sollten doppelte dann auch sowieso wegfallen
--                das filtern umschreiben (die variante mit den hashmaps und dann alle auf einmal filtern) => bringt das runter von > 1,5 std auf 2 min 10 bis 20 sek

-- wie gehts weiter: memory-usage einsparen. nach jeder (erfolgreichen) optimierung commiten (jetzt auch) und kurz notieren, dass es besser läuft (screenshot)
-- potentielle weitere optimierung: strictness -> googlen


getQFromTransition t = MultiSet.findMin $ fst t    -- todo auslagern
getPFromTransition t = MultiSet.findMax $ fst t
getQ'FromTransition t = MultiSet.findMin $ snd t    -- todo auslagern
getP'FromTransition t = MultiSet.findMax $ snd t

buildState q i t = "(" ++ q ++ "," ++ show i ++ "," ++ show t ++ ")"


buildTransitionsForBuilding :: [String] -> [(MultiSet.MultiSet String, MultiSet.MultiSet String)] -> [((String, String), (String, String))]
buildTransitionsForBuilding oldStates oldTransitions = [((q, p), (q', p')) |
                                                            q <- oldStates,
                                                            p <- oldStates,
                                                            p >= q,
                                                            let t = getTransitionWithQAndP q p,
                                                            let q' = fst t,
                                                            let p' = snd t]
    where
        hmap = HashMap.fromList $ map (\t -> (show $ fst t, t)) oldTransitions      -- maybe we have to make things strict here
        getTransitionWithQAndP :: String -> String -> (String, String)
        getTransitionWithQAndP q p = case HashMap.lookup (show $ MultiSet.fromList [q, p]) hmap of
                                        Just t -> (getQ'FromTransition t, getP'FromTransition t)
                                        Nothing -> (q, p)


filterTransitions :: [(MultiSet.MultiSet String, MultiSet.MultiSet String)] -> [(MultiSet.MultiSet String, MultiSet.MultiSet String)] -> [(MultiSet.MultiSet String, MultiSet.MultiSet String)] -> [(MultiSet.MultiSet String, MultiSet.MultiSet String)] -> [(MultiSet.MultiSet String, MultiSet.MultiSet String)]
filterTransitions certify convince drop noop = certify ++ filterList hsetCert convince ++ filterList hsetCert drop ++ filterList hsetAll noop
    where
        hsetCert = HashSet.fromList $ map (show . fst) certify
        hsetAll = HashSet.union hsetCert $ HashSet.fromList $ map (show . fst) $ convince ++ drop
        filterList hset = filter (\x -> not $ HashSet.member (show $ fst x) hset)


distribute :: PC.PopulationComputer Int-> PC.PopulationProtocol String
distribute pc = PC.PP {
    PC.statesPP = states,
    PC.deltaPP = transitions,
    PC.inputPP = Set.map (\q -> buildState (show q) 0 0) (PC.input pc),
    PC.outputPP = output
}
    where
        oldStates = map show $ Set.toAscList $ PC.states pc
        oldTransitions = map (\(input, output) -> (MultiSet.map show input, MultiSet.map show output)) $ Set.toList (PC.delta pc)
        oldOutput = mapOutput $ PC.outputOL pc
            where
                mapOutput (PC.Output true false) = PC.Output {
                    PC.true = map show true,
                    PC.false = map show false
                }
        states = [buildState q opinion token | q <- oldStates, opinion <- [0, 1], token <- [0, 1]]
        transitionsForBuilding = buildTransitionsForBuilding oldStates oldTransitions
        certifyTransitions = [(MultiSet.fromList [buildState q i1 t1, buildState p i2 t2], MultiSet.fromList [buildState q' 1 1, buildState p' 1 1]) |
                                    t <- transitionsForBuilding,
                                    let q = fst $ fst t,
                                    let p = snd $ fst t,
                                    let q' = fst $ snd t,
                                    let p' = snd $ snd t,
                                    q' `elem` PC.true oldOutput || p' `elem` PC.true oldOutput,
                                    q' `notElem` PC.false oldOutput,     -- this is a deviation from the paper, is it not?
                                    p' `notElem` PC.false oldOutput,     -- this is a deviation from the paper, is it not?
                                    i1 <- [0, 1],
                                    t1 <- [0, 1],
                                    i2 <- [0, 1],
                                    t2 <- [0, 1]] ++
                              [(MultiSet.fromList [buildState q i1 t1, buildState p i2 t2], MultiSet.fromList [buildState q' 0 1, buildState p' 0 1]) |
                                    t <- transitionsForBuilding,
                                    let q = fst $ fst t,
                                    let p = snd $ fst t,
                                    let q' = fst $ snd t,
                                    let p' = snd $ snd t,
                                    q' `elem` PC.false oldOutput || p' `elem` PC.false oldOutput,
                                    q' `notElem` PC.true oldOutput,      -- this is a deviation from the paper, is it not?
                                    p' `notElem` PC.true oldOutput,      -- this is a deviation from the paper, is it not?
                                    i1 <- [0, 1],
                                    t1 <- [0, 1],
                                    i2 <- [0, 1],
                                    t2 <- [0, 1]]
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
        transitions = filterTransitions certifyTransitions convinceTransitions dropTransitions noopTransitions
        output = PC.Output {
            PC.true = [buildState (show q) 1 token | q <- Set.toList (PC.states pc), token <- [0, 1]],
            PC.false = [buildState (show q) 0 token | q <- Set.toList (PC.states pc), token <- [0, 1]]
        }
