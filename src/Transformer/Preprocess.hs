module Transformer.Preprocess (
    preprocess
) where

import qualified Types.PopulationComputer as PC (PopulationComputer(..))

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet

preprocess :: PC.PopulationComputer String -> PC.PopulationComputer String
preprocess pc = PC.PCB {
    PC.states = Set.unions [PC.states pc, Set.fromList ["h"], Set.fromList xStars],
    PC.delta = Set.union (PC.delta pc) (Set.fromList starTransitions),
    PC.input = Set.fromList xStars,
    PC.output = PC.output pc,
    PC.helpers = MultiSet.union (PC.helpers pc) (MultiSet.insert "h" MultiSet.empty)
}
    where
        xAndxStars = map (\x -> (x, x ++ "_*")) $ Set.toList $ PC.input pc
        xStars = map snd xAndxStars
        starTransitions = map (\tuple -> (MultiSet.fromList [snd tuple, "h"], MultiSet.fromList [fst tuple, "h"])) xAndxStars
