module Types.PopulationComputer (
    OutputValues(..),
    PopulationComputer(..),
    PopulationProtocol(..),
) where

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet


data OutputValues = F | T | Undef
    deriving (Eq)

type Q a = Set.Set a
type C a = MultiSet.MultiSet a
type Delta a = Set.Set (C a, C a)
type I a = Q a
    -- maybe we have to give an explicit description as a set of tuples of configurations that fullfill the predicate (Powerset -> filter which configurations fullfill the predicate)
-- type O a = C a -> OutputValues -- We use the output of a configuration as default output function. This is different to the basic definition in the paper.
type H a = MultiSet.MultiSet a

data PopulationComputer a = PC {
    states:: Q a,
    delta:: Delta a,
    input:: I a,
    -- output:: O a,
    helpers:: H a
}
    deriving (Show)
-- type PopulationComputer a = (Q a, Delta a, I a, O a, H a) -- vllt als record

data PopulationProtocol a = PP {
    statesPP:: Q a,
    deltaPP:: Delta a,
    inputPP:: I a--,
    -- outputPP:: O a
}
-- type PopulationProtocol = (Q a, Delta a, I a, O a)
