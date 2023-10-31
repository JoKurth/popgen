module Types.PopulationComputer (
    OutputValues(..),
    Gate(..),
    Edge(..),
    BooleanCircuit(..),
    PopulationComputer(..),
    PopulationProtocol(..),
    intGateToStringGate
) where

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet


data OutputValues = F | T | Undef
    deriving (Eq)

data Gate a = AND | OR | NOT | ConstT | ConstF | Input a
    deriving (Show)

-------------(index of the gate that has these inputs, (in1, in2))
type Edge = (Int, (Int, Int))

type BooleanCircuit a = ([Gate a], [Edge])

data Output = BooleanCircuit
    deriving (Show)


type Q a = Set.Set a
type C a = MultiSet.MultiSet a
type Delta a = Set.Set (C a, C a)
type I a = Q a
type O a = BooleanCircuit a
type H a = MultiSet.MultiSet a

data PopulationComputer a = PC {
    states :: Q a,
    delta :: Delta a,
    input :: I a,
    output:: O a,
    helpers :: H a
}
    deriving (Show)
-- type PopulationComputer a = (Q a, Delta a, I a, O a, H a) -- vllt als record

data PopulationProtocol a = PP {
    statesPP :: Q a,
    deltaPP :: Delta a,
    inputPP :: I a--,
    -- outputPP :: O a
}
-- type PopulationProtocol = (Q a, Delta a, I a, O a)


intGateToStringGate :: Gate Int -> Gate String
intGateToStringGate AND = AND
intGateToStringGate OR = OR
intGateToStringGate NOT = NOT
intGateToStringGate ConstT = ConstT
intGateToStringGate ConstF = ConstF
intGateToStringGate (Input g) = Input $ show g
