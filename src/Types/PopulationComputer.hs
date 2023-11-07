module Types.PopulationComputer (
    OutputValues(..),
    Gate(..),
    Edge(..),
    BooleanCircuit(..),
    OutputLists(..),
    PopulationComputer(..),
    PopulationProtocol(..),
    intGateToStringGate,
    isInputGate,
    evalGate
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

data OutputLists a = Output {
    true :: [a],
    false :: [a]
}
    deriving (Show)


type Q a = Set.Set a
type C a = MultiSet.MultiSet a
type Delta a = Set.Set (C a, C a)
type I a = Q a
-- type O a = BooleanCircuit a | OutputLists a
type H a = MultiSet.MultiSet a

data PopulationComputer a = PCB {
    states :: Q a,
    delta :: Delta a,
    input :: I a,
    output :: BooleanCircuit a,
    helpers :: H a
} | PCL {
    states :: Q a,
    delta :: Delta a,
    input :: I a,
    outputOL :: OutputLists a,
    helpers :: H a
}
    deriving (Show)
-- type PopulationComputer a = (Q a, Delta a, I a, O a, H a) -- vllt als record

data PopulationProtocol a = PP {
    statesPP :: Q a,
    deltaPP :: Delta a,
    inputPP :: I a,
    outputPP :: OutputLists a
}
    deriving (Show)
-- type PopulationProtocol = (Q a, Delta a, I a, O a)


intGateToStringGate :: Gate Int -> Gate String
intGateToStringGate AND = AND
intGateToStringGate OR = OR
intGateToStringGate NOT = NOT
intGateToStringGate ConstT = ConstT
intGateToStringGate ConstF = ConstF
intGateToStringGate (Input g) = Input $ show g

isInputGate :: Gate a -> Bool
isInputGate (Input _) = True
isInputGate _ = False

-- | each Int encodes a boolean value, i.e. 0 encodes False, 1 encodes True all other values are undefined and lead to undefined behavior
evalGate :: Gate a -> Int -> Int -> Int
evalGate AND 1 1 = 1
evalGate AND _ _ = 0
evalGate OR 0 0 = 0
evalGate OR _ _ = 1
evalGate NOT 1 _ = 0
evalGate NOT 0 _ = 1
evalGate ConstT _ _ = 1
evalGate ConstF _ _ = 0
evalGate (Input _) _ _= 0
