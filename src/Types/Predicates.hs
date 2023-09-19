module Types.Predicates (
    RemainderPredicate(..),
    ThresholdPredicate(..),
    BasePredicate,
    BoolOp(..),
    NotOp(..),
    Predicate(..)
) where


type Sum = [[Int]]
--                          coefficients m c
data RemainderPredicate = RP Sum Int Int
    deriving (Show)
--                          coefficients c
data ThresholdPredicate = TP Sum Int
    deriving (Show)
type BasePredicate = Either RemainderPredicate ThresholdPredicate

data BoolOp = AND | OR
    deriving (Show)
data NotOp = NOT
    deriving (Show)

data Predicate = Leaf BasePredicate | NodeP Predicate BoolOp Predicate | NodeN NotOp Predicate
    deriving (Show)
