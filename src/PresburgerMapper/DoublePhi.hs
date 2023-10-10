module PresburgerMapper.DoublePhi (
    doublePredicate
) where

import Helper.List (flatten)
import qualified Types.Predicates as Predicates (RemainderPredicate(..), ThresholdPredicate(..), BasePredicate(..), Predicate(..))

double :: [[Int]] -> [[Int]]
double [] = []
double [[]] = [[]]
double (x:xs) = [[head x], [2 * head x]] ++ double xs

doubleBasePredicate :: Predicates.BasePredicate -> Predicates.BasePredicate
doubleBasePredicate (Left (Predicates.RP sum m c)) = Left (Predicates.RP (double sum) m c)
doubleBasePredicate (Right (Predicates.TP sum m)) = Right (Predicates.TP (double sum) m)

doublePredicate :: Predicates.Predicate -> Predicates.Predicate
doublePredicate (Predicates.NodeP lPred operator rPred) = Predicates.NodeP (doublePredicate lPred) operator (doublePredicate rPred)
doublePredicate (Predicates.NodeN operator pred) = Predicates.NodeN operator (doublePredicate pred)
doublePredicate (Predicates.Leaf pred) = Predicates.Leaf (doubleBasePredicate pred)
