module InputOutput.MapInput (
    stringToPredicate
) where

import Data.List.Split (splitOn)
import Text.Regex.TDFA ( (=~) )

import qualified Types.Predicates as Predicates (Predicate(..), BoolOp(..), NotOp(..), RemainderPredicate(..), ThresholdPredicate(..), BasePredicate)


mapRemainderPredicate :: String -> Predicates.BasePredicate
mapRemainderPredicate predicate = Left (Predicates.RP (map (: []) coefficients) (head constants) (last constants))
    where
        coefficientRegEx = "\\[\\-?[0-9]+(,[ ]*\\-?[0-9]+)*\\]"
        constantsRegEx = "[0-9]+[ ]+[0-9]+"
        coefficients = read (predicate =~ coefficientRegEx :: String) :: [Int]
        constants = map (\x -> read x :: Int) $ splitOn " " (predicate =~ constantsRegEx :: String)

mapThresholdPredicate :: String -> Predicates.BasePredicate
mapThresholdPredicate predicate = Right (Predicates.TP (map (: []) coefficients) constant)
    where
        coefficientRegEx = "\\[\\-?[0-9]+(,[ ]*\\-?[0-9]+)*\\]"
        constantRegEx = "[0-9]+[ ]*$"
        coefficients = read (predicate =~ coefficientRegEx :: String) :: [Int]
        constant = read (predicate =~ constantRegEx :: String) :: Int

mapPredicate :: String -> Predicates.BasePredicate
mapPredicate predicateString
    | predicateString =~ remainderRegEx :: Bool = mapRemainderPredicate predicateString
    | predicateString =~ thresholdRegEx :: Bool = mapThresholdPredicate predicateString
    | otherwise = error "The input predicate does not match the format"
        where
            remainderRegEx = "^[ ]*\\[\\-?[0-9]+(,[ ]*\\-?[0-9]+)*\\][ ]*[0-9]+[ ]+[0-9]+[ ]*$"
            thresholdRegEx = "^[ ]*\\[\\-?[0-9]+(,[ ]*\\-?[0-9]+)*\\][ ]*[0-9]+[ ]*$"


conjunctionStringsToPredicate :: [String] -> Predicates.Predicate
conjunctionStringsToPredicate [] = error "Cannot create predicate from empty input."
conjunctionStringsToPredicate [""] = error "Cannot create predicate from empty string."
conjunctionStringsToPredicate [x] = Predicates.Leaf (mapPredicate x)
conjunctionStringsToPredicate (x:xs) = Predicates.NodeP (Predicates.Leaf (mapPredicate x)) Predicates.AND (conjunctionStringsToPredicate xs)


stringToPredicate :: [[String]] -> Predicates.Predicate
stringToPredicate [] = error "Cannot create predicate from empty input."
stringToPredicate [[]] = error "Cannot create predicate from empty input."
stringToPredicate [[""]] = error "Cannot create predicate from empty string."
stringToPredicate [x] = conjunctionStringsToPredicate x
stringToPredicate (x:xs) = Predicates.NodeP (conjunctionStringsToPredicate x) Predicates.OR (stringToPredicate xs)
