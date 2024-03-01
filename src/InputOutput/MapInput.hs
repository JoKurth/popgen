module InputOutput.MapInput (
    stringToPredicate
) where

import Data.List.Split (splitOn)
import Text.Regex.TDFA ( (=~) )
import Text.Parsec (runParser, ParseError)
import Data.BoolExpr (BoolExpr(..), Signed(..))
import Data.BoolExpr.Parser (parseBoolExpr, identifier)

import qualified Types.Predicates as Predicates (Predicate(..), BoolOp(..), NotOp(..), RemainderPredicate(..), ThresholdPredicate(..), BasePredicate)


mapRemainderPredicate :: String -> Predicates.BasePredicate
mapRemainderPredicate predicate = Left (Predicates.RP (map ((: []) . (`mod` head constants)) coefficients) (head constants) (last constants))
    where
        coefficientRegEx = "\\[\\-?[0-9]+(,\\-?[0-9]+)*\\]"
        constantsRegEx = "[1-9]+;[0-9]+$"
        coefficients = read (predicate =~ coefficientRegEx :: String) :: [Int]
        constants = map (\x -> read x :: Int) $ splitOn ";" (predicate =~ constantsRegEx :: String)

mapThresholdPredicate :: String -> Predicates.BasePredicate
mapThresholdPredicate predicate = Right (Predicates.TP (map (: []) coefficients) constant)
    where
        coefficientRegEx = "\\[\\-?[0-9]+(,\\-?[0-9]+)*\\]"
        constantRegEx = "\\-?[0-9]+$"
        coefficients = read (predicate =~ coefficientRegEx :: String) :: [Int]
        constant = read (predicate =~ constantRegEx :: String) :: Int

mapPredicate :: String -> Predicates.BasePredicate
mapPredicate predicateString
    | predicateString =~ remainderRegEx :: Bool = mapRemainderPredicate predicateString
    | predicateString =~ thresholdRegEx :: Bool = mapThresholdPredicate predicateString
    | otherwise = error "The input predicate does not match the format"
        where
            remainderRegEx = "^\\[\\-?[0-9]+(,\\-?[0-9]+)*\\];[1-9]+;[0-9]+$"
            thresholdRegEx = "^\\[\\-?[0-9]+(,\\-?[0-9]+)*\\];\\-?[0-9]+$"


boolExprToPredicate :: BoolExpr String -> Predicates.Predicate
boolExprToPredicate (BOr leftExpr rightExpr) = Predicates.NodeP (boolExprToPredicate leftExpr) Predicates.OR (boolExprToPredicate rightExpr)
boolExprToPredicate (BAnd leftExpr rightExpr) = Predicates.NodeP (boolExprToPredicate leftExpr) Predicates.AND (boolExprToPredicate rightExpr)
boolExprToPredicate (BNot expr) = Predicates.NodeN Predicates.NOT (boolExprToPredicate expr)
boolExprToPredicate (BConst (Positive expr)) = Predicates.Leaf (mapPredicate expr)
boolExprToPredicate (BConst (Negative expr)) = Predicates.Leaf (mapPredicate expr)

resultFromParserOrThrow :: Either ParseError (BoolExpr String) -> BoolExpr String
resultFromParserOrThrow (Left _) = error "Could not parse the given predicate. Please try again with the correct syntax."
resultFromParserOrThrow (Right expr) = expr


-- | Maps an input string to a predicate. Throws an error if the string does not match the specified format.
stringToPredicate :: String -> Predicates.Predicate
stringToPredicate "" = error "Cannot create predicate from empty string."
stringToPredicate input = boolExprToPredicate $ resultFromParserOrThrow $ runParser (parseBoolExpr identifier) () "" input
