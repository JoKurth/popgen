module PresburgerMapper.Union (
    constructPC
) where


import Helper.Math
import Helper.List
import qualified Types.PopulationComputer as PC (OutputValues(..), PopulationComputer(..))
import qualified Types.Predicates as Predicates (Predicate(..), BoolOp(..), NotOp(..), RemainderPredicate(..), ThresholdPredicate(..), BasePredicate)
import qualified PresburgerMapper.Remainder as RemainderMapper (constructRemainderPC)
import qualified PresburgerMapper.Threshold as ThresholdMapper (constructThresholdPC)

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet


-- Helper

-- | The name of this function follows the naming in the paper
bin :: Int -> MultiSet.MultiSet Int
bin x = MultiSet.fromList [ signum x * 2^i | i <- intToBinary $ abs x]

-- Transformation

-- Rewrite the predicates
rewriteRemainderPredicate :: Predicates.RemainderPredicate -> Predicates.RemainderPredicate
rewriteRemainderPredicate (Predicates.RP coefficients m c) = Predicates.RP (map (flatten . map (Set.toList . multiSetSupport . bin)) coefficients) m c

rewriteThresholdPredicate :: Predicates.ThresholdPredicate -> Predicates.ThresholdPredicate
rewriteThresholdPredicate (Predicates.TP coefficients c) = Predicates.TP (map (flatten . map (Set.toList . multiSetSupport . bin)) coefficients) c

rewritePredicate :: Predicates.BasePredicate -> Predicates.BasePredicate
rewritePredicate (Left p) = Left (rewriteRemainderPredicate p)
rewritePredicate (Right p) = Right (rewriteThresholdPredicate p)

-- Construct subcomputers
predicateToPredicateList :: Predicates.Predicate -> [Predicates.BasePredicate]
predicateToPredicateList (Predicates.NodeP lPred operator rPred) = predicateToPredicateList lPred ++ predicateToPredicateList rPred
predicateToPredicateList (Predicates.NodeN operator pred) = predicateToPredicateList pred
predicateToPredicateList (Predicates.Leaf pred) = [rewritePredicate pred]

constructSubPCs :: Predicates.Predicate -> [PC.PopulationComputer Int]
constructSubPCs pred = mapWithIndex predToPC $ predicateToPredicateList pred
    where
        predToPC :: Int -> Predicates.BasePredicate -> PC.PopulationComputer Int
        predToPC _ (Left p) = RemainderMapper.constructRemainderPC p
        predToPC ind (Right p) = ThresholdMapper.constructThresholdPC p ind

-- Combine subcomputers
-- from now on we will use strings as names for the states following the schema x_i,j,e
renameState :: Int -> Int -> String
renameState _ 0 = show 0
renameState index state = show state ++ "_" ++ show index

renameStates :: Int -> PC.PopulationComputer Int -> Set.Set String
renameStates index pc = Set.map (renameState index) $ PC.states pc

renameTransition :: Int -> (MultiSet.MultiSet Int, MultiSet.MultiSet Int) -> (MultiSet.MultiSet String, MultiSet.MultiSet String)
renameTransition index (inputStates, outputStates) = (MultiSet.map (renameState index) inputStates, MultiSet.map (renameState index) outputStates)

renameStatesInTransitions :: Int -> Set.Set (MultiSet.MultiSet Int, MultiSet.MultiSet Int) -> Set.Set (MultiSet.MultiSet String, MultiSet.MultiSet String)
renameStatesInTransitions index = Set.map (renameTransition index)

numberOfVariables :: Predicates.BasePredicate -> Int
numberOfVariables (Left (Predicates.RP coefficients _ _)) = length coefficients
numberOfVariables (Right (Predicates.TP coefficients _)) = length coefficients

totalNumberOfVariables :: [Predicates.BasePredicate] -> Int
totalNumberOfVariables = maxFromList . map numberOfVariables

extractCoefficientsFromPredicates :: Predicates.BasePredicate -> [[Int]]
extractCoefficientsFromPredicates (Left (Predicates.RP coefficients _ _)) = coefficients
extractCoefficientsFromPredicates (Right (Predicates.TP coefficients _)) = coefficients

combine :: [Int] -> [Int] -> [Int]
combine [] [] = []
combine [] (y:ys) = y : combine [] ys
combine (x:xs) [] = x : combine xs []
combine (x:xs) (y:ys) = x+y : combine xs ys

combineLists :: [[Int]] -> [Int]
combineLists [] = []
combineLists [x] = x
combineLists [x, y] = combine x y
combineLists (x:y:xs) = combine (combine x y) $ combineLists xs

extractIndexFromAllLists :: Int -> [[a]] -> [a]
extractIndexFromAllLists index lists = map (!! index) $ filter (\list -> length list > index) lists

constructDistributionTransition :: Int -> String -> Int -> [[[Int]]] -> (MultiSet.MultiSet String, MultiSet.MultiSet String)
constructDistributionTransition variableNumber inputVar numberOfCoefficients coefficients = (inputTransition, outputTransition)
    where
        inputTransition = if numberOfCoefficients > 1
            then MultiSet.fromList $ flatten [[inputVar], [show (0*i) | i <- [1 .. numberOfCoefficients-1]]]
            else MultiSet.fromList [inputVar, show 0]
        outputTransition = if numberOfCoefficients > 1
            then MultiSet.fromList $ flatten $ mapWithIndex (map . renameState) $ map (!! (variableNumber-1)) $ filter (\l -> length l > variableNumber-1) coefficients
            else MultiSet.fromList $ show 0 : flatten (map (!! (variableNumber-1)) $ filter (\l -> length l > variableNumber-1) $ mapWithIndex (map . map . renameState) coefficients)

calcDistributeTransitions :: Set.Set String -> [Predicates.BasePredicate] -> Set.Set (MultiSet.MultiSet String, MultiSet.MultiSet String)
calcDistributeTransitions inputVariables predicates = Set.fromList $ mapWithIndex (\index numberOfCoefficients -> constructDistributionTransition index (Set.toList inputVariables !! (index-1)) numberOfCoefficients coefficients) numberOfCoefficientInputAgentsPerVariable -- we have to substract 1 from the index because mapWithIndex starts at 1
    where
        coefficients = map extractCoefficientsFromPredicates predicates
        numberOfBinaryCoefficientsPerCoefficientPerPredicate = map (map length) coefficients
        numberOfCoefficientInputAgentsPerVariable = combineLists numberOfBinaryCoefficientsPerCoefficientPerPredicate


combineSubPCs :: [Predicates.BasePredicate] -> [PC.PopulationComputer Int] -> PC.PopulationComputer String
combineSubPCs predList pcs = PC.PC{
    PC.states = Set.union renamedStates inputVariables,
    PC.delta = Set.unions joinedTransitions,
    PC.input = inputVariables,
    -- PC.output = outputFunction,
    PC.helpers = numOfHelpers
}
    where
        inputVariables = Set.fromList $ map (\i -> "x_" ++ show i) [1 .. totalNumberOfVariables predList] -- the input variables
        renamedStates = Set.unions $ mapWithIndex renameStates pcs
        joinedTransitions =    mapWithIndex (\i pc -> renameStatesInTransitions i $ PC.delta pc) pcs -- subcomputer
                            ++ [calcDistributeTransitions inputVariables predList]                   -- distribute
        coefficients = map extractCoefficientsFromPredicates predList
        bar = map (map length) coefficients
        numOfHelpers = MultiSet.map show $ MultiSet.insertMany 0 (max (maxFromList $ map maxFromList bar) 2 - 1) $ MultiSet.unions $ map PC.helpers pcs

-- Export
constructPC :: Predicates.Predicate -> PC.PopulationComputer String
constructPC pred = combineSubPCs inputPredicates $ constructSubPCs pred
    where
        inputPredicates = predicateToPredicateList pred
