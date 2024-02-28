module Tests.TestHelper (
    genericPpTest,
    genericPcTest,
    genericPcBcTest
) where

import InputOutput.MapOutput
import qualified Types.PopulationComputer as PC
import Helper.List

import Data.List (sortBy, sort, find)
import Test.HUnit
import System.IO
import System.Process
import Helper.Tuple
import Data.Maybe (fromJust)
import qualified Data.HashMap.Lazy as HashMap
import Control.Monad (when)
import Debug.Trace

runProtocol :: String -> IO String
runProtocol filePath = do
    (Just hin, Just hout, _, ph) <- createProcess (shell "/home/johannes/OneDrive/Uni/Bachelor-Arbeit/popsim-main/popsimio array 100000000") { std_in = CreatePipe, std_out = CreatePipe }
    hPutStr hin filePath
    hClose hin
    waitForProcess ph
    hGetContents hout


printPopsimConfiguration :: [Int] -> HashMap.HashMap String Int -> Int -> String -> String
printPopsimConfiguration output stateMapper finalNumAgents protDesc = do
    let stateMapperList = HashMap.toList stateMapper
    let reverseMapperList = map (\(k, v) -> (v, k)) stateMapperList
    let reversedStateMapper = HashMap.fromList reverseMapperList
    "\n-----------------------------------------------------\n" ++
        "Failure in protocol: " ++ protDesc ++ "\n" ++
        unlines (mapWithIndex (\i o -> show (reversedStateMapper HashMap.! i, o)) output) ++
        "\n-----------------------------------------------------\n"


sortOutputs :: (Int, Bool) -> (Int, Bool) -> Ordering
sortOutputs o1 o2
    | fst o1 > fst o2 = GT
    | fst o1 < fst o2 = LT
    | otherwise       = EQ


getOutputStatesFromBooleanCircuit :: PC.BooleanCircuit Int -> [Int]
getOutputStatesFromBooleanCircuit ([], _) = []
getOutputStatesFromBooleanCircuit ((PC.Input x):gates, _) = x : getOutputStatesFromBooleanCircuit (gates, [])
getOutputStatesFromBooleanCircuit (g:gates, _) = getOutputStatesFromBooleanCircuit (gates, [])


evalGate :: PC.BooleanCircuit Int -> Int -> [(Int, Int)] -> Int
evalGate circuit@(gates, edges) gateIndex output = case outputGate of
                                                        (PC.Input x) -> if snd (fromJust $ find (\(f, s) -> f == x) output) > 0 then 1 else 0
                                                        -- PC.ConstT -> 1
                                                        -- PC.ConstF -> 0
                                                        gate -> PC.evalGate gate leftOpinion rightOpinion
    where
        outputGate = gates !! gateIndex
        edge = fromJust $ find ((== gateIndex) . fst) edges
        leftOpinion = evalGate circuit (fst $ snd edge) output
        rightOpinion = evalGate circuit (snd $ snd edge) output


evalBooleanCircuit :: PC.BooleanCircuit Int -> [(Int, Int)] -> Bool
evalBooleanCircuit circuit@(gates, edges) output = PC.evalGate outputGate leftOpinion rightOpinion == 1
    where
        outputEdge = last edges
        outputGate = gates !! fst outputEdge
        leftOpinion = evalGate circuit (fst $ snd outputEdge) output
        rightOpinion = evalGate circuit (snd $ snd outputEdge) output

-- | returns whether or not the correct number of agents has the given opinion according to the expected output
evalOutputAgent :: Bool -> Bool -> Int -> (Bool, Bool)
evalOutputAgent expected opinion agents
    | expected && opinion && agents > 0 = (True, expected == opinion)
    | expected && not opinion && agents == 0 = (True, expected == opinion)
    | not expected && not opinion && agents > 0 = (True, expected == opinion)
    | not expected && opinion && agents == 0 = (True, expected == opinion)
    | otherwise = (False, expected == opinion)

-- | returns true if the given output represents the correct output and false otherwise
evalPopsimOutputPp :: PC.OutputLists Int -> String -> Int -> Bool -> Bool
evalPopsimOutputPp definition output numAgents expectedOutput = do
    let mappedTrue = map (\t -> (t, True)) $ PC.true definition
    let mappedFalse = map (\f -> (f, False)) $ PC.false definition
    let outputList = sortBy sortOutputs $ mappedTrue ++ mappedFalse
    let popsimOutput = map (\x -> read x :: Int) $ words output
    let finalNumAgents = sum popsimOutput
    let evaluatedOutput = map2 (evalOutputAgent expectedOutput . snd) outputList popsimOutput
    let outputConsens = all fst (filter (\(_, o) -> not o) evaluatedOutput) && any fst (filter snd evaluatedOutput)
    finalNumAgents == numAgents && outputConsens

evalPopsimOutputPc :: PC.OutputLists Int -> String -> Int -> Bool -> String -> HashMap.HashMap String Int -> Bool
evalPopsimOutputPc definition output numAgents expectedOutput protDesc stateMapper = do
    let mappedTrue = map (\t -> (t, True)) $ PC.true definition
    let mappedFalse = map (\f -> (f, False)) $ PC.false definition
    let outputList = sortBy sortOutputs $ mappedTrue ++ mappedFalse
    let popsimOutput = map (\x -> read x :: Int) $ words output
    let outputAgents = [x | i <- outputList, let x = popsimOutput !! (fst i - 1)]
    let finalNumAgents = sum outputAgents
    let evaluatedOutput = map2 (evalOutputAgent expectedOutput . snd) outputList outputAgents
    let outputConsens = all fst (filter (\(_, o) -> not o) evaluatedOutput) && any fst (filter snd evaluatedOutput)
    if finalNumAgents == 0
        then {- trace (printPopsimConfiguration popsimOutput stateMapper finalNumAgents protDesc) -} (error $ "Haven't reached a output configuration yet. Protocol for: " ++ protDesc)
        else {- trace (printPopsimConfiguration popsimOutput stateMapper finalNumAgents protDesc) -} outputConsens

evalPopsimOutputPcBc :: PC.BooleanCircuit Int -> String -> Int -> Bool -> String -> HashMap.HashMap String Int -> Bool
evalPopsimOutputPcBc definition output numAgents expectedOutput protDesc stateMapper = do
    let outputList = sort $ getOutputStatesFromBooleanCircuit definition
    let popsimOutput = map (\x -> read x :: Int) $ words output
    let outputAgents = [x | i <- outputList, let x = popsimOutput !! (i - 1)]
    let finalNumAgents = sum outputAgents
    let outputConsens = evalBooleanCircuit definition $ map2 (,) outputList outputAgents
    {- if outputConsens /= expectedOutput
        then trace (printPopsimConfiguration popsimOutput stateMapper finalNumAgents protDesc) False
        else -}
    outputConsens == expectedOutput


genericPpTest :: (String -> PC.PopulationProtocol Int) -> String -> [Int] -> Bool -> Bool -> IO (String, Bool)
genericPpTest createProtocol protocolStr input expectedOutput isPreprocessed = do
    let protocol = createProtocol protocolStr
    let mappedProtocol = populationProtocolToPopSim protocol $ Just input
    popsimOutput <- runProtocol $ fst mappedProtocol
    let finalConfiguration = last $ lines popsimOutput
    return (protocolStr ++ " with input " ++ show input ++ (if isPreprocessed then " - preprocessed" else ""),
            evalPopsimOutputPp (snd mappedProtocol) finalConfiguration (sum input) expectedOutput)

genericPcTest :: (String -> PC.PopulationComputer String) -> String -> [Int] -> Bool -> Bool -> IO (String, Bool)
genericPcTest createProtocol protocolStr input expectedOutput isPreprocessed = do
    let protocol = createProtocol protocolStr
    let mappedProtocol = populationComputerToPopSim protocol $ Just input
    popsimOutput <- runProtocol $ fst3 mappedProtocol
    let finalConfiguration = last $ lines popsimOutput
    let  protocolDesc = protocolStr ++ " with input " ++ show input ++ (if isPreprocessed then " - preprocessed" else "")
    return (protocolDesc,
            evalPopsimOutputPc (snd3 mappedProtocol) finalConfiguration (sum input) expectedOutput protocolDesc (thd3 mappedProtocol))

genericPcBcTest :: (String -> PC.PopulationComputer String) -> String -> [Int] -> Bool -> Bool -> IO (String, Bool)
genericPcBcTest createProtocol protocolStr input expectedOutput isPreprocessed = do
    let protocol = createProtocol protocolStr
    let mappedProtocol = populationComputerToPopSimBc protocol $ Just input
    popsimOutput <- runProtocol $ fst3 mappedProtocol
    let finalConfiguration = last $ lines popsimOutput
    let  protocolDesc = protocolStr ++ " with input " ++ show input ++ (if isPreprocessed then " - preprocessed" else "")
    return (protocolDesc,
            evalPopsimOutputPcBc (snd3 mappedProtocol) finalConfiguration (sum input) expectedOutput protocolDesc (thd3 mappedProtocol))
