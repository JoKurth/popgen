-- we do not have to initialize any fields in the else branch in line 140 because if this branch is selected, the no field is ever selected
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main where

import qualified Types.PopulationComputer as PC (OutputValues(..), PopulationComputer(..), PopulationProtocol(..), OutputLists (..))
import InputOutput.MapInput
import InputOutput.MapOutput
import PresburgerMapper.DoublePhi
import PresburgerMapper.Union
import Transformer.Preprocess
import Transformer.Binarise
import Transformer.Focalise
import Transformer.Autarkify
import Transformer.Distribute

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet
import Data.List (sort)
import Data.List.Split (splitOn)
import Helper.List (flatten)
import Control.Monad (when)
import Helper.Tuple (fst3, omitLast)
import System.IO (openFile, IOMode (WriteMode), hClose, hPutStr, hPutStrLn, hPrint)

main :: IO ()
main = do
    -- user input

    putStrLn "Please enter the predicate you want to transform:"
    putStrLn "(The predicate must be in the following format:"
    putStrLn "    - Modulo predicates: [a1,..,as];m;c"
    putStrLn "    - Threshold predicates: [a1,..,as];c"
    putStrLn "    - Operator AND: AND"
    putStrLn "    - Operator OR: OR"
    putStrLn "    - Operator NOT: NOT"
    putStrLn "    - Parenthesis: (...)"
    putStrLn "    - coefficents must be seperated by a single `,` (no additional spaces)"
    putStrLn "      the coefficients and each constant must be seperated by a single `;` (no additional spaces either)"
    putStrLn "      predicates and Operators must be separated by at least one space"
    putStrLn "    - there may be no spaces within a predicate"
    putStrLn "      more than one spaces between a predicate and an operator are accepted"
    putStrLn "      multiple spaces around parenthesis are accepted (not around brackets)"
    putStrLn "  e. g. `([8,5,1];7;2 AND [-2,1];5) OR [9,4,3];2;1`"
    putStrLn "  Note:"
    putStrLn "    - AND has a higher precedence than OR"
    putStrLn "    - NOT refers to the following single predicate or parenthesis"
    putStrLn ")"
    putStrLn "Input: "

    predicateString <- getLine

    putStrLn ""

    putStrLn "Do you want to execute the preprocessing step? (y/N)"
    putStrLn "  Note: It is not necessasry but increases the size of the generated protocols."
    perfPreproc <- getLine

    putStrLn ""

    putStrLn "Do you want to execute the distribute conversion? (y/N)"
    putStrLn "  Note: You need huge amounts of memory and the conversion is not tested."
    perfDistr <- getLine

    putStrLn ""

    putStrLn "Do you want to perform the analysis? (y/N)"
    perfAnalysis <- getLine

    putStrLn ""

    putStrLn "Please enter a file path for the output of the conversion."
    putStrLn "  Note: All contents of the file will be overwritten."
    putStrLn "        If the file does not exist, it will be created."
    putStrLn "        You can enter absolute as well as realtive paths."
    filePath <- getLine

    putStrLn ""
    putStrLn "----------------------------"
    putStrLn ""
    putStrLn "Output: "

    -- construction

    let predicate = stringToPredicate predicateString

    let doublePhi = doublePredicate predicate

    let initPC = constructPC doublePhi
    putStrLn "  initial population computer..."
    when (perfAnalysis == "y") (putStrLn $ "    number of states: " ++ show (length $ PC.states initPC))
    when (perfAnalysis == "y") (putStrLn $ "    number of transitions: " ++ show (length $ PC.delta initPC))

    when (perfPreproc == "y") (putStrLn "")
    let preprocessedPC = (if perfPreproc == "y" then preprocess else id) initPC
    when (perfPreproc == "y") (putStrLn "  Preprocess...")
    when (perfPreproc == "y" && perfAnalysis == "y") (putStrLn $ "    number of states: " ++ show (length $ PC.states preprocessedPC))
    when (perfPreproc == "y" && perfAnalysis == "y") (putStrLn $ "    number of transitions: " ++ show (length $ PC.delta preprocessedPC))

    putStrLn ""
    let binarisedPC = binarise preprocessedPC
    putStrLn "  Binarise..."
    when (perfAnalysis == "y") (putStrLn $ "    number of states: " ++ show (length $ PC.states binarisedPC))
    when (perfAnalysis == "y") (putStrLn $ "    number of transitions: " ++ show (length $ PC.delta binarisedPC))

    putStrLn ""
    let focalisedPC = focalise binarisedPC
    putStrLn "  Focalise..."
    when (perfAnalysis == "y") (putStrLn $ "    number of states: " ++ show (length $ PC.states focalisedPC))
    when (perfAnalysis == "y") (putStrLn $ "    number of transitions: " ++ show (length $ PC.delta focalisedPC))

    putStrLn ""
    let autarkifiedPC = autarkify focalisedPC
    putStrLn "  Autarkify..."
    when (perfAnalysis == "y") (putStrLn $ "    number of states: " ++ show (length $ PC.states autarkifiedPC))
    when (perfAnalysis == "y") (putStrLn $ "    number of transitions: " ++ show (length $ PC.delta autarkifiedPC))

    when (perfDistr == "y") (putStrLn "")
    let distributedPC = if perfDistr == "y" then distribute $ stringPcToIntPc autarkifiedPC else PC.PP{} -- we do not have to initialize any fields because if this branch is selected, the no field is ever selected
    when (perfDistr == "y") (putStrLn "  Distribute...")
    when (perfDistr == "y" && perfAnalysis == "y") (putStrLn $ "    number of states: " ++ show (length $ PC.statesPP distributedPC))
    when (perfDistr == "y" && perfAnalysis == "y") (putStrLn $ "    number of transitions: " ++ show (length $ PC.deltaPP distributedPC))

    -- output

    putStrLn ""

    let output = if perfDistr == "y" then populationProtocolToPopSim distributedPC Nothing else omitLast $ populationComputerToPopSim autarkifiedPC Nothing
    let outputFunctionStr = "Output {\n    true: " ++
                            show (sort (PC.true $ snd output)) ++ ",\n    false: " ++
                            show (sort (PC.false $ snd output)) ++ "\n}"

    outputFile <- openFile filePath WriteMode
    hPutStrLn outputFile outputFunctionStr
    hPutStrLn outputFile ""
    hPutStrLn outputFile ""
    hPutStrLn outputFile $ fst output
    hClose outputFile

    putStrLn $ "Succesfully written file to path `" ++ filePath ++ "`"
