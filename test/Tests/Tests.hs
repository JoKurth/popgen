module Tests.Tests
where

import qualified Tests.BinariseTests as BT
import qualified Tests.FocaliseTests as FT
import qualified Tests.AutarkifyTests as AT
import qualified Tests.DistributeTests as DT
import qualified Tests.BoolCombTests as BC

import Test.HUnit
import System.Exit
import Control.Monad (when)


main = do
    putStrLn "Starting all tests."
    putStrLn "Note: This requires a lot of time and huge amounts of memory."

    binariseTests <- BT.tests
    countBinarise <- runTestTT binariseTests
    binarisePreprocessTests <- BT.testsPreprocess
    countBinarisePreprocess <- runTestTT binarisePreprocessTests

    focaliseTests <- FT.tests
    countFocalise <- runTestTT focaliseTests
    focalisePreprocessTests <- FT.testsPreprocess
    countFocalisePreprocess <- runTestTT focalisePreprocessTests

    autarkifyTests <- AT.tests
    countAutarkify <- runTestTT autarkifyTests
    autarkifyPreprocessTests <- AT.testsPreprocess
    countAutarkifyPreprocess <- runTestTT autarkifyPreprocessTests

    -- distributeTests <- DT.tests
    -- countDistribute <- runTestTT distributeTests
    -- distributePreprocessTests <- DT.testsPreprocess
    -- countDistributePreprocess <- runTestTT distributePreprocessTests

    boolCombTests <- BC.tests
    countBoolComb <- runTestTT boolCombTests


    let binariseFailures = failures countBinarise
    let binariseErrors = errors countBinarise
    let binarisePreprocessFailures = failures countBinarisePreprocess
    let binarisePreprocessErrors = errors countBinarisePreprocess

    let focaliseFailures = failures countFocalise
    let focaliseErrors = errors countFocalise
    let focalisePreprocessFailures = failures countFocalisePreprocess
    let focalisePreprocessErrors = errors countFocalisePreprocess

    let autarkifyFailures = failures countAutarkify
    let autarkifyErrors = errors countAutarkify
    let autarkifyPreprocessFailures = failures countAutarkifyPreprocess
    let autarkifyPreprocessErrors = errors countAutarkifyPreprocess

    -- let distributeFailures = failures countDistribute
    -- let distributeErrors = errors countDistribute
    -- let distributePreprocessFailures = failures countDistributePreprocess
    -- let distributePreprocessErrors = errors countDistributePreprocess

    let boolCombFailures = failures countBoolComb
    let boolCombErrors = errors countBoolComb


    let totalFailures = binariseFailures + focaliseFailures + autarkifyFailures + {- distributeFailures + -} boolCombFailures
    let totalErrors = binariseErrors + focaliseErrors + autarkifyErrors + {- distributeErrors + -} boolCombErrors
    let totalPreprocessFailures = binarisePreprocessFailures + focalisePreprocessFailures +  autarkifyPreprocessFailures-- + distributePreprocessFailures
    let totalPreprocessErrors = binarisePreprocessErrors + focalisePreprocessErrors +  autarkifyPreprocessErrors-- + distributePreprocessErrors

    putStrLn $ "Total number failures: " ++ show totalFailures
    putStrLn $ "Total number errors: " ++ show totalErrors
    putStrLn $ "Total number failures with preprocess: " ++ show totalPreprocessFailures
    putStrLn $ "Total number errors with preprocess: " ++ show totalPreprocessErrors

    when (totalFailures + totalErrors + totalPreprocessFailures + totalPreprocessErrors > 0) exitFailure
