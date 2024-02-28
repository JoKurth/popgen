module Tests.FocaliseTests(
    tests,
    testsPreprocess
) where


import InputOutput.MapInput (stringToPredicate)
import InputOutput.MapOutput (populationComputerToPopSim)
import PresburgerMapper.DoublePhi (doublePredicate)
import PresburgerMapper.Union (constructPC)
import Transformer.Preprocess (preprocess)
import Transformer.Binarise (binarise)
import Transformer.Focalise (focalise)

import qualified Types.PopulationComputer as PC

import Tests.TestHelper
import Tests.TestCases

import Test.HUnit


-- Helper

createProtocol :: String -> PC.PopulationComputer String
createProtocol = focalise . binarise . constructPC . doublePredicate . stringToPredicate

createPreprocessedProtocol :: String -> PC.PopulationComputer String
createPreprocessedProtocol = focalise . binarise . preprocess . constructPC . doublePredicate . stringToPredicate


buildTestCase :: (TestGenerator -> ProtocolGenerator -> Bool -> IO Test) -> IO Test
buildTestCase abstractTest = abstractTest genericPcTest createProtocol False

buildPreprocessTestCase :: (TestGenerator -> ProtocolGenerator -> Bool -> IO Test) -> IO Test
buildPreprocessTestCase abstractTest = abstractTest genericPcTest createPreprocessedProtocol True


ioTestListToIoListTest :: [IO Test] -> IO [Test]
ioTestListToIoListTest [] = return []
ioTestListToIoListTest (x:xs) = do
    test <- x
    otherTests <- ioTestListToIoListTest xs
    return $ test : otherTests


-- Tests

tests = do
    ioList <- ioTestListToIoListTest $ map buildTestCase genericBinFocTests
    return $ TestList ioList

testsPreprocess = do
    ioList <- ioTestListToIoListTest $ map buildPreprocessTestCase genericBinFocTests
    return $ TestList ioList
