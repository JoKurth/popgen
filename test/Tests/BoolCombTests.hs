module Tests.BoolCombTests (
    tests
) where


import InputOutput.MapInput (stringToPredicate)
import InputOutput.MapOutput (populationComputerToPopSim, stringPcToIntPc)
import PresburgerMapper.DoublePhi (doublePredicate)
import PresburgerMapper.Union (constructPC)
import Transformer.Preprocess (preprocess)
import Transformer.Binarise (binarise)
import Transformer.Focalise (focalise)
import Transformer.Autarkify (autarkify)
import Transformer.Distribute (distribute)

import qualified Types.PopulationComputer as PC

import Tests.TestHelper
import Tests.TestCases

import Test.HUnit


-- Helper

-- createProtocol :: String -> PC.PopulationProtocol Int
createProtocol :: String -> PC.PopulationComputer String
createProtocol = {- distribute . stringPcToIntPc . -} autarkify . focalise . binarise . preprocess . constructPC . doublePredicate . stringToPredicate


buildTestCase :: (TestGenerator -> ProtocolGenerator -> Bool -> IO Test) -> IO Test
buildTestCase abstractTest = abstractTest genericPcTest createProtocol True


ioTestListToIoListTest :: [IO Test] -> IO [Test]
ioTestListToIoListTest [] = return []
ioTestListToIoListTest (x:xs) = do
    test <- x
    otherTests <- ioTestListToIoListTest xs
    return $ test : otherTests


-- Tests

tests = do
    ioList <- ioTestListToIoListTest $ map buildTestCase genericOpAutDistTest
    return $ TestList ioList
