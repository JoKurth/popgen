module Tests.DistributeTests (
    tests
    -- testsPreprocess
) where

import InputOutput.MapInput (stringToPredicate)
import InputOutput.MapOutput (populationProtocolToPopSim, stringPcToIntPc)
import PresburgerMapper.DoublePhi (doublePredicate)
import PresburgerMapper.Union (constructPC)
import Transformer.Preprocess (preprocess)
import Transformer.Binarise (binarise)
import Transformer.Focalise (focalise)
import Transformer.Autarkify (autarkify)
import Transformer.Distribute (distribute)
import qualified Types.PopulationComputer as PC

import Helper.Tuple
import Tests.TestHelper

import Test.HUnit

-- Tests

-- createProtocol :: String -> PC.PopulationProtocol String
createProtocol = distribute . stringPcToIntPc . autarkify . focalise . binarise . constructPC . doublePredicate . stringToPredicate

-- createPreprocessedProtocol :: String -> PC.PopulationProtocol String
createPreprocessedProtocol = distribute . stringPcToIntPc . autarkify . focalise . binarise . preprocess . constructPC . doublePredicate . stringToPredicate
-- we can only preprocess the last three elements of each test cluster


-- buildTestCase :: (TestGenerator -> ProtocolGenerator -> Bool -> IO Test) -> IO Test
buildTestCase abstractTest = abstractTest genericPpTest createProtocol False

-- buildPreprocessTestCase :: (TestGenerator -> ProtocolGenerator -> Bool -> IO Test) -> IO Test
buildPreprocessTestCase abstractTest = abstractTest genericPpTest createPreprocessedProtocol True


ioTestListToIoListTest :: [IO Test] -> IO [Test]
ioTestListToIoListTest [] = return []
ioTestListToIoListTest (x:xs) = do
    test <- x
    otherTests <- ioTestListToIoListTest xs
    return $ test : otherTests


-- tests = do
--     ioList <- ioTestListToIoListTest $ map buildTestCase genericAutDistTests
--     return $ TestList ioList

-- testsPreprocess = do
--     ioList <- ioTestListToIoListTest $ map buildPreprocessTestCase $ drop 3 genericAutDistTests
--     return $ TestList ioList

-- This is an example test.
-- We cannot execute any of our other test cases
tests = do
    testData <- genericPpTest createProtocol "[1];2;1" [59] True False
    return $ TestCase (uncurry assertBool testData)
