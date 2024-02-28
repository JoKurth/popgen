{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Tests.TestCases
where

import Test.HUnit
import qualified Types.PopulationComputer as PC


type TestGenerator = ((String -> PC.PopulationComputer String) -> String -> [Int] -> Bool -> Bool -> IO (String, Bool))
type ProtocolGenerator = (String -> PC.PopulationComputer String)


abstractTest :: TestGenerator -> ProtocolGenerator -> String -> [Int] -> Bool -> Bool -> IO Test
abstractTest testType createProtocol protocolString input expectedOutput isPreprocessed = do
    testData <- testType createProtocol protocolString input expectedOutput isPreprocessed
    return $ TestCase (uncurry assertBool testData)



---------------------------------------------
--      Tests for Binarise/Focalise
---------------------------------------------


-- Remainder tests

genericBinFocTest_RP_1_1_1 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [0, 0] True
genericBinFocTest_RP_1_1_2 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [1, 0] False
genericBinFocTest_RP_1_1_3 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [0, 1] True
genericBinFocTest_RP_1_1_4 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [1, 1] False
genericBinFocTest_RP_1_1_5 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [0, 2] True
genericBinFocTest_RP_1_1_6 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [1, 20] False
genericBinFocTest_RP_1_1 = [genericBinFocTest_RP_1_1_1, genericBinFocTest_RP_1_1_2, genericBinFocTest_RP_1_1_3, genericBinFocTest_RP_1_1_4, genericBinFocTest_RP_1_1_5, genericBinFocTest_RP_1_1_6]

genericBinFocTest_RP_1_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_1_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_1_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [1, 0, 1, 0, 1, 0] True
genericBinFocTest_RP_1_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [1, 0, 1, 16, 1, 4] False
genericBinFocTest_RP_1_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [0, 2, 0, 2, 0, 2] True
genericBinFocTest_RP_1_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [1, 2, 1, 5, 1, 6] False
genericBinFocTest_RP_1_2 = [genericBinFocTest_RP_1_2_1, genericBinFocTest_RP_1_2_2, genericBinFocTest_RP_1_2_3, genericBinFocTest_RP_1_2_4, genericBinFocTest_RP_1_2_5, genericBinFocTest_RP_1_2_6]

genericBinFocTest_RP_1_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_1_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_1_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [1, 0, 1, 0, 1, 0] True
genericBinFocTest_RP_1_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [1, 0, 1, 16, 1, 4] True
genericBinFocTest_RP_1_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [0, 2, 0, 1, 0, 1] True
genericBinFocTest_RP_1_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [1, 2, 1, 5, 0, 6] False
genericBinFocTest_RP_1_3 = [genericBinFocTest_RP_1_3_1, genericBinFocTest_RP_1_3_2, genericBinFocTest_RP_1_3_3, genericBinFocTest_RP_1_3_4, genericBinFocTest_RP_1_3_5, genericBinFocTest_RP_1_3_6]

genericBinFocTest_RP_1_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_1_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_1_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [1, 0, 1, 0, 1, 0] False
genericBinFocTest_RP_1_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [1, 0, 1, 16, 1, 4] False
genericBinFocTest_RP_1_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [0, 0, 0, 0, 1, 34] True
genericBinFocTest_RP_1_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [1, 2, 1, 5, 1, 6] False
genericBinFocTest_RP_1_4 = [genericBinFocTest_RP_1_4_1, genericBinFocTest_RP_1_4_2, genericBinFocTest_RP_1_4_3, genericBinFocTest_RP_1_4_4, genericBinFocTest_RP_1_4_5, genericBinFocTest_RP_1_4_6]

genericBinFocTest_RP_1 = genericBinFocTest_RP_1_1 ++ genericBinFocTest_RP_1_2 ++ genericBinFocTest_RP_1_3 ++ genericBinFocTest_RP_1_4


genericBinFocTest_RP_2_1_1 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [0, 0] False
genericBinFocTest_RP_2_1_2 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [1, 0] True
genericBinFocTest_RP_2_1_3 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [0, 1] False
genericBinFocTest_RP_2_1_4 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [1, 1] True
genericBinFocTest_RP_2_1_5 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [0, 2] False
genericBinFocTest_RP_2_1_6 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [1, 20] True
genericBinFocTest_RP_2_1 = [genericBinFocTest_RP_2_1_1, genericBinFocTest_RP_2_1_2, genericBinFocTest_RP_2_1_3, genericBinFocTest_RP_2_1_4, genericBinFocTest_RP_2_1_5, genericBinFocTest_RP_2_1_6]

genericBinFocTest_RP_2_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_2_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_2_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [1, 0, 1, 1, 1, 0] True
genericBinFocTest_RP_2_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [1, 0, 1, 16, 1, 4] False
genericBinFocTest_RP_2_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [0, 2, 0, 3, 0, 2] True
genericBinFocTest_RP_2_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [1, 2, 1, 6, 1, 7] True
genericBinFocTest_RP_2_2 = [genericBinFocTest_RP_2_2_1, genericBinFocTest_RP_2_2_2, genericBinFocTest_RP_2_2_3, genericBinFocTest_RP_2_2_4, genericBinFocTest_RP_2_2_5, genericBinFocTest_RP_2_2_6]

genericBinFocTest_RP_2_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_2_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_2_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [1, 0, 1, 0, 1, 0] False
genericBinFocTest_RP_2_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [1, 0, 1, 16, 1, 4] False
genericBinFocTest_RP_2_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [0, 2, 0, 1, 1, 0] True
genericBinFocTest_RP_2_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [1, 2, 1, 5, 0, 6] True
genericBinFocTest_RP_2_3 = [genericBinFocTest_RP_2_3_1, genericBinFocTest_RP_2_3_2, genericBinFocTest_RP_2_3_3, genericBinFocTest_RP_2_3_4, genericBinFocTest_RP_2_3_5, genericBinFocTest_RP_2_3_6]

genericBinFocTest_RP_2_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_2_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_2_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [1, 0, 1, 0, 1, 0] False
genericBinFocTest_RP_2_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [0, 0, 1, 8, 1, 13] True
genericBinFocTest_RP_2_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [0, 0, 0, 0, 1, 34] False
genericBinFocTest_RP_2_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [1, 2, 1, 5, 1, 6] False
genericBinFocTest_RP_2_4 = [genericBinFocTest_RP_2_4_1, genericBinFocTest_RP_2_4_2, genericBinFocTest_RP_2_4_3, genericBinFocTest_RP_2_4_4, genericBinFocTest_RP_2_4_5, genericBinFocTest_RP_2_4_6]

genericBinFocTest_RP_2 = genericBinFocTest_RP_2_1 ++ genericBinFocTest_RP_2_2 ++ genericBinFocTest_RP_2_3 ++ genericBinFocTest_RP_2_4


genericBinFocTest_RP_3_1_1 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [0, 0] True
genericBinFocTest_RP_3_1_2 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [1, 0] True
genericBinFocTest_RP_3_1_3 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [0, 1] True
genericBinFocTest_RP_3_1_4 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [1, 1] True
genericBinFocTest_RP_3_1_5 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [0, 2] True
genericBinFocTest_RP_3_1_6 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [1, 20] True
genericBinFocTest_RP_3_1 = [genericBinFocTest_RP_3_1_1, genericBinFocTest_RP_3_1_2, genericBinFocTest_RP_3_1_3, genericBinFocTest_RP_3_1_4, genericBinFocTest_RP_3_1_5, genericBinFocTest_RP_3_1_6]

genericBinFocTest_RP_3_2_1 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_3_2_2 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_3_2_3 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [1, 0, 1, 0, 1, 0] False
genericBinFocTest_RP_3_2_4 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [1, 0, 1, 16, 1, 4] False
genericBinFocTest_RP_3_2_5 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [0, 2, 0, 1, 1, 0] True
genericBinFocTest_RP_3_2_6 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [1, 2, 1, 5, 1, 7] True
genericBinFocTest_RP_3_2 = [genericBinFocTest_RP_3_2_1, genericBinFocTest_RP_3_2_2, genericBinFocTest_RP_3_2_3, genericBinFocTest_RP_3_2_4, genericBinFocTest_RP_3_2_5, genericBinFocTest_RP_3_2_6]

genericBinFocTest_RP_3_3_1 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_3_3_2 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_3_3_3 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [1, 0, 1, 0, 1, 0] False
genericBinFocTest_RP_3_3_4 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [1, 0, 1, 16, 1, 4] False
genericBinFocTest_RP_3_3_5 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [0, 2, 0, 1, 0, 1] True
genericBinFocTest_RP_3_3_6 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [1, 2, 1, 5, 0, 6] False
genericBinFocTest_RP_3_3 = [genericBinFocTest_RP_3_3_1, genericBinFocTest_RP_3_3_2, genericBinFocTest_RP_3_3_3, genericBinFocTest_RP_3_3_4, genericBinFocTest_RP_3_3_5, genericBinFocTest_RP_3_3_6]

genericBinFocTest_RP_3_4_1 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_3_4_2 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_3_4_3 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [1, 0, 1, 0, 1, 0] False
genericBinFocTest_RP_3_4_4 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [1, 0, 1, 16, 1, 4] False
genericBinFocTest_RP_3_4_5 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [0, 0, 0, 0, 1, 34] True
genericBinFocTest_RP_3_4_6 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [1, 2, 1, 5, 1, 6] False
genericBinFocTest_RP_3_4 = [genericBinFocTest_RP_3_4_1, genericBinFocTest_RP_3_4_2, genericBinFocTest_RP_3_4_3, genericBinFocTest_RP_3_4_4, genericBinFocTest_RP_3_4_5, genericBinFocTest_RP_3_4_6]

genericBinFocTest_RP_3 = genericBinFocTest_RP_3_1 ++ genericBinFocTest_RP_3_2 ++ genericBinFocTest_RP_3_3 ++ genericBinFocTest_RP_3_4


genericBinFocTest_RP_4_1_1 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [0, 0] False
genericBinFocTest_RP_4_1_2 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [1, 0] False
genericBinFocTest_RP_4_1_3 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [0, 1] False
genericBinFocTest_RP_4_1_4 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [1, 1] False
genericBinFocTest_RP_4_1_5 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [0, 2] False
genericBinFocTest_RP_4_1_6 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [1, 20] False
genericBinFocTest_RP_4_1 = [genericBinFocTest_RP_4_1_1, genericBinFocTest_RP_4_1_2, genericBinFocTest_RP_4_1_3, genericBinFocTest_RP_4_1_4, genericBinFocTest_RP_4_1_5, genericBinFocTest_RP_4_1_6]

genericBinFocTest_RP_4_2_1 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_4_2_2 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_4_2_3 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [1, 0, 1, 1, 1, 0] False
genericBinFocTest_RP_4_2_4 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [1, 0, 1, 16, 1, 3] True
genericBinFocTest_RP_4_2_5 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [0, 2, 0, 2, 1, 2] True
genericBinFocTest_RP_4_2_6 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [1, 2, 1, 5, 1, 9] True
genericBinFocTest_RP_4_2 = [genericBinFocTest_RP_4_2_1, genericBinFocTest_RP_4_2_2, genericBinFocTest_RP_4_2_3, genericBinFocTest_RP_4_2_4, genericBinFocTest_RP_4_2_5, genericBinFocTest_RP_4_2_6]

genericBinFocTest_RP_4_3_1 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_4_3_2 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_RP_4_3_3 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [1, 0, 1, 0, 1, 0] True
genericBinFocTest_RP_4_3_4 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [1, 0, 1, 16, 1, 4] True
genericBinFocTest_RP_4_3_5 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [0, 2, 0, 1, 1, 0] False
genericBinFocTest_RP_4_3_6 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [1, 2, 1, 5, 0, 6] True
genericBinFocTest_RP_4_3 = [genericBinFocTest_RP_4_3_1, genericBinFocTest_RP_4_3_2, genericBinFocTest_RP_4_3_3, genericBinFocTest_RP_4_3_4, genericBinFocTest_RP_4_3_5, genericBinFocTest_RP_4_3_6]

genericBinFocTest_RP_4_4_1 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_4_4_2 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_RP_4_4_3 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [1, 0, 1, 0, 1, 0] False
genericBinFocTest_RP_4_4_4 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [0, 0, 0, 1, 0, 41] True
genericBinFocTest_RP_4_4_5 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [0, 0, 0, 1, 1, 6] True
genericBinFocTest_RP_4_4_6 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [1, 2, 1, 5, 1, 6] False
genericBinFocTest_RP_4_4 = [genericBinFocTest_RP_4_4_1, genericBinFocTest_RP_4_4_2, genericBinFocTest_RP_4_4_3, genericBinFocTest_RP_4_4_4, genericBinFocTest_RP_4_4_5, genericBinFocTest_RP_4_4_6]

genericBinFocTest_RP_4 = genericBinFocTest_RP_4_1 ++ genericBinFocTest_RP_4_2 ++ genericBinFocTest_RP_4_3 ++ genericBinFocTest_RP_4_4


-- Threshold tests

genericBinFocTest_TP_1_1_1 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [0, 0] True
genericBinFocTest_TP_1_1_2 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [1, 0] True
genericBinFocTest_TP_1_1_3 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [0, 1] False
genericBinFocTest_TP_1_1_4 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [1, 1] False
genericBinFocTest_TP_1_1_5 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [0, 5] False
genericBinFocTest_TP_1_1_6 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [1, 10] False
genericBinFocTest_TP_1_1 = [genericBinFocTest_TP_1_1_1, genericBinFocTest_TP_1_1_2, genericBinFocTest_TP_1_1_3, genericBinFocTest_TP_1_1_4, genericBinFocTest_TP_1_1_5, genericBinFocTest_TP_1_1_6]

genericBinFocTest_TP_1_2_1 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_1_2_2 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [0, 0, 0, 1, 0, 0] True
genericBinFocTest_TP_1_2_3 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_1_2_4 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_1_2_5 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [1, 5, 1, 6, 0, 4] False
genericBinFocTest_TP_1_2_6 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [1, 12, 1, 3, 0, 1] False
genericBinFocTest_TP_1_2 = [genericBinFocTest_TP_1_2_1, genericBinFocTest_TP_1_2_2, genericBinFocTest_TP_1_2_3, genericBinFocTest_TP_1_2_4, genericBinFocTest_TP_1_2_5, genericBinFocTest_TP_1_2_6]

genericBinFocTest_TP_1_3_1 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_1_3_2 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [0, 0, 0, 0, 1, 0] False
genericBinFocTest_TP_1_3_3 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [0, 0, 1, 0, 0, 1] False
genericBinFocTest_TP_1_3_4 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_1_3_5 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [1, 5, 1, 6, 0, 4] False
genericBinFocTest_TP_1_3_6 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [1, 12, 1, 3, 0, 1] False
genericBinFocTest_TP_1_3 = [genericBinFocTest_TP_1_3_1, genericBinFocTest_TP_1_3_2, genericBinFocTest_TP_1_3_3, genericBinFocTest_TP_1_3_4, genericBinFocTest_TP_1_3_5, genericBinFocTest_TP_1_3_6]

genericBinFocTest_TP_1_4_1 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_1_4_2 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [0, 0, 0, 0, 1, 0] True
genericBinFocTest_TP_1_4_3 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [0, 0, 0, 0, 1, 1] True
genericBinFocTest_TP_1_4_4 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [0, 0, 0, 0, 0, 2] False
genericBinFocTest_TP_1_4_5 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [1, 5, 1, 6, 0, 4] False
genericBinFocTest_TP_1_4_6 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [1, 12, 1, 3, 0, 1] False
genericBinFocTest_TP_1_4 = [genericBinFocTest_TP_1_4_1, genericBinFocTest_TP_1_4_2, genericBinFocTest_TP_1_4_3, genericBinFocTest_TP_1_4_4, genericBinFocTest_TP_1_4_5, genericBinFocTest_TP_1_4_6]

genericBinFocTest_TP_1 = genericBinFocTest_TP_1_1 ++ genericBinFocTest_TP_1_2 ++ genericBinFocTest_TP_1_3 ++ genericBinFocTest_TP_1_4


genericBinFocTest_TP_2_1_1 testType createProtocol = abstractTest testType createProtocol "[-1];0" [0, 0] True
genericBinFocTest_TP_2_1_2 testType createProtocol = abstractTest testType createProtocol "[-1];0" [1, 0] False
genericBinFocTest_TP_2_1_3 testType createProtocol = abstractTest testType createProtocol "[-1];0" [0, 1] False
genericBinFocTest_TP_2_1_4 testType createProtocol = abstractTest testType createProtocol "[-1];0" [1, 1] False
genericBinFocTest_TP_2_1_5 testType createProtocol = abstractTest testType createProtocol "[-1];0" [1, 2] False
genericBinFocTest_TP_2_1_6 testType createProtocol = abstractTest testType createProtocol "[-1];0" [1, 15] False
genericBinFocTest_TP_2_1 = [genericBinFocTest_TP_2_1_1, genericBinFocTest_TP_2_1_2, genericBinFocTest_TP_2_1_3, genericBinFocTest_TP_2_1_4, genericBinFocTest_TP_2_1_5, genericBinFocTest_TP_2_1_6]

genericBinFocTest_TP_2_2_1 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_2_2_2 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_2_2_3 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [0, 161, 0, 0, 0, 0] False
genericBinFocTest_TP_2_2_4 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_2_2_5 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_2_2_6 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [0, 0, 1, 0, 0, 1] False
genericBinFocTest_TP_2_2 = [genericBinFocTest_TP_2_2_1, genericBinFocTest_TP_2_2_2, genericBinFocTest_TP_2_2_3, genericBinFocTest_TP_2_2_4, genericBinFocTest_TP_2_2_5, genericBinFocTest_TP_2_2_6]

genericBinFocTest_TP_2_3_1 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_2_3_2 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_2_3_3 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [0, 161, 0, 0, 0, 0] False
genericBinFocTest_TP_2_3_4 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_2_3_5 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_2_3_6 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [0, 0, 1, 0, 0, 1] False
genericBinFocTest_TP_2_3 = [genericBinFocTest_TP_2_3_1, genericBinFocTest_TP_2_3_2, genericBinFocTest_TP_2_3_3, genericBinFocTest_TP_2_3_4, genericBinFocTest_TP_2_3_5, genericBinFocTest_TP_2_3_6]

genericBinFocTest_TP_2_4_1 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_2_4_2 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_2_4_3 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [0, 161, 0, 0, 0, 0] False
genericBinFocTest_TP_2_4_4 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_2_4_5 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_2_4_6 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [0, 0, 1, 0, 0, 1] False
genericBinFocTest_TP_2_4 = [genericBinFocTest_TP_2_4_1, genericBinFocTest_TP_2_4_2, genericBinFocTest_TP_2_4_3, genericBinFocTest_TP_2_4_4, genericBinFocTest_TP_2_4_5, genericBinFocTest_TP_2_4_6]

genericBinFocTest_TP_2 = genericBinFocTest_TP_2_1 ++ genericBinFocTest_TP_2_2 ++ genericBinFocTest_TP_2_3 ++ genericBinFocTest_TP_2_4


genericBinFocTest_TP_3_1_1 testType createProtocol = abstractTest testType createProtocol "[-1];1" [0, 0] False
genericBinFocTest_TP_3_1_2 testType createProtocol = abstractTest testType createProtocol "[-1];1" [1, 0] False
genericBinFocTest_TP_3_1_3 testType createProtocol = abstractTest testType createProtocol "[-1];1" [0, 1] False
genericBinFocTest_TP_3_1_4 testType createProtocol = abstractTest testType createProtocol "[-1];1" [1, 1] False
genericBinFocTest_TP_3_1_5 testType createProtocol = abstractTest testType createProtocol "[-1];1" [1, 2] False
genericBinFocTest_TP_3_1_6 testType createProtocol = abstractTest testType createProtocol "[-1];1" [1, 15] False
genericBinFocTest_TP_3_1 = [genericBinFocTest_TP_3_1_1, genericBinFocTest_TP_3_1_2, genericBinFocTest_TP_3_1_3, genericBinFocTest_TP_3_1_4, genericBinFocTest_TP_3_1_5, genericBinFocTest_TP_3_1_6]

genericBinFocTest_TP_3_2_1 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_3_2_2 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_3_2_3 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [0, 161, 0, 0, 0, 0] False
genericBinFocTest_TP_3_2_4 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_3_2_5 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_3_2_6 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [0, 0, 1, 0, 0, 1] False
genericBinFocTest_TP_3_2 = [genericBinFocTest_TP_3_2_1, genericBinFocTest_TP_3_2_2, genericBinFocTest_TP_3_2_3, genericBinFocTest_TP_3_2_4, genericBinFocTest_TP_3_2_5, genericBinFocTest_TP_3_2_6]

genericBinFocTest_TP_3_3_1 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_3_3_2 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_3_3_3 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [0, 161, 0, 0, 0, 0] False
genericBinFocTest_TP_3_3_4 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_3_3_5 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_3_3_6 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [0, 0, 1, 0, 0, 1] False
genericBinFocTest_TP_3_3 = [genericBinFocTest_TP_3_3_1, genericBinFocTest_TP_3_3_2, genericBinFocTest_TP_3_3_3, genericBinFocTest_TP_3_3_4, genericBinFocTest_TP_3_3_5, genericBinFocTest_TP_3_3_6]

genericBinFocTest_TP_3_4_1 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_3_4_2 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [1, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_3_4_3 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [0, 161, 0, 0, 0, 0] False
genericBinFocTest_TP_3_4_4 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_3_4_5 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_3_4_6 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [0, 0, 1, 0, 0, 1] False
genericBinFocTest_TP_3_4 = [genericBinFocTest_TP_3_4_1, genericBinFocTest_TP_3_4_2, genericBinFocTest_TP_3_4_3, genericBinFocTest_TP_3_4_4, genericBinFocTest_TP_3_4_5, genericBinFocTest_TP_3_4_6]

genericBinFocTest_TP_3 = genericBinFocTest_TP_3_1 ++ genericBinFocTest_TP_3_2 ++ genericBinFocTest_TP_3_3 ++ genericBinFocTest_TP_3_4


genericBinFocTest_TP_4_1_1 testType createProtocol = abstractTest testType createProtocol "[1];-1" [0, 0] True
genericBinFocTest_TP_4_1_2 testType createProtocol = abstractTest testType createProtocol "[1];-1" [1, 0] True
genericBinFocTest_TP_4_1_3 testType createProtocol = abstractTest testType createProtocol "[1];-1" [0, 1] True
genericBinFocTest_TP_4_1_4 testType createProtocol = abstractTest testType createProtocol "[1];-1" [1, 1] True
genericBinFocTest_TP_4_1_5 testType createProtocol = abstractTest testType createProtocol "[1];-1" [1, 2] True
genericBinFocTest_TP_4_1_6 testType createProtocol = abstractTest testType createProtocol "[1];-1" [1, 15] True
genericBinFocTest_TP_4_1 = [genericBinFocTest_TP_4_1_1, genericBinFocTest_TP_4_1_2, genericBinFocTest_TP_4_1_3, genericBinFocTest_TP_4_1_4, genericBinFocTest_TP_4_1_5, genericBinFocTest_TP_4_1_6]

genericBinFocTest_TP_4_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_4_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_4_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_4_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_4_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [0, 0, 0, 15, 0, 0] True
genericBinFocTest_TP_4_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_4_2 = [genericBinFocTest_TP_4_2_1, genericBinFocTest_TP_4_2_2, genericBinFocTest_TP_4_2_3, genericBinFocTest_TP_4_2_4, genericBinFocTest_TP_4_2_5, genericBinFocTest_TP_4_2_6]

genericBinFocTest_TP_4_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_4_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_4_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_4_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_4_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [0, 0, 0, 15, 0, 0] True
genericBinFocTest_TP_4_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_4_3 = [genericBinFocTest_TP_4_3_1, genericBinFocTest_TP_4_3_2, genericBinFocTest_TP_4_3_3, genericBinFocTest_TP_4_3_4, genericBinFocTest_TP_4_3_5, genericBinFocTest_TP_4_3_6]

genericBinFocTest_TP_4_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_4_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_4_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_4_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_4_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [0, 0, 0, 15, 0, 0] True
genericBinFocTest_TP_4_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_4_4 = [genericBinFocTest_TP_4_4_1, genericBinFocTest_TP_4_4_2, genericBinFocTest_TP_4_4_3, genericBinFocTest_TP_4_4_4, genericBinFocTest_TP_4_4_5, genericBinFocTest_TP_4_4_6]

genericBinFocTest_TP_4 = genericBinFocTest_TP_4_1 ++ genericBinFocTest_TP_4_2 ++ genericBinFocTest_TP_4_3 ++ genericBinFocTest_TP_4_4


genericBinFocTest_TP_5_1_1 testType createProtocol = abstractTest testType createProtocol "[1];0" [0, 0] True
genericBinFocTest_TP_5_1_2 testType createProtocol = abstractTest testType createProtocol "[1];0" [1, 0] True
genericBinFocTest_TP_5_1_3 testType createProtocol = abstractTest testType createProtocol "[1];0" [0, 1] True
genericBinFocTest_TP_5_1_4 testType createProtocol = abstractTest testType createProtocol "[1];0" [1, 1] True
genericBinFocTest_TP_5_1_5 testType createProtocol = abstractTest testType createProtocol "[1];0" [1, 2] True
genericBinFocTest_TP_5_1_6 testType createProtocol = abstractTest testType createProtocol "[1];0" [1, 15] True
genericBinFocTest_TP_5_1 = [genericBinFocTest_TP_5_1_1, genericBinFocTest_TP_5_1_2, genericBinFocTest_TP_5_1_3, genericBinFocTest_TP_5_1_4, genericBinFocTest_TP_5_1_5, genericBinFocTest_TP_5_1_6]

genericBinFocTest_TP_5_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_5_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_5_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_5_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_5_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [0, 0, 0, 15, 0, 0] True
genericBinFocTest_TP_5_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_5_2 = [genericBinFocTest_TP_5_2_1, genericBinFocTest_TP_5_2_2, genericBinFocTest_TP_5_2_3, genericBinFocTest_TP_5_2_4, genericBinFocTest_TP_5_2_5, genericBinFocTest_TP_5_2_6]

genericBinFocTest_TP_5_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_5_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_5_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_5_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_5_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [0, 0, 0, 15, 0, 0] True
genericBinFocTest_TP_5_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_5_3 = [genericBinFocTest_TP_5_3_1, genericBinFocTest_TP_5_3_2, genericBinFocTest_TP_5_3_3, genericBinFocTest_TP_5_3_4, genericBinFocTest_TP_5_3_5, genericBinFocTest_TP_5_3_6]

genericBinFocTest_TP_5_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_5_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_5_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_5_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_5_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [0, 0, 0, 15, 0, 0] True
genericBinFocTest_TP_5_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_5_4 = [genericBinFocTest_TP_5_4_1, genericBinFocTest_TP_5_4_2, genericBinFocTest_TP_5_4_3, genericBinFocTest_TP_5_4_4, genericBinFocTest_TP_5_4_5, genericBinFocTest_TP_5_4_6]

genericBinFocTest_TP_5 = genericBinFocTest_TP_5_1 ++ genericBinFocTest_TP_5_2 ++ genericBinFocTest_TP_5_3 ++ genericBinFocTest_TP_5_4


genericBinFocTest_TP_6_1_1 testType createProtocol = abstractTest testType createProtocol "[1];1" [0, 0] False
genericBinFocTest_TP_6_1_2 testType createProtocol = abstractTest testType createProtocol "[1];1" [1, 0] True
genericBinFocTest_TP_6_1_3 testType createProtocol = abstractTest testType createProtocol "[1];1" [0, 1] True
genericBinFocTest_TP_6_1_4 testType createProtocol = abstractTest testType createProtocol "[1];1" [1, 1] True
genericBinFocTest_TP_6_1_5 testType createProtocol = abstractTest testType createProtocol "[1];1" [1, 2] True
genericBinFocTest_TP_6_1_6 testType createProtocol = abstractTest testType createProtocol "[1];1" [1, 15] True
genericBinFocTest_TP_6_1 = [genericBinFocTest_TP_6_1_1, genericBinFocTest_TP_6_1_2, genericBinFocTest_TP_6_1_3, genericBinFocTest_TP_6_1_4, genericBinFocTest_TP_6_1_5, genericBinFocTest_TP_6_1_6]

genericBinFocTest_TP_6_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_6_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_6_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_6_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_6_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [0, 0, 0, 15, 0, 0] True
genericBinFocTest_TP_6_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_6_2_7 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [0, 0, 1, 0, 1, 0] False
genericBinFocTest_TP_6_2 = [genericBinFocTest_TP_6_2_1, genericBinFocTest_TP_6_2_2, genericBinFocTest_TP_6_2_3, genericBinFocTest_TP_6_2_4, genericBinFocTest_TP_6_2_5, genericBinFocTest_TP_6_2_6, genericBinFocTest_TP_6_2_7]

genericBinFocTest_TP_6_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_6_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_6_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_6_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_6_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [0, 0, 0, 15, 0, 0] True
genericBinFocTest_TP_6_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_6_3 = [genericBinFocTest_TP_6_3_1, genericBinFocTest_TP_6_3_2, genericBinFocTest_TP_6_3_3, genericBinFocTest_TP_6_3_4, genericBinFocTest_TP_6_3_5, genericBinFocTest_TP_6_3_6]

genericBinFocTest_TP_6_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_6_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_6_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_6_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_6_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [0, 0, 0, 0, 1, 0] False
genericBinFocTest_TP_6_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_6_4_7 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [0, 0, 0, 0, 1, 1] False
genericBinFocTest_TP_6_4 = [genericBinFocTest_TP_6_4_1, genericBinFocTest_TP_6_4_2, genericBinFocTest_TP_6_4_3, genericBinFocTest_TP_6_4_4, genericBinFocTest_TP_6_4_5, genericBinFocTest_TP_6_4_6, genericBinFocTest_TP_6_4_7]

genericBinFocTest_TP_6 = genericBinFocTest_TP_6_1 ++ genericBinFocTest_TP_6_2 ++ genericBinFocTest_TP_6_3 ++ genericBinFocTest_TP_6_4


genericBinFocTest_TP_7_1_1 testType createProtocol = abstractTest testType createProtocol "[0];-1" [0, 0] True
genericBinFocTest_TP_7_1_2 testType createProtocol = abstractTest testType createProtocol "[0];-1" [1, 0] True
genericBinFocTest_TP_7_1_3 testType createProtocol = abstractTest testType createProtocol "[0];-1" [0, 1] True
genericBinFocTest_TP_7_1_4 testType createProtocol = abstractTest testType createProtocol "[0];-1" [1, 1] True
genericBinFocTest_TP_7_1_5 testType createProtocol = abstractTest testType createProtocol "[0];-1" [1, 2] True
genericBinFocTest_TP_7_1_6 testType createProtocol = abstractTest testType createProtocol "[0];-1" [1, 15] True
genericBinFocTest_TP_7_1 = [genericBinFocTest_TP_7_1_1, genericBinFocTest_TP_7_1_2, genericBinFocTest_TP_7_1_3, genericBinFocTest_TP_7_1_4, genericBinFocTest_TP_7_1_5, genericBinFocTest_TP_7_1_6]

genericBinFocTest_TP_7_2_1 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [0, 0, 0, 0] True
genericBinFocTest_TP_7_2_2 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [1, 0, 0, 0] True
genericBinFocTest_TP_7_2_3 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [0, 1, 0, 0] True
genericBinFocTest_TP_7_2_4 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [1, 1, 0, 0] False
genericBinFocTest_TP_7_2_5 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [0, 2, 1, 1] True
genericBinFocTest_TP_7_2_6 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [1, 15, 0, 16] False
genericBinFocTest_TP_7_2 = [genericBinFocTest_TP_7_2_1, genericBinFocTest_TP_7_2_2, genericBinFocTest_TP_7_2_3, genericBinFocTest_TP_7_2_4, genericBinFocTest_TP_7_2_5, genericBinFocTest_TP_7_2_6]

genericBinFocTest_TP_7_3_1 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_7_3_2 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_7_3_4 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_7_3_3 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [0, 0, 0, 1, 0, 0] True
genericBinFocTest_TP_7_3_5 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_7_3_6 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_7_3_7 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [0, 0, 1, 0, 1, 1] False
genericBinFocTest_TP_7_3 = [genericBinFocTest_TP_7_3_1, genericBinFocTest_TP_7_3_2, genericBinFocTest_TP_7_3_3, genericBinFocTest_TP_7_3_4, genericBinFocTest_TP_7_3_5, genericBinFocTest_TP_7_3_6, genericBinFocTest_TP_7_3_7]

genericBinFocTest_TP_7_4_1 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_7_4_2 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_7_4_3 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [0, 0, 0, 161, 0, 0] False
genericBinFocTest_TP_7_4_4 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_7_4_5 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_7_4_6 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [0, 0, 1, 4, 1, 0] False
genericBinFocTest_TP_7_4 = [genericBinFocTest_TP_7_4_1, genericBinFocTest_TP_7_4_2, genericBinFocTest_TP_7_4_3, genericBinFocTest_TP_7_4_4, genericBinFocTest_TP_7_4_5, genericBinFocTest_TP_7_4_6]

genericBinFocTest_TP_7_5_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_7_5_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_7_5_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_7_5_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [0, 0, 0, 0, 0, 10] False
genericBinFocTest_TP_7_5_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [0, 0, 0, 1, 1, 20] False
genericBinFocTest_TP_7_5_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_7_5 = [genericBinFocTest_TP_7_5_1, genericBinFocTest_TP_7_5_2, genericBinFocTest_TP_7_5_3, genericBinFocTest_TP_7_5_4, genericBinFocTest_TP_7_5_5, genericBinFocTest_TP_7_5_6]

genericBinFocTest_TP_7 = {- genericBinFocTest_TP_7_1 ++ -} genericBinFocTest_TP_7_2 ++ genericBinFocTest_TP_7_3 ++ genericBinFocTest_TP_7_4 ++ genericBinFocTest_TP_7_5


genericBinFocTest_TP_8_1_1 testType createProtocol = abstractTest testType createProtocol "[0];0" [0, 0] True
genericBinFocTest_TP_8_1_2 testType createProtocol = abstractTest testType createProtocol "[0];0" [1, 0] True
genericBinFocTest_TP_8_1_3 testType createProtocol = abstractTest testType createProtocol "[0];0" [0, 1] True
genericBinFocTest_TP_8_1_4 testType createProtocol = abstractTest testType createProtocol "[0];0" [1, 1] True
genericBinFocTest_TP_8_1_5 testType createProtocol = abstractTest testType createProtocol "[0];0" [1, 2] True
genericBinFocTest_TP_8_1_6 testType createProtocol = abstractTest testType createProtocol "[0];0" [1, 15] True
genericBinFocTest_TP_8_1 = [genericBinFocTest_TP_8_1_1, genericBinFocTest_TP_8_1_2, genericBinFocTest_TP_8_1_3, genericBinFocTest_TP_8_1_4, genericBinFocTest_TP_8_1_5, genericBinFocTest_TP_8_1_6]

genericBinFocTest_TP_8_2_1 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [0, 0, 0, 0] True
genericBinFocTest_TP_8_2_2 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [1, 0, 0, 0] False
genericBinFocTest_TP_8_2_3 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [0, 1, 0, 2] True
genericBinFocTest_TP_8_2_4 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [1, 1, 0, 0] False
genericBinFocTest_TP_8_2_5 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [1, 0, 0, 1] True
genericBinFocTest_TP_8_2_6 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [1, 15, 0, 16] False
genericBinFocTest_TP_8_2 = [genericBinFocTest_TP_8_2_1, genericBinFocTest_TP_8_2_2, genericBinFocTest_TP_8_2_3, genericBinFocTest_TP_8_2_4, genericBinFocTest_TP_8_2_5, genericBinFocTest_TP_8_2_6]

genericBinFocTest_TP_8_3_1 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_8_3_2 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_8_3_4 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_8_3_3 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [0, 0, 0, 1, 0, 0] False
genericBinFocTest_TP_8_3_5 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_8_3_6 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [1, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_8_3 = [genericBinFocTest_TP_8_3_1, genericBinFocTest_TP_8_3_2, genericBinFocTest_TP_8_3_3, genericBinFocTest_TP_8_3_4, genericBinFocTest_TP_8_3_5, genericBinFocTest_TP_8_3_6]

genericBinFocTest_TP_8_4_1 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_8_4_2 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_8_4_3 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [0, 0, 0, 161, 0, 0] False
genericBinFocTest_TP_8_4_4 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_8_4_5 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [20, 0, 0, 15, 0, 0] True
genericBinFocTest_TP_8_4_6 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [0, 0, 1, 4, 1, 0] False
genericBinFocTest_TP_8_4 = [genericBinFocTest_TP_8_4_1, genericBinFocTest_TP_8_4_2, genericBinFocTest_TP_8_4_3, genericBinFocTest_TP_8_4_4, genericBinFocTest_TP_8_4_5, genericBinFocTest_TP_8_4_6]

genericBinFocTest_TP_8_5_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [0, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_8_5_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_8_5_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_8_5_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [0, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_8_5_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [0, 0, 0, 0, 1, 0] False
genericBinFocTest_TP_8_5_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [0, 0, 1, 0, 0, 10] False
genericBinFocTest_TP_8_5 = [genericBinFocTest_TP_8_5_1, genericBinFocTest_TP_8_5_2, genericBinFocTest_TP_8_5_3, genericBinFocTest_TP_8_5_4, genericBinFocTest_TP_8_5_5, genericBinFocTest_TP_8_5_6]

genericBinFocTest_TP_8 = {- genericBinFocTest_TP_8_1 ++ -} genericBinFocTest_TP_8_2 ++ genericBinFocTest_TP_8_3 ++ genericBinFocTest_TP_8_4 ++ genericBinFocTest_TP_8_5


genericBinFocTest_TP_9_1_1 testType createProtocol = abstractTest testType createProtocol "[0];1" [0, 0] False
genericBinFocTest_TP_9_1_2 testType createProtocol = abstractTest testType createProtocol "[0];1" [1, 0] False
genericBinFocTest_TP_9_1_3 testType createProtocol = abstractTest testType createProtocol "[0];1" [0, 1] False
genericBinFocTest_TP_9_1_4 testType createProtocol = abstractTest testType createProtocol "[0];1" [1, 1] False
genericBinFocTest_TP_9_1_5 testType createProtocol = abstractTest testType createProtocol "[0];1" [1, 2] False
genericBinFocTest_TP_9_1_6 testType createProtocol = abstractTest testType createProtocol "[0];1" [1, 15] False
genericBinFocTest_TP_9_1 = [genericBinFocTest_TP_9_1_1, genericBinFocTest_TP_9_1_2, genericBinFocTest_TP_9_1_3, genericBinFocTest_TP_9_1_4, genericBinFocTest_TP_9_1_5, genericBinFocTest_TP_9_1_6]

genericBinFocTest_TP_9_2_1 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [0, 0, 0, 0] False
genericBinFocTest_TP_9_2_2 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [0, 0, 1, 2] True
genericBinFocTest_TP_9_2_3 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [0, 1, 0, 0] False
genericBinFocTest_TP_9_2_4 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [1, 1, 0, 0] False
genericBinFocTest_TP_9_2_5 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [0, 2, 1, 6] True
genericBinFocTest_TP_9_2_6 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [1, 15, 0, 160] True
genericBinFocTest_TP_9_2 = [genericBinFocTest_TP_9_2_1, genericBinFocTest_TP_9_2_2, genericBinFocTest_TP_9_2_3, genericBinFocTest_TP_9_2_4, genericBinFocTest_TP_9_2_5, genericBinFocTest_TP_9_2_6]

genericBinFocTest_TP_9_3_1 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_9_3_2 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_9_3_4 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [0, 0, 1, 0, 0, 0] False
genericBinFocTest_TP_9_3_3 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [0, 0, 0, 1, 0, 0] False
genericBinFocTest_TP_9_3_5 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [0, 161, 0, 15, 0, 0] True
genericBinFocTest_TP_9_3_6 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [1, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_9_3 = [genericBinFocTest_TP_9_3_1, genericBinFocTest_TP_9_3_2, genericBinFocTest_TP_9_3_3, genericBinFocTest_TP_9_3_4, genericBinFocTest_TP_9_3_5, genericBinFocTest_TP_9_3_6]

genericBinFocTest_TP_9_4_1 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_9_4_2 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_9_4_3 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [0, 0, 0, 161, 0, 0] False
genericBinFocTest_TP_9_4_4 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [1, 0, 1, 0, 0, 0] True
genericBinFocTest_TP_9_4_5 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [0, 0, 0, 15, 0, 0] False
genericBinFocTest_TP_9_4_6 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [0, 2, 1, 4, 1, 0] True
genericBinFocTest_TP_9_4 = [genericBinFocTest_TP_9_4_1, genericBinFocTest_TP_9_4_2, genericBinFocTest_TP_9_4_3, genericBinFocTest_TP_9_4_4, genericBinFocTest_TP_9_4_5, genericBinFocTest_TP_9_4_6]

genericBinFocTest_TP_9_5_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [0, 0, 0, 0, 0, 0] False
genericBinFocTest_TP_9_5_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [1, 0, 0, 0, 0, 0] True
genericBinFocTest_TP_9_5_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [0, 161, 0, 0, 0, 0] True
genericBinFocTest_TP_9_5_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [0, 0, 0, 0, 0, 10] False
genericBinFocTest_TP_9_5_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [0, 0, 0, 1, 1, 20] False
genericBinFocTest_TP_9_5_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [0, 0, 1, 0, 0, 1] True
genericBinFocTest_TP_9_5 = [genericBinFocTest_TP_9_5_1, genericBinFocTest_TP_9_5_2, genericBinFocTest_TP_9_5_3, genericBinFocTest_TP_9_5_4, genericBinFocTest_TP_9_5_5, genericBinFocTest_TP_9_5_6]

genericBinFocTest_TP_9 = {- genericBinFocTest_TP_9_1 ++ -} genericBinFocTest_TP_9_2 ++ genericBinFocTest_TP_9_3 ++ genericBinFocTest_TP_9_4 ++ genericBinFocTest_TP_9_5



-----------------------------------------
--    Tests for Autarkify/Distribute
-----------------------------------------


-- Remainder tests

-- 30/41 helper
genericAutDistTest_RP_1_1_1 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [60] True
genericAutDistTest_RP_1_1_2 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [61] False
genericAutDistTest_RP_1_1_3 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [62] True
genericAutDistTest_RP_1_1_4 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [85] False
genericAutDistTest_RP_1_1_5 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [86] True
genericAutDistTest_RP_1_1_6 testType createProtocol = abstractTest testType createProtocol "[1];2;0" [123] False
genericAutDistTest_RP_1_1 = [genericAutDistTest_RP_1_1_1, genericAutDistTest_RP_1_1_2, genericAutDistTest_RP_1_1_3, genericAutDistTest_RP_1_1_4, genericAutDistTest_RP_1_1_5, genericAutDistTest_RP_1_1_6]

-- 95/122 helper
genericAutDistTest_RP_1_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [1, 0, 190] True
genericAutDistTest_RP_1_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [1, 0, 191] False
genericAutDistTest_RP_1_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [1, 193, 2] True
genericAutDistTest_RP_1_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [61, 103, 83] False
genericAutDistTest_RP_1_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [104, 92, 95] True
genericAutDistTest_RP_1_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;0" [95, 110, 103] False
genericAutDistTest_RP_1_2 = [genericAutDistTest_RP_1_2_1, genericAutDistTest_RP_1_2_2, genericAutDistTest_RP_1_2_3, genericAutDistTest_RP_1_2_4, genericAutDistTest_RP_1_2_5, genericAutDistTest_RP_1_2_6]

-- 46/73 helper
genericAutDistTest_RP_1_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [23, 48, 23] True
genericAutDistTest_RP_1_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [24, 48, 23] False
genericAutDistTest_RP_1_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [24, 49, 24] True
genericAutDistTest_RP_1_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [147, 33, 9] True
genericAutDistTest_RP_1_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [74, 72, 72] True
genericAutDistTest_RP_1_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;0" [75, 81, 92] False
genericAutDistTest_RP_1_3 = [genericAutDistTest_RP_1_3_1, genericAutDistTest_RP_1_3_2, genericAutDistTest_RP_1_3_3, genericAutDistTest_RP_1_3_4, genericAutDistTest_RP_1_3_5, genericAutDistTest_RP_1_3_6]

-- 183/210 helper
-- genericAutDistTest_RP_1_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [x, x, x] b
-- genericAutDistTest_RP_1_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [x, x, x] b
-- genericAutDistTest_RP_1_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [x, x, x] b
-- genericAutDistTest_RP_1_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [x, x, x] b
-- genericAutDistTest_RP_1_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [x, x, x] b
-- genericAutDistTest_RP_1_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;0" [x, x, x] b
-- genericAutDistTest_RP_1_4 = [genericAutDistTest_RP_1_4_1, genericAutDistTest_RP_1_4_2, genericAutDistTest_RP_1_4_3, genericAutDistTest_RP_1_4_4, genericAutDistTest_RP_1_4_5, genericAutDistTest_RP_1_4_6]

genericAutDistTest_RP_1 = genericAutDistTest_RP_1_1 ++ genericAutDistTest_RP_1_2 ++ genericAutDistTest_RP_1_3 -- ++ genericAutDistTest_RP_1_4


-- 29/40 helper
genericAutDistTest_RP_2_1_1 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [58] False
genericAutDistTest_RP_2_1_2 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [59] True
genericAutDistTest_RP_2_1_3 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [60] False
genericAutDistTest_RP_2_1_4 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [81] True
genericAutDistTest_RP_2_1_5 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [82] False
genericAutDistTest_RP_2_1_6 testType createProtocol = abstractTest testType createProtocol "[1];2;1" [119] True
genericAutDistTest_RP_2_1 = [genericAutDistTest_RP_2_1_1, genericAutDistTest_RP_2_1_2, genericAutDistTest_RP_2_1_3, genericAutDistTest_RP_2_1_4, genericAutDistTest_RP_2_1_5, genericAutDistTest_RP_2_1_6]

-- 94/121 helper
genericAutDistTest_RP_2_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [69, 69, 69] False
genericAutDistTest_RP_2_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [76, 69, 69] False
genericAutDistTest_RP_2_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [75, 69, 69] True
genericAutDistTest_RP_2_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [82, 115, 82] False
genericAutDistTest_RP_2_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [82, 115, 86] True
genericAutDistTest_RP_2_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4" [82, 113, 101] True
genericAutDistTest_RP_2_2 = [genericAutDistTest_RP_2_2_1, genericAutDistTest_RP_2_2_2, genericAutDistTest_RP_2_2_3, genericAutDistTest_RP_2_2_4, genericAutDistTest_RP_2_2_5, genericAutDistTest_RP_2_2_6]

-- 45/72 helper
genericAutDistTest_RP_2_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [30, 30, 30] False
genericAutDistTest_RP_2_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [31, 30, 30] True
genericAutDistTest_RP_2_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [31, 31, 31] False
genericAutDistTest_RP_2_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [49, 81, 57] False
genericAutDistTest_RP_2_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [52, 50, 49] True
genericAutDistTest_RP_2_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];2;1" [53, 59, 60] True
genericAutDistTest_RP_2_3 = [genericAutDistTest_RP_2_3_1, genericAutDistTest_RP_2_3_2, genericAutDistTest_RP_2_3_3, genericAutDistTest_RP_2_3_4, genericAutDistTest_RP_2_3_5, genericAutDistTest_RP_2_3_6]

-- 180/207 helper
-- genericAutDistTest_RP_2_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_2_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_2_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_2_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_2_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_2_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_2_4 = [genericAutDistTest_RP_2_4_1, genericAutDistTest_RP_2_4_2, genericAutDistTest_RP_2_4_3, genericAutDistTest_RP_2_4_4, genericAutDistTest_RP_2_4_5, genericAutDistTest_RP_2_4_6]

genericAutDistTest_RP_2 = genericAutDistTest_RP_2_1 ++ genericAutDistTest_RP_2_2 ++ genericAutDistTest_RP_2_3 -- ++ genericAutDistTest_RP_2_4


-- 30/41 helper
genericAutDistTest_RP_3_1_1 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [60] True
genericAutDistTest_RP_3_1_2 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [61] True
genericAutDistTest_RP_3_1_3 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [62] True
genericAutDistTest_RP_3_1_4 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [85] True
genericAutDistTest_RP_3_1_5 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [86] True
genericAutDistTest_RP_3_1_6 testType createProtocol = abstractTest testType createProtocol "[0];2;0" [124] True
genericAutDistTest_RP_3_1 = [genericAutDistTest_RP_3_1_1, genericAutDistTest_RP_3_1_2, genericAutDistTest_RP_3_1_3, genericAutDistTest_RP_3_1_4, genericAutDistTest_RP_3_1_5, genericAutDistTest_RP_3_1_6]

-- 95/122 helper
genericAutDistTest_RP_3_2_1 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [96, 0, 101] True
genericAutDistTest_RP_3_2_2 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [96, 0, 96] False
genericAutDistTest_RP_3_2_3 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [97, 1, 97] False
genericAutDistTest_RP_3_2_4 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [123, 0, 131] False
genericAutDistTest_RP_3_2_5 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [126, 1, 125] True
genericAutDistTest_RP_3_2_6 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;0" [120, 6, 118] True
genericAutDistTest_RP_3_2 = [genericAutDistTest_RP_3_2_1, genericAutDistTest_RP_3_2_2, genericAutDistTest_RP_3_2_3, genericAutDistTest_RP_3_2_4, genericAutDistTest_RP_3_2_5, genericAutDistTest_RP_3_2_6]

-- 46/73 helper
genericAutDistTest_RP_3_3_1 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [0, 92, 0] True
genericAutDistTest_RP_3_3_2 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [1, 46, 46] False
genericAutDistTest_RP_3_3_3 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [1, 93, 1] False
genericAutDistTest_RP_3_3_4 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [1, 179, 9] False
genericAutDistTest_RP_3_3_5 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [4, 146, 2] True
genericAutDistTest_RP_3_3_6 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;0" [5, 157, 12] False
genericAutDistTest_RP_3_3 = [genericAutDistTest_RP_3_3_1, genericAutDistTest_RP_3_3_2, genericAutDistTest_RP_3_3_3, genericAutDistTest_RP_3_3_4, genericAutDistTest_RP_3_3_5, genericAutDistTest_RP_3_3_6]

-- 181/208 helper
genericAutDistTest_RP_3_4_1 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [362, 0, 0] True
genericAutDistTest_RP_3_4_2 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [363, 0, 0] True
genericAutDistTest_RP_3_4_3 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [363, 1, 1] False
genericAutDistTest_RP_3_4_4 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [417, 33, 9] False
genericAutDistTest_RP_3_4_5 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [416, 0, 69] True
genericAutDistTest_RP_3_4_6 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;0" [421, 11, 13] False
genericAutDistTest_RP_3_4 = [genericAutDistTest_RP_3_4_1, genericAutDistTest_RP_3_4_2, genericAutDistTest_RP_3_4_3, genericAutDistTest_RP_3_4_4, genericAutDistTest_RP_3_4_5, genericAutDistTest_RP_3_4_6]

genericAutDistTest_RP_3 = genericAutDistTest_RP_3_1 ++ genericAutDistTest_RP_3_2 ++ genericAutDistTest_RP_3_3 ++ genericAutDistTest_RP_3_4


-- 29/40 helper
genericAutDistTest_RP_4_1_1 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [58] False
genericAutDistTest_RP_4_1_2 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [60] False
genericAutDistTest_RP_4_1_3 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [61] False
genericAutDistTest_RP_4_1_4 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [83] False
genericAutDistTest_RP_4_1_5 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [84] False
genericAutDistTest_RP_4_1_6 testType createProtocol = abstractTest testType createProtocol "[0];2;1" [121] False
genericAutDistTest_RP_4_1 = [genericAutDistTest_RP_4_1_1, genericAutDistTest_RP_4_1_2, genericAutDistTest_RP_4_1_3, genericAutDistTest_RP_4_1_4, genericAutDistTest_RP_4_1_5, genericAutDistTest_RP_4_1_6]

-- 94/121 helper
genericAutDistTest_RP_4_2_1 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [64, 64, 64] True
genericAutDistTest_RP_4_2_2 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [95, 0, 94] False
genericAutDistTest_RP_4_2_3 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [97, 3, 161] False
genericAutDistTest_RP_4_2_4 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [106, 33, 107] False
genericAutDistTest_RP_4_2_5 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [125, 0, 126] True
genericAutDistTest_RP_4_2_6 testType createProtocol = abstractTest testType createProtocol "[8,0,1];11;4" [5, 242, 19] True
genericAutDistTest_RP_4_2 = [genericAutDistTest_RP_4_2_1, genericAutDistTest_RP_4_2_2, genericAutDistTest_RP_4_2_3, genericAutDistTest_RP_4_2_4, genericAutDistTest_RP_4_2_5, genericAutDistTest_RP_4_2_6]

-- 45/72 helper
genericAutDistTest_RP_4_3_1 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [0, 90, 0] False
genericAutDistTest_RP_4_3_2 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [1, 90, 0] True
genericAutDistTest_RP_4_3_3 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [91, 1, 1] True
genericAutDistTest_RP_4_3_4 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [73, 33, 79] True
genericAutDistTest_RP_4_3_5 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [144, 1, 1] False
genericAutDistTest_RP_4_3_6 testType createProtocol = abstractTest testType createProtocol "[9,0,0];2;1" [49, 48, 48] True
genericAutDistTest_RP_4_3 = [genericAutDistTest_RP_4_3_1, genericAutDistTest_RP_4_3_2, genericAutDistTest_RP_4_3_3, genericAutDistTest_RP_4_3_4, genericAutDistTest_RP_4_3_5, genericAutDistTest_RP_4_3_6]

-- 179/206 helper
-- genericAutDistTest_RP_4_4_1 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_4_4_2 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_4_4_3 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_4_4_4 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_4_4_5 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [x, x, x] b
-- genericAutDistTest_RP_4_4_6 testType createProtocol = abstractTest testType createProtocol "[0,269,42];69;49" [x, x,x] b
-- genericAutDistTest_RP_4_4 = [genericAutDistTest_RP_4_4_1, genericAutDistTest_RP_4_4_2, genericAutDistTest_RP_4_4_3, genericAutDistTest_RP_4_4_4, genericAutDistTest_RP_4_4_5, genericAutDistTest_RP_4_4_6]

genericAutDistTest_RP_4 = genericAutDistTest_RP_4_1 ++ genericAutDistTest_RP_4_2 ++ genericAutDistTest_RP_4_3 -- ++ genericAutDistTest_RP_4_4


-- Threshold tests

-- 197/208 helper
genericAutDistTest_TP_1_1_1 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [394] False
genericAutDistTest_TP_1_1_2 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [395] False
genericAutDistTest_TP_1_1_3 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [415] False
genericAutDistTest_TP_1_1_4 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [416] False
genericAutDistTest_TP_1_1_5 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [417] False
genericAutDistTest_TP_1_1_6 testType createProtocol = abstractTest testType createProtocol "[-1];-1" [815] False
genericAutDistTest_TP_1_1 = [genericAutDistTest_TP_1_1_1, genericAutDistTest_TP_1_1_2, genericAutDistTest_TP_1_1_3, genericAutDistTest_TP_1_1_4, genericAutDistTest_TP_1_1_5, genericAutDistTest_TP_1_1_6]

-- 307/334 helper
genericAutDistTest_TP_1_2_1 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [0, 0, 614] False
genericAutDistTest_TP_1_2_2 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [200, 220, 200] False
genericAutDistTest_TP_1_2_3 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [400, 0, 220] False
genericAutDistTest_TP_1_2_4 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [0, 0, 668] False
genericAutDistTest_TP_1_2_5 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [300, 300, 300] False
genericAutDistTest_TP_1_2_6 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];-4" [500, 0, 200] False
genericAutDistTest_TP_1_2 = [genericAutDistTest_TP_1_2_1, genericAutDistTest_TP_1_2_2, genericAutDistTest_TP_1_2_3, genericAutDistTest_TP_1_2_4, genericAutDistTest_TP_1_2_5, genericAutDistTest_TP_1_2_6]

-- 312/339 helper
genericAutDistTest_TP_1_3_1 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [0, 624, 0] False
genericAutDistTest_TP_1_3_2 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [624, 0, 1] False
genericAutDistTest_TP_1_3_3 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [624, 2, 3] False
genericAutDistTest_TP_1_3_4 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [678, 0, 0] False
genericAutDistTest_TP_1_3_5 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [300, 300, 300] False
genericAutDistTest_TP_1_3_6 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];-1" [680, 125, 2] False
genericAutDistTest_TP_1_3 = [genericAutDistTest_TP_1_3_1, genericAutDistTest_TP_1_3_2, genericAutDistTest_TP_1_3_3, genericAutDistTest_TP_1_3_4, genericAutDistTest_TP_1_3_5, genericAutDistTest_TP_1_3_6]

-- 675/702 helper
genericAutDistTest_TP_1_4_1 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [2, 675, 675] False
genericAutDistTest_TP_1_4_2 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [20, 675, 675] False
genericAutDistTest_TP_1_4_3 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [75, 640, 675] False
genericAutDistTest_TP_1_4_4 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [0, 702, 702] False
genericAutDistTest_TP_1_4_5 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [16, 783, 758] False
genericAutDistTest_TP_1_4_6 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];-161" [1200, 800, 393] False
genericAutDistTest_TP_1_4 = [genericAutDistTest_TP_1_4_1, genericAutDistTest_TP_1_4_2, genericAutDistTest_TP_1_4_3, genericAutDistTest_TP_1_4_4, genericAutDistTest_TP_1_4_5, genericAutDistTest_TP_1_4_6]

genericAutDistTest_TP_1 = genericAutDistTest_TP_1_1 ++ genericAutDistTest_TP_1_2 ++ genericAutDistTest_TP_1_3 ++ genericAutDistTest_TP_1_4


-- 177/188 helper
genericAutDistTest_TP_2_1_1 testType createProtocol = abstractTest testType createProtocol "[-1];0" [354] False
genericAutDistTest_TP_2_1_2 testType createProtocol = abstractTest testType createProtocol "[-1];0" [355] False
genericAutDistTest_TP_2_1_3 testType createProtocol = abstractTest testType createProtocol "[-1];0" [370] False
genericAutDistTest_TP_2_1_4 testType createProtocol = abstractTest testType createProtocol "[-1];0" [376] False
genericAutDistTest_TP_2_1_5 testType createProtocol = abstractTest testType createProtocol "[-1];0" [400] False
genericAutDistTest_TP_2_1_6 testType createProtocol = abstractTest testType createProtocol "[-1];0" [560] False
genericAutDistTest_TP_2_1 = [genericAutDistTest_TP_2_1_1, genericAutDistTest_TP_2_1_2, genericAutDistTest_TP_2_1_3, genericAutDistTest_TP_2_1_4, genericAutDistTest_TP_2_1_5, genericAutDistTest_TP_2_1_6]

-- 277/304 helper
genericAutDistTest_TP_2_2_1 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [277, 277, 2] False
genericAutDistTest_TP_2_2_2 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [250, 250, 34] False
genericAutDistTest_TP_2_2_3 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [322, 58, 20] False
genericAutDistTest_TP_2_2_4 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [304, 304, 0] False
genericAutDistTest_TP_2_2_5 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [304, 304, 66] False
genericAutDistTest_TP_2_2_6 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];0" [300, 300, 86] False
genericAutDistTest_TP_2_2 = [genericAutDistTest_TP_2_2_1, genericAutDistTest_TP_2_2_2, genericAutDistTest_TP_2_2_3, genericAutDistTest_TP_2_2_4, genericAutDistTest_TP_2_2_5, genericAutDistTest_TP_2_2_6]

-- 277/304 helper
genericAutDistTest_TP_2_3_1 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [277, 277, 2] False
genericAutDistTest_TP_2_3_2 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [250, 250, 34] False
genericAutDistTest_TP_2_3_3 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [322, 58, 20] False
genericAutDistTest_TP_2_3_4 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [304, 304, 0] False
genericAutDistTest_TP_2_3_5 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [304, 304, 66] False
genericAutDistTest_TP_2_3_6 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];0" [300, 300, 86] False
genericAutDistTest_TP_2_3 = [genericAutDistTest_TP_2_3_1, genericAutDistTest_TP_2_3_2, genericAutDistTest_TP_2_3_3, genericAutDistTest_TP_2_3_4, genericAutDistTest_TP_2_3_5, genericAutDistTest_TP_2_3_6]

-- 595/622 helper
genericAutDistTest_TP_2_4_1 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [1190, 0, 0] False
genericAutDistTest_TP_2_4_2 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [596, 596, 0] False
genericAutDistTest_TP_2_4_3 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [1190, 10, 10] False
genericAutDistTest_TP_2_4_4 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [1244, 0, 0] False
genericAutDistTest_TP_2_4_5 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [1244, 1, 0] False
genericAutDistTest_TP_2_4_6 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];0" [1244, 60, 150] False
genericAutDistTest_TP_2_4 = [genericAutDistTest_TP_2_4_1, genericAutDistTest_TP_2_4_2, genericAutDistTest_TP_2_4_3, genericAutDistTest_TP_2_4_4, genericAutDistTest_TP_2_4_5, genericAutDistTest_TP_2_4_6]

genericAutDistTest_TP_2 = genericAutDistTest_TP_2_1 ++ genericAutDistTest_TP_2_2 ++ genericAutDistTest_TP_2_3 ++ genericAutDistTest_TP_2_4


-- 177/188 helper
genericAutDistTest_TP_3_1_1 testType createProtocol = abstractTest testType createProtocol "[-1];1" [354] False
genericAutDistTest_TP_3_1_2 testType createProtocol = abstractTest testType createProtocol "[-1];1" [364] False
genericAutDistTest_TP_3_1_3 testType createProtocol = abstractTest testType createProtocol "[-1];1" [375] False
genericAutDistTest_TP_3_1_4 testType createProtocol = abstractTest testType createProtocol "[-1];1" [376] False
genericAutDistTest_TP_3_1_5 testType createProtocol = abstractTest testType createProtocol "[-1];1" [380] False
genericAutDistTest_TP_3_1_6 testType createProtocol = abstractTest testType createProtocol "[-1];1" [500] False
genericAutDistTest_TP_3_1 = [genericAutDistTest_TP_3_1_1, genericAutDistTest_TP_3_1_2, genericAutDistTest_TP_3_1_3, genericAutDistTest_TP_3_1_4, genericAutDistTest_TP_3_1_5, genericAutDistTest_TP_3_1_6]

 -- 307/334 helper
genericAutDistTest_TP_3_2_1 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [307, 307, 2] False
genericAutDistTest_TP_3_2_2 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [308, 308, 0] False
genericAutDistTest_TP_3_2_3 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [322, 58, 20] False
genericAutDistTest_TP_3_2_4 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [668, 0, 0] False
genericAutDistTest_TP_3_2_5 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [668, 1, 0] False
genericAutDistTest_TP_3_2_6 testType createProtocol = abstractTest testType createProtocol "[-8,-2,-1];4" [300, 368, 100] False
genericAutDistTest_TP_3_2 = [genericAutDistTest_TP_3_2_1, genericAutDistTest_TP_3_2_2, genericAutDistTest_TP_3_2_3, genericAutDistTest_TP_3_2_4, genericAutDistTest_TP_3_2_5, genericAutDistTest_TP_3_2_6]

-- 277/304 helper
genericAutDistTest_TP_3_3_1 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [277, 277, 2] False
genericAutDistTest_TP_3_3_2 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [250, 250, 34] False
genericAutDistTest_TP_3_3_3 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [322, 0, 0] False
genericAutDistTest_TP_3_3_4 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [304, 304, 0] False
genericAutDistTest_TP_3_3_5 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [304, 304, 66] False
genericAutDistTest_TP_3_3_6 testType createProtocol = abstractTest testType createProtocol "[-9,-4,-3];1" [300, 300, 86] False
genericAutDistTest_TP_3_3 = [genericAutDistTest_TP_3_3_1, genericAutDistTest_TP_3_3_2, genericAutDistTest_TP_3_3_3, genericAutDistTest_TP_3_3_4, genericAutDistTest_TP_3_3_5, genericAutDistTest_TP_3_3_6]

-- 655/682 helper
genericAutDistTest_TP_3_4_1 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [1310, 0, 0] False
genericAutDistTest_TP_3_4_2 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [1312, 0, 0] False
genericAutDistTest_TP_3_4_3 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [1312, 10, 4] False
genericAutDistTest_TP_3_4_4 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [1364, 1, 0] False
genericAutDistTest_TP_3_4_5 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [0, 682, 682] False
genericAutDistTest_TP_3_4_6 testType createProtocol = abstractTest testType createProtocol "[-5500,-269,-42];161" [100, 681, 682] False
genericAutDistTest_TP_3_4 = [genericAutDistTest_TP_3_4_1, genericAutDistTest_TP_3_4_2, genericAutDistTest_TP_3_4_3, genericAutDistTest_TP_3_4_4, genericAutDistTest_TP_3_4_5, genericAutDistTest_TP_3_4_6]

genericAutDistTest_TP_3 = genericAutDistTest_TP_3_1 ++ genericAutDistTest_TP_3_2 ++ genericAutDistTest_TP_3_3 ++ genericAutDistTest_TP_3_4


-- 197/208 helper
genericAutDistTest_TP_4_1_1 testType createProtocol = abstractTest testType createProtocol "[1];-1" [394] True
genericAutDistTest_TP_4_1_2 testType createProtocol = abstractTest testType createProtocol "[1];-1" [400] True
genericAutDistTest_TP_4_1_3 testType createProtocol = abstractTest testType createProtocol "[1];-1" [411] True
genericAutDistTest_TP_4_1_4 testType createProtocol = abstractTest testType createProtocol "[1];-1" [416] True
genericAutDistTest_TP_4_1_5 testType createProtocol = abstractTest testType createProtocol "[1];-1" [450] True
genericAutDistTest_TP_4_1_6 testType createProtocol = abstractTest testType createProtocol "[1];-1" [500] True
genericAutDistTest_TP_4_1 = [genericAutDistTest_TP_4_1_1, genericAutDistTest_TP_4_1_2, genericAutDistTest_TP_4_1_3, genericAutDistTest_TP_4_1_4, genericAutDistTest_TP_4_1_5, genericAutDistTest_TP_4_1_6]

-- 307/334 helper
genericAutDistTest_TP_4_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [614, 0, 0] True
genericAutDistTest_TP_4_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [1, 308, 308] True
genericAutDistTest_TP_4_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [322, 0, 307] True
genericAutDistTest_TP_4_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [668, 1, 0] True
genericAutDistTest_TP_4_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [334, 30, 334] True
genericAutDistTest_TP_4_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];-4" [668, 10, 52] True
genericAutDistTest_TP_4_2 = [genericAutDistTest_TP_4_2_1, genericAutDistTest_TP_4_2_2, genericAutDistTest_TP_4_2_3, genericAutDistTest_TP_4_2_4, genericAutDistTest_TP_4_2_5, genericAutDistTest_TP_4_2_6]

-- 312/339 helper
genericAutDistTest_TP_4_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [624, 0, 0] True
genericAutDistTest_TP_4_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [1, 312, 312] True
genericAutDistTest_TP_4_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [322, 14, 339] True
genericAutDistTest_TP_4_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [678, 1, 0] True
genericAutDistTest_TP_4_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [678, 30, 0] True
genericAutDistTest_TP_4_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];-1" [700, 1, 2] True
genericAutDistTest_TP_4_3 = [genericAutDistTest_TP_4_3_1, genericAutDistTest_TP_4_3_2, genericAutDistTest_TP_4_3_3, genericAutDistTest_TP_4_3_4, genericAutDistTest_TP_4_3_5, genericAutDistTest_TP_4_3_6]

-- 675/702 helper
genericAutDistTest_TP_4_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [0, 1350, 0] True
genericAutDistTest_TP_4_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [1, 48, 1312] True
genericAutDistTest_TP_4_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [322, 354, 685] True
genericAutDistTest_TP_4_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [1402, 1, 0] True
genericAutDistTest_TP_4_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [702, 30, 702] True
genericAutDistTest_TP_4_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];-161" [63, 51, 1402] True
genericAutDistTest_TP_4_4 = [genericAutDistTest_TP_4_4_1, genericAutDistTest_TP_4_4_2, genericAutDistTest_TP_4_4_3, genericAutDistTest_TP_4_4_4, genericAutDistTest_TP_4_4_5, genericAutDistTest_TP_4_4_6]

genericAutDistTest_TP_4 = genericAutDistTest_TP_4_1 ++ genericAutDistTest_TP_4_2 ++ genericAutDistTest_TP_4_3 ++ genericAutDistTest_TP_4_4


-- 177/188 helper
genericAutDistTest_TP_5_1_1 testType createProtocol = abstractTest testType createProtocol "[1];0" [354] True
genericAutDistTest_TP_5_1_2 testType createProtocol = abstractTest testType createProtocol "[1];0" [360] True
genericAutDistTest_TP_5_1_3 testType createProtocol = abstractTest testType createProtocol "[1];0" [366] True
genericAutDistTest_TP_5_1_4 testType createProtocol = abstractTest testType createProtocol "[1];0" [376] True
genericAutDistTest_TP_5_1_5 testType createProtocol = abstractTest testType createProtocol "[1];0" [423] True
genericAutDistTest_TP_5_1_6 testType createProtocol = abstractTest testType createProtocol "[1];0" [562] True
genericAutDistTest_TP_5_1 = [genericAutDistTest_TP_5_1_1, genericAutDistTest_TP_5_1_2, genericAutDistTest_TP_5_1_3, genericAutDistTest_TP_5_1_4, genericAutDistTest_TP_5_1_5, genericAutDistTest_TP_5_1_6]

-- 277/304 helper
genericAutDistTest_TP_5_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [277, 277, 2] True
genericAutDistTest_TP_5_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [277, 300, 4] True
genericAutDistTest_TP_5_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [300, 304, 0] True
genericAutDistTest_TP_5_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [304, 316, 50] True
genericAutDistTest_TP_5_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [600, 301, 0] True
genericAutDistTest_TP_5_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];0" [304, 304, 304] True
genericAutDistTest_TP_5_2 = [genericAutDistTest_TP_5_2_1, genericAutDistTest_TP_5_2_2, genericAutDistTest_TP_5_2_3, genericAutDistTest_TP_5_2_4, genericAutDistTest_TP_5_2_5, genericAutDistTest_TP_5_2_6]

-- 277/304 helper
genericAutDistTest_TP_5_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [554, 0, 0] True
genericAutDistTest_TP_5_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [277, 277, 10] True
genericAutDistTest_TP_5_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [322, 200, 77] True
genericAutDistTest_TP_5_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [304, 1, 307] True
genericAutDistTest_TP_5_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [0, 304, 600] True
genericAutDistTest_TP_5_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];0" [0, 1000, 200] True
genericAutDistTest_TP_5_3 = [genericAutDistTest_TP_5_3_1, genericAutDistTest_TP_5_3_2, genericAutDistTest_TP_5_3_3, genericAutDistTest_TP_5_3_4, genericAutDistTest_TP_5_3_5, genericAutDistTest_TP_5_3_6]

-- 595/622 helper
genericAutDistTest_TP_5_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [1190, 0, 0] True
genericAutDistTest_TP_5_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [595, 595, 20] True
genericAutDistTest_TP_5_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [600, 295, 300] True
genericAutDistTest_TP_5_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [1244, 0, 0] True
genericAutDistTest_TP_5_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [1244, 30, 0] True
genericAutDistTest_TP_5_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];0" [1312, 158, 92] True
genericAutDistTest_TP_5_4 = [genericAutDistTest_TP_5_4_1, genericAutDistTest_TP_5_4_2, genericAutDistTest_TP_5_4_3, genericAutDistTest_TP_5_4_4, genericAutDistTest_TP_5_4_5, genericAutDistTest_TP_5_4_6]

genericAutDistTest_TP_5 = genericAutDistTest_TP_5_1 ++ genericAutDistTest_TP_5_2 ++ genericAutDistTest_TP_5_3 ++ genericAutDistTest_TP_5_4


-- 177/188 helper
genericAutDistTest_TP_6_1_1 testType createProtocol = abstractTest testType createProtocol "[1];1" [354] True
genericAutDistTest_TP_6_1_2 testType createProtocol = abstractTest testType createProtocol "[1];1" [360] True
genericAutDistTest_TP_6_1_3 testType createProtocol = abstractTest testType createProtocol "[1];1" [370] True
genericAutDistTest_TP_6_1_4 testType createProtocol = abstractTest testType createProtocol "[1];1" [376] True
genericAutDistTest_TP_6_1_5 testType createProtocol = abstractTest testType createProtocol "[1];1" [480] True
genericAutDistTest_TP_6_1_6 testType createProtocol = abstractTest testType createProtocol "[1];1" [512] True
genericAutDistTest_TP_6_1 = [genericAutDistTest_TP_6_1_1, genericAutDistTest_TP_6_1_2, genericAutDistTest_TP_6_1_3, genericAutDistTest_TP_6_1_4, genericAutDistTest_TP_6_1_5, genericAutDistTest_TP_6_1_6]

-- 307/334 helper
genericAutDistTest_TP_6_2_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [614, 0, 0] True
genericAutDistTest_TP_6_2_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [307, 309, 0] True
genericAutDistTest_TP_6_2_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [322, 0, 300] True
genericAutDistTest_TP_6_2_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [334, 1, 334] True
genericAutDistTest_TP_6_2_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [0, 30, 668] True
genericAutDistTest_TP_6_2_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];4" [312, 131, 334] True
genericAutDistTest_TP_6_2 = [genericAutDistTest_TP_6_2_1, genericAutDistTest_TP_6_2_2, genericAutDistTest_TP_6_2_3, genericAutDistTest_TP_6_2_4, genericAutDistTest_TP_6_2_5, genericAutDistTest_TP_6_2_6]

-- 277/304 helper
genericAutDistTest_TP_6_3_1 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [554, 0, 0] True
genericAutDistTest_TP_6_3_2 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [277, 277, 10] True
genericAutDistTest_TP_6_3_3 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [322, 200, 77] True
genericAutDistTest_TP_6_3_4 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [304, 1, 307] True
genericAutDistTest_TP_6_3_5 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [0, 304, 600] True
genericAutDistTest_TP_6_3_6 testType createProtocol = abstractTest testType createProtocol "[9,4,3];1" [0, 1000, 200] True
genericAutDistTest_TP_6_3 = [genericAutDistTest_TP_6_3_1, genericAutDistTest_TP_6_3_2, genericAutDistTest_TP_6_3_3, genericAutDistTest_TP_6_3_4, genericAutDistTest_TP_6_3_5, genericAutDistTest_TP_6_3_6]

-- 655/682 helper
genericAutDistTest_TP_6_4_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [1310, 0, 0] True
genericAutDistTest_TP_6_4_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [1312, 0, 0] True
genericAutDistTest_TP_6_4_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [1312, 10, 4] True
genericAutDistTest_TP_6_4_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [1364, 1, 0] True
genericAutDistTest_TP_6_4_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [0, 682, 682] True
genericAutDistTest_TP_6_4_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,42];161" [100, 681, 682] True
genericAutDistTest_TP_6_4 = [genericAutDistTest_TP_6_4_1, genericAutDistTest_TP_6_4_2, genericAutDistTest_TP_6_4_3, genericAutDistTest_TP_6_4_4, genericAutDistTest_TP_6_4_5, genericAutDistTest_TP_6_4_6]

genericAutDistTest_TP_6 = genericAutDistTest_TP_6_1 ++ genericAutDistTest_TP_6_2 ++ genericAutDistTest_TP_6_3 ++ genericAutDistTest_TP_6_4


-- error
genericAutDistTest_TP_7_1_1 testType createProtocol = abstractTest testType createProtocol "[0];-1" [354] True
genericAutDistTest_TP_7_1_2 testType createProtocol = abstractTest testType createProtocol "[0];-1" [360] True
genericAutDistTest_TP_7_1_3 testType createProtocol = abstractTest testType createProtocol "[0];-1" [370] True
genericAutDistTest_TP_7_1_4 testType createProtocol = abstractTest testType createProtocol "[0];-1" [376] True
genericAutDistTest_TP_7_1_5 testType createProtocol = abstractTest testType createProtocol "[0];-1" [450] True
genericAutDistTest_TP_7_1_6 testType createProtocol = abstractTest testType createProtocol "[0];-1" [512] True
genericAutDistTest_TP_7_1 = [genericAutDistTest_TP_7_1_1, genericAutDistTest_TP_7_1_2, genericAutDistTest_TP_7_1_3, genericAutDistTest_TP_7_1_4, genericAutDistTest_TP_7_1_5, genericAutDistTest_TP_7_1_6]

-- 238/257 helper
genericAutDistTest_TP_7_2_1 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [0, 476] True
genericAutDistTest_TP_7_2_2 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [476, 0] False
genericAutDistTest_TP_7_2_3 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [2, 480] True
genericAutDistTest_TP_7_2_4 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [514, 0] False
genericAutDistTest_TP_7_2_5 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [200, 514] True
genericAutDistTest_TP_7_2_6 testType createProtocol = abstractTest testType createProtocol "[-2,1];-5" [514, 232] False
genericAutDistTest_TP_7_2 = [genericAutDistTest_TP_7_2_1, genericAutDistTest_TP_7_2_2, genericAutDistTest_TP_7_2_3, genericAutDistTest_TP_7_2_4, genericAutDistTest_TP_7_2_5, genericAutDistTest_TP_7_2_6]

-- 307/334 helper
genericAutDistTest_TP_7_3_1 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [614, 0, 0] True
genericAutDistTest_TP_7_3_2 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [308, 0, 308] True
genericAutDistTest_TP_7_3_4 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [3, 10, 610] False
genericAutDistTest_TP_7_3_3 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [668, 2, 0] True
genericAutDistTest_TP_7_3_5 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [20, 30, 660] False
genericAutDistTest_TP_7_3_6 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];-4" [100, 1, 700] True
genericAutDistTest_TP_7_3 = [genericAutDistTest_TP_7_3_1, genericAutDistTest_TP_7_3_2, genericAutDistTest_TP_7_3_3, genericAutDistTest_TP_7_3_4, genericAutDistTest_TP_7_3_5, genericAutDistTest_TP_7_3_6]

-- 297/324 helper
genericAutDistTest_TP_7_4_1 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [600, 0, 0] True
genericAutDistTest_TP_7_4_2 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [297, 2, 297] True
genericAutDistTest_TP_7_4_3 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [0, 322, 297] False
genericAutDistTest_TP_7_4_4 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [648, 1, 0] True
genericAutDistTest_TP_7_4_5 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [0, 648, 0] False
genericAutDistTest_TP_7_4_6 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];-16" [100, 648, 1] False
genericAutDistTest_TP_7_4 = [genericAutDistTest_TP_7_4_1, genericAutDistTest_TP_7_4_2, genericAutDistTest_TP_7_4_3, genericAutDistTest_TP_7_4_4, genericAutDistTest_TP_7_4_5, genericAutDistTest_TP_7_4_6]

-- 675/702 helper
genericAutDistTest_TP_7_5_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [676, 676, 0] True
genericAutDistTest_TP_7_5_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [1350, 5, 0] True
genericAutDistTest_TP_7_5_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [676, 0, 676] True
genericAutDistTest_TP_7_5_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [0, 0, 1410] False
genericAutDistTest_TP_7_5_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [0, 50, 2000] False
genericAutDistTest_TP_7_5_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];-161" [1500, 1, 2] True
genericAutDistTest_TP_7_5 = [genericAutDistTest_TP_7_5_1, genericAutDistTest_TP_7_5_2, genericAutDistTest_TP_7_5_3, genericAutDistTest_TP_7_5_4, genericAutDistTest_TP_7_5_5, genericAutDistTest_TP_7_5_6]

genericAutDistTest_TP_7 = {- genericAutDistTest_TP_7_1 ++ -} genericAutDistTest_TP_7_2 ++ genericAutDistTest_TP_7_3 ++ genericAutDistTest_TP_7_4 ++ genericAutDistTest_TP_7_5


-- error
genericAutDistTest_TP_8_1_1 testType createProtocol = abstractTest testType createProtocol "[0];0" [354] True
genericAutDistTest_TP_8_1_2 testType createProtocol = abstractTest testType createProtocol "[0];0" [360] True
genericAutDistTest_TP_8_1_3 testType createProtocol = abstractTest testType createProtocol "[0];0" [370] True
genericAutDistTest_TP_8_1_4 testType createProtocol = abstractTest testType createProtocol "[0];0" [376] True
genericAutDistTest_TP_8_1_5 testType createProtocol = abstractTest testType createProtocol "[0];0" [400] True
genericAutDistTest_TP_8_1_6 testType createProtocol = abstractTest testType createProtocol "[0];0" [512] True
genericAutDistTest_TP_8_1 = [genericAutDistTest_TP_8_1_1, genericAutDistTest_TP_8_1_2, genericAutDistTest_TP_8_1_3, genericAutDistTest_TP_8_1_4, genericAutDistTest_TP_8_1_5, genericAutDistTest_TP_8_1_6]

-- 213/232 helper
genericAutDistTest_TP_8_2_1 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [0, 426] True
genericAutDistTest_TP_8_2_2 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [216, 213] False
genericAutDistTest_TP_8_2_3 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [20, 440] True
genericAutDistTest_TP_8_2_4 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [464, 3] False
genericAutDistTest_TP_8_2_5 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [400, 800] True
genericAutDistTest_TP_8_2_6 testType createProtocol = abstractTest testType createProtocol "[-2,1];0" [400, 300] False
genericAutDistTest_TP_8_2 = [genericAutDistTest_TP_8_2_1, genericAutDistTest_TP_8_2_2, genericAutDistTest_TP_8_2_3, genericAutDistTest_TP_8_2_4, genericAutDistTest_TP_8_2_5, genericAutDistTest_TP_8_2_6]

-- 277/304 helper
genericAutDistTest_TP_8_3_1 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [554, 0, 0] True
genericAutDistTest_TP_8_3_2 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [277, 277, 10] True
genericAutDistTest_TP_8_3_4 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [77, 200, 322] False
genericAutDistTest_TP_8_3_3 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [304, 1, 307] True
genericAutDistTest_TP_8_3_5 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [0, 304, 600] False
genericAutDistTest_TP_8_3_6 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];0" [200, 1000, 200] False
genericAutDistTest_TP_8_3 = [genericAutDistTest_TP_8_3_1, genericAutDistTest_TP_8_3_2, genericAutDistTest_TP_8_3_3, genericAutDistTest_TP_8_3_4, genericAutDistTest_TP_8_3_5, genericAutDistTest_TP_8_3_6]

-- 277/304 helper
genericAutDistTest_TP_8_4_1 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [554, 0, 0] True
genericAutDistTest_TP_8_4_2 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [277, 277, 10] True
genericAutDistTest_TP_8_4_3 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [77, 328, 200] False
genericAutDistTest_TP_8_4_4 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [0, 610, 0] False
genericAutDistTest_TP_8_4_5 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [304, 1, 307] True
genericAutDistTest_TP_8_4_6 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];0" [0, 1000, 200] False
genericAutDistTest_TP_8_4 = [genericAutDistTest_TP_8_4_1, genericAutDistTest_TP_8_4_2, genericAutDistTest_TP_8_4_3, genericAutDistTest_TP_8_4_4, genericAutDistTest_TP_8_4_5, genericAutDistTest_TP_8_4_6]

-- 595,622 helper
genericAutDistTest_TP_8_5_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [596, 596, 0] True
genericAutDistTest_TP_8_5_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [600, 0, 600] True
genericAutDistTest_TP_8_5_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [1200, 0, 0] True
genericAutDistTest_TP_8_5_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [622, 624, 0] True
genericAutDistTest_TP_8_5_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [0, 0, 1312] False
genericAutDistTest_TP_8_5_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];0" [2, 10, 2450] False
genericAutDistTest_TP_8_5 = [genericAutDistTest_TP_8_5_1, genericAutDistTest_TP_8_5_2, genericAutDistTest_TP_8_5_3, genericAutDistTest_TP_8_5_4, genericAutDistTest_TP_8_5_5, genericAutDistTest_TP_8_5_6]

genericAutDistTest_TP_8 = {- genericAutDistTest_TP_8_1 ++ -} genericAutDistTest_TP_8_2 ++ genericAutDistTest_TP_8_3 ++ genericAutDistTest_TP_8_4 ++ genericAutDistTest_TP_8_5


-- error
genericAutDistTest_TP_9_1_1 testType createProtocol = abstractTest testType createProtocol "[0];1" [354] False
genericAutDistTest_TP_9_1_2 testType createProtocol = abstractTest testType createProtocol "[0];1" [360] False
genericAutDistTest_TP_9_1_3 testType createProtocol = abstractTest testType createProtocol "[0];1" [370] False
genericAutDistTest_TP_9_1_4 testType createProtocol = abstractTest testType createProtocol "[0];1" [376] False
genericAutDistTest_TP_9_1_5 testType createProtocol = abstractTest testType createProtocol "[0];1" [400] False
genericAutDistTest_TP_9_1_6 testType createProtocol = abstractTest testType createProtocol "[0];1" [512] False
genericAutDistTest_TP_9_1 = [genericAutDistTest_TP_9_1_1, genericAutDistTest_TP_9_1_2, genericAutDistTest_TP_9_1_3, genericAutDistTest_TP_9_1_4, genericAutDistTest_TP_9_1_5, genericAutDistTest_TP_9_1_6]

-- 299/318 helper
genericAutDistTest_TP_9_2_1 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [598, 0] False
genericAutDistTest_TP_9_2_2 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [5, 600] True
genericAutDistTest_TP_9_2_3 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [300, 300] False
genericAutDistTest_TP_9_2_4 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [310, 320] False
genericAutDistTest_TP_9_2_5 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [40, 600] True
genericAutDistTest_TP_9_2_6 testType createProtocol = abstractTest testType createProtocol "[-2,1];5" [31, 320] True
genericAutDistTest_TP_9_2 = [genericAutDistTest_TP_9_2_1, genericAutDistTest_TP_9_2_2, genericAutDistTest_TP_9_2_3, genericAutDistTest_TP_9_2_4, genericAutDistTest_TP_9_2_5 , genericAutDistTest_TP_9_2_6]

-- 307/334 helper
genericAutDistTest_TP_9_3_1 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [0, 308, 308] False
genericAutDistTest_TP_9_3_2 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [640, 0, 0] True
genericAutDistTest_TP_9_3_4 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [100, 400, 308] False
genericAutDistTest_TP_9_3_3 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [0, 668, 0] False
genericAutDistTest_TP_9_3_5 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [200, 800, 400] True
genericAutDistTest_TP_9_3_6 testType createProtocol = abstractTest testType createProtocol "[8,-2,-1];4" [200, 700, 50] True
genericAutDistTest_TP_9_3 = [genericAutDistTest_TP_9_3_1, genericAutDistTest_TP_9_3_2, genericAutDistTest_TP_9_3_3, genericAutDistTest_TP_9_3_4, genericAutDistTest_TP_9_3_5, genericAutDistTest_TP_9_3_6]

-- 277/304 helper
genericAutDistTest_TP_9_4_1 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [0, 554, 0] False
genericAutDistTest_TP_9_4_2 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [322, 200, 77] True
genericAutDistTest_TP_9_4_3 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [0, 282, 282] False
genericAutDistTest_TP_9_4_4 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [304, 1, 307] True
genericAutDistTest_TP_9_4_5 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [0, 600, 304] False
genericAutDistTest_TP_9_4_6 testType createProtocol = abstractTest testType createProtocol "[9,-4,3];1" [1000, 200, 1] True
genericAutDistTest_TP_9_4 = [genericAutDistTest_TP_9_4_1, genericAutDistTest_TP_9_4_2, genericAutDistTest_TP_9_4_3, genericAutDistTest_TP_9_4_4, genericAutDistTest_TP_9_4_5, genericAutDistTest_TP_9_4_6]

-- 655/682 helper
genericAutDistTest_TP_9_5_1 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [0, 0, 1310] False
genericAutDistTest_TP_9_5_2 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [1312, 50, 0] True
genericAutDistTest_TP_9_5_3 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [656, 20, 656] True
genericAutDistTest_TP_9_5_4 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [0, 0, 1364] False
genericAutDistTest_TP_9_5_5 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [3, 2, 2441] False
genericAutDistTest_TP_9_5_6 testType createProtocol = abstractTest testType createProtocol "[5500,269,-42];161" [700, 700, 700] True
genericAutDistTest_TP_9_5 = [genericAutDistTest_TP_9_5_1, genericAutDistTest_TP_9_5_2, genericAutDistTest_TP_9_5_3, genericAutDistTest_TP_9_5_4, genericAutDistTest_TP_9_5_5, genericAutDistTest_TP_9_5_6]

genericAutDistTest_TP_9 = {- genericAutDistTest_TP_9_1 ++ -} genericAutDistTest_TP_9_2 ++ genericAutDistTest_TP_9_3 ++ genericAutDistTest_TP_9_4 ++ genericAutDistTest_TP_9_5



--------------------------------------------------------------
--           Tests for Boolean Combinations
--------------------------------------------------------------


-- 95/122 helper
genericOpAutDistTest_NOT_1_1 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];11;4" [95, 95, 4] True
genericOpAutDistTest_NOT_1_2 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];11;4" [95, 95, 11] False
genericOpAutDistTest_NOT_1_3 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];11;4" [100, 95, 0] True
genericOpAutDistTest_NOT_1_4 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];11;4" [0, 122, 122] True
genericOpAutDistTest_NOT_1_5 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];11;4" [1, 122, 126] False
genericOpAutDistTest_NOT_1_6 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];11;4" [151, 161, 171] True
genericOpAutDistTest_NOT_1 = [genericOpAutDistTest_NOT_1_1, genericOpAutDistTest_NOT_1_2, genericOpAutDistTest_NOT_1_3, genericOpAutDistTest_NOT_1_4, genericOpAutDistTest_NOT_1_5, genericOpAutDistTest_NOT_1_6]

-- 308/335 helper
genericOpAutDistTest_NOT_2_1 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];4" [308, 2, 308] False
genericOpAutDistTest_NOT_2_2 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];4" [600, 20, 5] False
genericOpAutDistTest_NOT_2_3 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];4" [616, 20, 20] False
genericOpAutDistTest_NOT_2_4 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];4" [670, 0, 0] False
genericOpAutDistTest_NOT_2_5 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];4" [670, 1, 1] False
genericOpAutDistTest_NOT_2_6 testType createProtocol = abstractTest testType createProtocol "NOT [8,2,1];4" [700, 20, 30] False
genericOpAutDistTest_NOT_2 = [genericOpAutDistTest_NOT_2_1, genericOpAutDistTest_NOT_2_2, genericOpAutDistTest_NOT_2_3, genericOpAutDistTest_NOT_2_4, genericOpAutDistTest_NOT_2_5, genericOpAutDistTest_NOT_2_6]


-- 111/138 helper
genericOpAutDistTest_AND_1_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [9,0,0];2;0" [222, 0, 0] False
genericOpAutDistTest_AND_1_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [9,0,0];2;0" [111, 111, 20] False
genericOpAutDistTest_AND_1_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [9,0,0];2;0" [240, 5, 10] True
genericOpAutDistTest_AND_1_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [9,0,0];2;0" [276, 0, 7] True
genericOpAutDistTest_AND_1_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [9,0,0];2;0" [300, 50, 5] False
genericOpAutDistTest_AND_1_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [9,0,0];2;0" [336, 234, 5] True
genericOpAutDistTest_AND_1 = [genericOpAutDistTest_AND_1_1, genericOpAutDistTest_AND_1_2, genericOpAutDistTest_AND_1_3, genericOpAutDistTest_AND_1_4, genericOpAutDistTest_AND_1_5, genericOpAutDistTest_AND_1_6]

-- 372/391 helper
genericOpAutDistTest_AND_2_1 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 AND [-2,1];5" [744, 0] False
genericOpAutDistTest_AND_2_2 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 AND [-2,1];5" [3, 744] True
genericOpAutDistTest_AND_2_3 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 AND [-2,1];5" [750, 20] False
genericOpAutDistTest_AND_2_4 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 AND [-2,1];5" [782, 0] False
genericOpAutDistTest_AND_2_5 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 AND [-2,1];5" [10, 790] True
genericOpAutDistTest_AND_2_6 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 AND [-2,1];5" [800, 161] False
genericOpAutDistTest_AND_2 = [genericOpAutDistTest_AND_2_1, genericOpAutDistTest_AND_2_2, genericOpAutDistTest_AND_2_3,genericOpAutDistTest_AND_2_4, genericOpAutDistTest_AND_2_5, genericOpAutDistTest_AND_2_6]

-- 405/432 helper
genericOpAutDistTest_AND_3_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [8,2,1];4" [810, 0, 0] False
genericOpAutDistTest_AND_3_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [8,2,1];4" [810, 0, 3] True
genericOpAutDistTest_AND_3_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [8,2,1];4" [810, 6, 3] False
genericOpAutDistTest_AND_3_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [8,2,1];4" [864, 0, 0] False
genericOpAutDistTest_AND_3_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [8,2,1];4" [864, 3, 5] True
genericOpAutDistTest_AND_3_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND [8,2,1];4" [460, 460, 460] False
genericOpAutDistTest_AND_3 = [genericOpAutDistTest_AND_3_1, genericOpAutDistTest_AND_3_2, genericOpAutDistTest_AND_3_3, genericOpAutDistTest_AND_3_4, genericOpAutDistTest_AND_3_5, genericOpAutDistTest_AND_3_6]


-- 111/138 helper
genericOpAutDistTest_OR_1_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [9,0,0];2;0" [0, 0, 222] True
genericOpAutDistTest_OR_1_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [9,0,0];2;0" [20, 111, 111] True
genericOpAutDistTest_OR_1_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [9,0,0];2;0" [21, 111, 111] False
genericOpAutDistTest_OR_1_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [9,0,0];2;0" [275, 2, 0] True
genericOpAutDistTest_OR_1_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [9,0,0];2;0" [275, 2, 1] False
genericOpAutDistTest_OR_1_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [9,0,0];2;0" [275, 2, 36] False
genericOpAutDistTest_OR_1 = [genericOpAutDistTest_OR_1_1, genericOpAutDistTest_OR_1_2, genericOpAutDistTest_OR_1_3, genericOpAutDistTest_OR_1_4, genericOpAutDistTest_OR_1_5, genericOpAutDistTest_OR_1_6]

-- 372/391 helper
genericOpAutDistTest_OR_2_1 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 OR [-2,1];5" [0, 745] True
genericOpAutDistTest_OR_2_2 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 OR [-2,1];5" [380, 380] False
genericOpAutDistTest_OR_2_3 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 OR [-2,1];5" [390, 390] False
genericOpAutDistTest_OR_2_4 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 OR [-2,1];5" [400, 400] False
genericOpAutDistTest_OR_2_5 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 OR [-2,1];5" [800, 0] False
genericOpAutDistTest_OR_2_6 testType createProtocol = abstractTest testType createProtocol "[8,5];11;4 OR [-2,1];5" [23, 861] True
genericOpAutDistTest_OR_2 = [genericOpAutDistTest_OR_2_1, genericOpAutDistTest_OR_2_2, genericOpAutDistTest_OR_2_3, genericOpAutDistTest_OR_2_4, genericOpAutDistTest_OR_2_5, genericOpAutDistTest_OR_2_6]

-- 405/432 helper
genericOpAutDistTest_OR_3_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [8,2,1];4" [810, 5, 6] True
genericOpAutDistTest_OR_3_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [8,2,1];4" [410, 400, 10] True
genericOpAutDistTest_OR_3_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [8,2,1];4" [40, 200, 600] True
genericOpAutDistTest_OR_3_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [8,2,1];4" [0, 0, 864] True
genericOpAutDistTest_OR_3_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [8,2,1];4" [400, 100, 400] True
genericOpAutDistTest_OR_3_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 OR [8,2,1];4" [900, 200, 500] True
genericOpAutDistTest_OR_3 = [genericOpAutDistTest_OR_3_1, genericOpAutDistTest_OR_3_2, genericOpAutDistTest_OR_3_3, genericOpAutDistTest_OR_3_4, genericOpAutDistTest_OR_3_5, genericOpAutDistTest_OR_3_6]


-- 467/494 helper
genericOpAutDistTest_MIXED_1_1 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND NOT ([9,0,0];2;0 OR [8,-2,-1];4)" [467, 467, 2] False
genericOpAutDistTest_MIXED_1_2 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND NOT ([9,0,0];2;0 OR [8,-2,-1];4)" [9, 467, 472] True
genericOpAutDistTest_MIXED_1_3 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND NOT ([9,0,0];2;0 OR [8,-2,-1];4)" [470, 470, 5] False
genericOpAutDistTest_MIXED_1_4 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND NOT ([9,0,0];2;0 OR [8,-2,-1];4)" [494, 494, 0] False
genericOpAutDistTest_MIXED_1_5 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND NOT ([9,0,0];2;0 OR [8,-2,-1];4)" [494, 494, 3] False
genericOpAutDistTest_MIXED_1_6 testType createProtocol = abstractTest testType createProtocol "[8,2,1];11;4 AND NOT ([9,0,0];2;0 OR [8,-2,-1];4)" [11, 500, 500] True
genericOpAutDistTest_MIXED_1 = [genericOpAutDistTest_MIXED_1_1, genericOpAutDistTest_MIXED_1_2, genericOpAutDistTest_MIXED_1_3, genericOpAutDistTest_MIXED_1_4, genericOpAutDistTest_MIXED_1_5, genericOpAutDistTest_MIXED_1_6]



----------------------------------------------
--              Export Functions
----------------------------------------------


genericBinFocTest_RP = genericBinFocTest_RP_1 ++ genericBinFocTest_RP_2 ++ genericBinFocTest_RP_3 ++ genericBinFocTest_RP_4
genericBinFocTest_TP = genericBinFocTest_TP_1 ++ genericBinFocTest_TP_2 ++ genericBinFocTest_TP_3 ++ genericBinFocTest_TP_4 ++ genericBinFocTest_TP_5 ++ genericBinFocTest_TP_6 ++ genericBinFocTest_TP_7 ++ genericBinFocTest_TP_8 ++ genericBinFocTest_TP_9
genericBinFocTests = genericBinFocTest_RP ++ genericBinFocTest_TP


genericAutDistTest_RP = genericAutDistTest_RP_1 ++ genericAutDistTest_RP_2 ++ genericAutDistTest_RP_3 ++ genericAutDistTest_RP_4
genericAutDistTest_TP = genericAutDistTest_TP_1 ++ genericAutDistTest_TP_2 ++ genericAutDistTest_TP_3 ++ genericAutDistTest_TP_4 ++ genericAutDistTest_TP_5 ++ genericAutDistTest_TP_6 ++ genericAutDistTest_TP_7 ++ genericAutDistTest_TP_8 ++ genericAutDistTest_TP_9
genericAutDistTests = genericAutDistTest_RP ++ genericAutDistTest_TP


genericOpAutDistTest_NOT = genericOpAutDistTest_NOT_1 ++ genericOpAutDistTest_NOT_2
genericOpAutDistTest_AND = genericOpAutDistTest_AND_1 ++ genericOpAutDistTest_AND_2 ++ genericOpAutDistTest_AND_3
genericOpAutDistTest_OR = genericOpAutDistTest_OR_1 ++ genericOpAutDistTest_OR_2 ++ genericOpAutDistTest_OR_3
genericOpAutDistTest_MIXED = genericOpAutDistTest_MIXED_1
genericOpAutDistTest = genericOpAutDistTest_NOT ++ genericOpAutDistTest_AND ++ genericOpAutDistTest_OR ++ genericOpAutDistTest_MIXED
