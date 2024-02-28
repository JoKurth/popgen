module Helper.BooleanCircuit (
    buildThresholdCircuit,
    buildRemainderCircuit,
    buildUnionCircuit
)
where

import qualified Data.HashMap.Lazy as HashMap

import qualified Types.PopulationComputer as PC (Gate(..), Edge(..), BooleanCircuit(..))
import qualified Types.Predicates as Predicates (Predicate(..), BoolOp(..), NotOp(..), RemainderPredicate(..), ThresholdPredicate(..), BasePredicate)


-- Helper

data InternalGate = AND | OR | NOT   -- we model the NOT-gate as the result of the function not(x, y) = ¬x
    deriving (Show)
data InternalConstGate = T | F | Any -- we use this as the second input of NOT-gates and map it later to `Input 0` because the input here does not matter for the NOT function
    deriving (Show)
data InternalBooleanCircuit = Leaf Int | Node InternalBooleanCircuit (InternalGate, Int, String) InternalBooleanCircuit | Const InternalConstGate
    deriving (Show)
                                                                    -- the gate;     d;  the function that is calculated, e.g. >= 3 => additional information for the deduplication later 


getDescFromInternalCircuit :: InternalBooleanCircuit -> String
getDescFromInternalCircuit (Leaf s) = show s
getDescFromInternalCircuit (Const b) = show b
getDescFromInternalCircuit (Node _ (_, _, desc) _) = desc


intToBinary :: Int -> [Int]
intToBinary 0 = [0]
intToBinary n = intToBinary (n `div` 2) ++ [n `mod` 2]


-- Building

notGate :: Int -> Int -> InternalBooleanCircuit
notGate d val = Node (Leaf val) (NOT, d, "not " ++ show val) (Const Any)

buildInternalThresholdCircuit :: Int -> Int -> InternalBooleanCircuit
buildInternalThresholdCircuit c d
                                | c >= 2^(d+1) = Const F
                                | c <= -2^(d+1) + 1 = Const T
                                | d < 0 = error "Error during creation of threshold circuit. Reached a negative exponent."
                                | otherwise = Node
                                                (Node (Node (Node (Leaf (2^d)) (AND, d, show (2^d) ++ " && not " ++ show (-2^d)) (notGate d (-2^d)))
                                                        (AND, d, "(" ++ show (2^d) ++ " && not " ++ show (-2^d) ++ ") && >= " ++ show (c - 2^d))
                                                        (buildInternalThresholdCircuit (c - 2^d) (d - 1)))
                                                    (OR, d, "inner OR " ++ show c)
                                                    (Node (Node (notGate d (2^d)) (AND, d, "not " ++ show (2^d) ++ " && " ++ show (-2^d)) (Leaf (-2^d)))
                                                        (AND, d, "(not " ++ show (2^d) ++ " && " ++ show (-2^d) ++ ") && >= " ++ show (c + 2^d))
                                                        (buildInternalThresholdCircuit (c + 2^d) (d - 1))))
                                                (OR, d, "outer OR " ++ show c)
                                                (Node (Node (notGate d (2^d)) (AND, d, "not " ++ show (2^d) ++ " && not " ++ show (-2^d)) (notGate d (-2^d)))
                                                    (AND, d, "not " ++ show (2^d) ++ " && not " ++ show (-2^d) ++ " && >= " ++ show c)
                                                    (buildInternalThresholdCircuit c (d - 1)))

-- | 2^d == c
buildEqualityCicuit :: Int -> Int -> InternalBooleanCircuit
buildEqualityCicuit 1 c = if even c
                                then notGate 0 1
                                else Leaf 1
buildEqualityCicuit d c = Node leftLeaf (AND, d, leftDesc ++ " && (" ++ getDescFromInternalCircuit rightCirc ++ ")") rightCirc
    where
        binRep = intToBinary c
        binaryRepresentation = reverse $ if length binRep >= d
                                            then binRep
                                            else [0 | i <- [1 .. d - length binRep]] ++ binRep
        i = d - 1
        p_i = binaryRepresentation !! i
        leftLeaf = if p_i == 1
                        then Leaf (2^i)
                        else notGate d (2^i)
        leftDesc = if p_i == 1
                        then show (2^i)
                        else "not " ++ show (2^i)
        rightCirc = buildEqualityCicuit (d-1) c

buildInternalRemainderCircuit :: Int -> Int -> Int -> InternalBooleanCircuit
buildInternalRemainderCircuit m c d = Node (buildEqualityCicuit d c) (OR, d, "== " ++ show c ++ " || == " ++ show (m+c)) (buildEqualityCicuit d (c+m))


-- Minimizing

deduplicateHelper :: InternalBooleanCircuit -> (HashMap.HashMap String Int, PC.BooleanCircuit Int) -> (HashMap.HashMap String Int, PC.BooleanCircuit Int)
deduplicateHelper (Leaf s) (hmap, (gates, edges)) = if HashMap.member (show s) hmap
                                        then (hmap, (gates, edges))
                                        else (HashMap.insert (show s) (length gates) hmap, (gates ++ [PC.Input s], edges))
deduplicateHelper (Const b) (hmap, (gates, edges)) = if HashMap.member (show b) hmap
                                        then (hmap, (gates, edges))
                                        else (HashMap.insert (show b) (length gates) hmap, (gates ++ [mappedGate], edges))
    where
        mappedGate = case b of
            T -> PC.ConstT
            F -> PC.ConstF
            Any -> PC.Input 0       -- we map the any inputs to 0 so that it reacts with helpers. thus we get a higher possibility to trigger that reaction later
deduplicateHelper (Node lIn (gate, d, desc) rIn) (hmap, (gates, edges)) =
    if HashMap.member gateDesc updatedMap
        then (updatedMap, updatedCicuit)
        else (HashMap.insert gateDesc (length newGates) updatedMap, (newGates ++ [mappedGate], newEdges ++ [(length newGates, (updatedMap HashMap.! leftDesc, updatedMap HashMap.! rightDesc))]))
    where
        mappedGate = case gate of
            AND -> PC.AND
            OR -> PC.OR
            NOT -> PC.NOT
        leftDesc = case lIn of
            (Node _ (_, lD, lDesc) _) -> show lD ++ "; " ++ lDesc
            (Leaf s) -> show s
            (Const b) -> show b
        rightDesc = case rIn of
            (Node _ (_, rD, rDesc) _) -> show rD ++ "; " ++ rDesc
            (Leaf s) -> show s
            (Const b) -> show b
        gateDesc = show d ++ "; " ++ desc
        leftRes = deduplicateHelper lIn (hmap, (gates, edges))
        rightRes = deduplicateHelper rIn leftRes
        updatedMap = fst rightRes
        updatedCicuit = snd rightRes
        newGates = fst updatedCicuit
        newEdges = snd updatedCicuit

deduplicate :: InternalBooleanCircuit -> PC.BooleanCircuit Int
deduplicate circuit = snd $ deduplicateHelper circuit (HashMap.empty, ([], []))


-- we do not have to deduplicate anything here because each circuit has different input variables. thus two circuits cannot have the same gates and each circuit itself is already deduplicated
-- except all the Input 0 gates or do I? those gates aint real gates after all
buildUnionCircuitHelper :: Predicates.Predicate -> ([PC.BooleanCircuit String], PC.BooleanCircuit String) -> ([PC.BooleanCircuit String], PC.BooleanCircuit String)
buildUnionCircuitHelper (Predicates.Leaf _) (circuit:xs, (gates, edges)) = (xs, (gates ++ fst circuit, edges ++ map renameEdges (snd circuit)))
    where
        renameIndex = (+) (length gates)
        renameEdges (a, (b, c)) = (renameIndex a, (renameIndex b, renameIndex c))
buildUnionCircuitHelper (Predicates.NodeN _ pred) (circuits, (gates, edges)) = (fst newCircuit, (fst (snd newCircuit) ++ [PC.Input "0", PC.NOT], snd (snd newCircuit) ++ [edge]))
    where
        newCircuit = buildUnionCircuitHelper pred (circuits, (gates, edges))
        edge = (length (fst $ snd newCircuit) + 1, (length (fst $ snd newCircuit) - 1, length (fst $ snd newCircuit)))
buildUnionCircuitHelper (Predicates.NodeP lPred op rPred) (circuits, (gates, edges)) = (fst rightCircuit, (newGates ++ [gate], newEdges ++ [(length newGates, (leftGate, rightGate))]))
    where
        leftCircuit = buildUnionCircuitHelper lPred (circuits, (gates, edges))
        leftGate = length (fst $ snd leftCircuit) - 1
        rightCircuit = buildUnionCircuitHelper rPred leftCircuit
        newGates = fst $ snd rightCircuit
        newEdges = snd $ snd rightCircuit
        rightGate = length newGates - 1
        gate = case op of
            Predicates.AND -> PC.AND
            Predicates.OR -> PC.OR


-- Export

buildThresholdCircuit :: Int -> Int -> PC.BooleanCircuit Int
buildThresholdCircuit c d = deduplicate $ buildInternalThresholdCircuit c d

buildRemainderCircuit :: Int -> Int -> Int -> PC.BooleanCircuit Int
buildRemainderCircuit m c d = deduplicate $ buildInternalRemainderCircuit m c d

-- | The states within each circuit must already been renamed
buildUnionCircuit :: Predicates.Predicate -> [PC.BooleanCircuit String] -> PC.BooleanCircuit String
buildUnionCircuit pred circuits = snd $ buildUnionCircuitHelper pred (circuits, ([], []))
