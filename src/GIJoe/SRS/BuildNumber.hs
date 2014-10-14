module GIJoe.SRS.BuildNumber where

import Data.List
import System.Random
import System.Random.Shuffle (shuffle', shuffleM)
import Control.Monad.Random.Class

-- while we don't need every rule
-- for all A,B,C
-- A(X,Y) <- B(Y,X).          # #4# flip
-- A(12,34) <- B(1,2),C(3,4). # #1# both from a single predicate
-- A(12,34) <- B(1,2),C(4,3). # =1, flip C
-- A(12,34) <- B(1,3),C(2,4). # #2# one from each predicate
-- A(12,34) <- B(1,3),C(4,2). # =2, flip C
-- A(12,34) <- B(1,4),C(2,3). # #3#
-- A(12,34) <- B(1,4),C(3,2). # =3, flip C
-- A(12,34) <- B(2,1),C(3,4). # =1, flip B
-- A(12,34) <- B(2,1),C(4,3). # =1, flip B, flip C
-- A(12,34) <- B(2,3),C(1,4). # =3, reordered
-- A(12,34) <- B(2,3),C(4,1). # =3, reordered, flip C
-- A(12,34) <- B(2,4),C(1,3). # =2, reordered
-- A(12,34) <- B(2,4),C(3,1). # =2, reordered, flip C
-- A(12,34) <- B(3,1),C(2,4). # =2, flip B
-- A(12,34) <- B(3,1),C(4,2). # =2, flip B, flip C
-- A(12,34) <- B(3,2),C(1,4). # =3, reordered, flip B
-- A(12,34) <- B(3,2),C(4,1). # =3, reordered, flip B, flip C
-- A(12,34) <- B(3,4),C(1,2). # =1, reordered
-- A(12,34) <- B(3,4),C(2,1). # =1, reordered, flip C
-- A(12,34) <- B(4,1),C(2,3). # =3, flip B
-- A(12,34) <- B(4,1),C(3,2). # =3, flip B, flip C
-- A(12,34) <- B(4,2),C(1,3). # =2, reordered, flip B
-- A(12,34) <- B(4,2),C(3,1). # =2, reordered, flip B, flip C
-- A(12,34) <- B(4,3),C(1,2). # =1, reordered, flip B
-- A(12,34) <- B(4,3),C(2,1). # =1, reordered, flip B, flip C
--
-- we use every rule to avoid cycles in the explanation graph from flipping
-- so, the full grammar is:
-- for all i,j,k \in [1..n], w1,w2 \in lexicon, A,B,C,D \in permutations [X,Y,U,V]
-- A_i(XY,UV) <-- A_j(A,B),A_k(C,D).
-- A_i([w1],[w2]).
-- Number(X) <-- A_1(X,Y).
-- Next(X) <-- A_2(X,Y).

grammarOfNumber2 :: Int -> FilePath -> IO ()
grammarOfNumber2 n outpath = do
  let lexicon = ones ++ teens ++ tens ++ ["after", "comes"]
  let reorderRules = ["A" ++ show i ++ "(X Y, U V) <-- A" ++ show j ++ "(" ++ (ls !! 0) ++ "," ++ (ls !! 1) ++ "), A" ++ show k ++ "(" ++ (ls !! 2) ++ "," ++ (ls !! 3) ++ ")." | i <- [1..n], j <- [1..n], k <- [1..n], ls <- (permutations ["X","Y","U","V"]), j < k]
  let reorderRulesEqual = ["A" ++ show i ++ "(X Y, U V) <-- A" ++ show j ++ "(" ++ (ls !! 0) ++ "," ++ (ls !! 1) ++ "), A" ++ show j ++ "(" ++ (ls !! 2) ++ "," ++ (ls !! 3) ++ ")." | i <- [1..n], j <- [1..n], ls <- ((map (\x -> "X":x) (permutations ["Y","U","V"])) ++ (map (\y -> "Y":y) (permutations ["X","U","V"])))]
  let binaryRelationRules = ["A" ++ show i ++ "(" ++ (lexicon !! w1) ++ "," ++ (lexicon !! w2) ++ ")." | i <- [1..n], w1 <- [0..((length lexicon) - 1)] , w2 <- [0..((length lexicon) - 1)], w1 <= w2]
  let unaryRelationRules = ["A" ++ show i ++ "(" ++ (lexicon !! w1) ++ ",null)." | i <- [1..n], w1 <- [0..((length lexicon) - 1)] ]
  let numberRule = ["Number(X,Y) <-- A1(X,Y)."]
  let nextRule = ["Next(X,Y) <-- A2(X,Y)."]      
  writeFile outpath $ unlines $ reorderRules ++ reorderRulesEqual ++ binaryRelationRules ++ unaryRelationRules ++ numberRule ++ nextRule

numberLists :: [[String]]
numberLists = first19 ++ theRest
  where
    first19 = [[o] | o <- ones] ++ [[t] | t <- teens]
    theRest = concatMap theDecade tens
    theDecade x = [[x]] ++ [[x,o] | o <- ones]

showCleanList xs = "[" ++ intercalate "," xs ++ "]"
  
ones = ["one","two","three","four","five","six","seven","eight","nine"]
tens = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]

trainTestSplit :: (MonadRandom m)
                  => Double -- ^ percent of examples to be used for training
                  -> [Example]  -- ^ list of examples
                  -> m ([Example], [Example]) -- ^ (train_list, test_list)
trainTestSplit perc xs = do
  shuffled <- shuffleM xs
  let n = length xs
      n_train = round (fromIntegral n * perc)
  let (train, test) = splitAt n_train shuffled
  return (train, test)

trainTestDataToPrism :: String -- ^ predicate name
                     -> [Example] -- ^ train
                     -> [Example] -- ^ test
                     -> String
trainTestDataToPrism pred train test = unlines [trainString, testString]
  where wrapSrs x = "srs('" ++ pred ++ "'-[" ++ intercalate ", " (map showCleanList x) ++ "])"
        trainString = unlines [ "train(" ++ wrapSrs t ++ ")." | t <- train]
        testString = unlines [ "test(" ++ wrapSrs t ++ ")." | t <- test]        

type PredicateName = String
type Example = [[String]]
type PercTrain = Double
type TrainTestSpec = (PredicateName, [Example], PercTrain)

-- Example: to generate file of 90% of the numbers and 20% of the nexts as training:
--  mkTrainTestDataFile <path>
--        [("Number_1", numberExamples , 0.90), ("Next_2", nextExamples , 0.20)]
mkTrainTestDataFile :: FilePath
                    -> [TrainTestSpec]
                    -> IO ()
mkTrainTestDataFile path specs = do
  ss <- sequence $ do 
    (pred, xs, perc) <- specs
    return $ do
      (train,test) <- trainTestSplit perc xs
      return $ trainTestDataToPrism pred train test
  let out = unlines ss
  writeFile path out

decadeExamples = [[["after"] ++ a ++ ["comes"] ++ b,[""]] | (a,b) <- zip (init $ map (\x -> [x]) tens) (tail $ map (\x -> [x]) tens)]
numberExamples = [[x,[""]] | x <- numberLists]
nextExamples = [[a, b] | (a, b) <- zip (init numberLists) (tail numberLists)]
nextSentences = [[["after"] ++ a ++ ["comes"] ++  b,[""]] | [a,b] <- nextExamples]
