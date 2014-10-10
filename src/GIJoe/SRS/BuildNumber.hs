module GIJoe.SRS.BuildNumber where

import Data.List
import System.Random
import System.Random.Shuffle (shuffle', shuffleM)
import Control.Monad.Random.Class

-- for all i,j,k \in [1..n], w \in lexicon
-- A_i(X,Y) <- A_j(X),A_k(Y).
-- A_i([w]).
-- Number <-- A_1(X).

grammarOfNumber :: Int -> FilePath -> IO ()
grammarOfNumber k outpath = do
  let lexicon = ["one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
  let binaryRules = ["A" ++ show i ++ "(X Y) <-- A" ++ show j ++ "(X), A" ++ show ki ++ "(Y)." | i <- [1..k], j <- [1..k], ki <- [1..k]]
  let unaryRules = ["A" ++ show i ++ "("++ w ++ ")."| i <- [1..k], w <- lexicon]
  let constantRule = ["Number(X) <-- A(X)."]
  writeFile outpath $ unlines $ binaryRules ++ unaryRules ++ constantRule



-- even if we generalize, we don't need every rule
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

-- so, the full grammar is:
-- for all i,j,k \in [1..n], w \in lexicon
-- A_i(12,34) <-- A_j(1,2),A_k(3,4).
-- A_i(12,34) <-- A_j(1,3),A_k(2,4).
-- A_i(12,34) <-- A_j(1,4),A_k(2,3).
-- A_i(X,Y) <-- A_j(Y,X).
-- Number(X) <-- A_1(X,Y).
-- A_i([w],[w]).

grammarOfNumber2 :: Int -> FilePath -> IO ()
grammarOfNumber2 n outpath = do
  let lexicon = ones ++ teens ++ tens ++ ["after", "comes"]
  let reorderRules = ["A" ++ show i ++ "(X Y, U V) <-- A" ++ show j ++ "(" ++ (ls !! 0) ++ "," ++ (ls !! 1) ++ "), A" ++ show k ++ "(" ++ (ls !! 2) ++ "," ++ (ls !! 3) ++ ")." | i <- [1..n], j <- [1..n], k <- [1..n], ls <- (permutations ["X","Y","U","V"]), j < k]
  let reorderRulesEqual = ["A" ++ show i ++ "(X Y, U V) <-- A" ++ show j ++ "(" ++ (ls !! 0) ++ "," ++ (ls !! 1) ++ "), A" ++ show j ++ "(" ++ (ls !! 2) ++ "," ++ (ls !! 3) ++ ")." | i <- [1..n], j <- [1..n], ls <- ((map (\x -> "X":x) (permutations ["Y","U","V"])) ++ (map (\y -> "Y":y) (permutations ["X","U","V"])))]
--  let terminalRules = ["word" ++ "("++ w ++ ")."| w <- lexicon]
  let relationRules = ["A" ++ show i ++ "(" ++ w1 ++ "," ++ w2 ++ ")." | i <- [1..n], w1 <- lexicon ++ ["null"] , w2 <- lexicon ++ ["null"]]
  let numberRule = ["Number(X) <-- A1(X,X)."]
  let nextRule = ["Next(X,Y) <-- A2(X,Y)."]
  let nextSentenceRule = ["NextSentence(X) <-- A2(X,Y)."]      
  writeFile outpath $ unlines $ reorderRules ++ reorderRulesEqual ++ relationRules ++ numberRule ++ nextRule ++ nextSentenceRule

writeWrapper :: Int -> Int -> FilePath -> IO ()
writeWrapper nNu nNe outpath = do
    nums <- giveNumbers nNu 10
    nexts <- giveNexts nNe 10
    writeData (nums ++ nexts) outpath

writeData :: [String] -> FilePath -> IO ()
writeData ss outpath = writeFile outpath $ "[" ++ intercalate ",\n" ss ++ "]."

numberLists :: [[String]]
numberLists = first19 ++ theRest
  where
    first19 = [[o] | o <- ones] ++ [[t] | t <- teens]
    theRest = concatMap theDecade tens
    theDecade x = [[x]] ++ [[x,o] | o <- ones]

giveNumbers :: Int -> Int -> IO [String]
giveNumbers n c = do
    gen <- getStdGen
    let nShuffledNumbers = take n $ shuffle' numberLists (length numberLists) gen
    return $ map (\x -> concat ["count(srs('Number_1'-[", showCleanList x, "]),", show c, ")"]) nShuffledNumbers

showCleanList xs = "[" ++ intercalate "," xs ++ "]"



giveNexts :: Int -> Int -> IO [String]
giveNexts n c = do
    gen <- getStdGen
    let nShuffledNexts = take n $ shuffle' (zip (init numberLists) (tail numberLists)) ((length numberLists) - 1) gen
    return $ map (\(x,y) -> concat ["count(srs('Next_2'-[", showCleanList x, ",", showCleanList y, "]),", show c, ")"]) nShuffledNexts
  
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

numberExamples = [[x] | x <- numberLists]
nextExamples = [[a, b] | (a, b) <- zip (init numberLists) (tail numberLists)]

-- Example: to generate file of 90% of the numbers and 20% of the nexts as training:
--  mkTrainTestDataFile <path>
--        [("Number_1", numberExamples , 0.90), ("Next_2", nextExamples , 0.20)]

nextSentences = [[["after"] ++ a ++ ["comes"] ++  b] | [a,b] <- nextExamples]




