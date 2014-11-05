{-# Language ParallelListComp #-}
module GIJoe.SRS.TestTrain where

import Data.List
import System.Random
import System.Random.Shuffle (shuffle', shuffleM)
import Control.Monad.Random.Class
import Control.Monad

import GIJoe.SRS.Type
import GIJoe.SRS.Parse

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
  where wrapSrs x = "prove('" ++ pred ++ "'-[" ++ intercalate ", " (map showCleanList x) ++ "])"
        trainString = unlines [ "train(" ++ show n ++ ", " ++ wrapSrs t ++ ")." | t <- train | n <-[1..]]
        testString = unlines [ "test(" ++ show n ++ ", "++ wrapSrs t ++ ")." | t <- test | n <- [1..]]        
        showCleanList xs = "[" ++ intercalate "," xs ++ "]"
