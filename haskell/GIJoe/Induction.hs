

module GIJoe.Induction where

import GIJoe.Grammar
import GIJoe.Parse

import System.Random
import Control.Monad.Random
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Function



_ROOT = N 0 Nothing
short = stringToSentence "a dog ate a mouse"
shortCorpus = [short]
pruneLimit = 5

main = do
  let corpus_size = 100
  liangGrammar <- readGrammar "liangGrammar.txt"
  
  corpus <- replicateM corpus_size (sample liangGrammar _S)
  let lexicon = nub $ concat corpus
      _K = 10
      gr = initGrammar _ROOT lexicon _K
  gr' <- evalRandIO $ randomizeGrammar gr
--   print $ loglikelihoodCorpus gr' _ROOT pruneLimit corpus 
--   let p = parse gr' _ROOT (head corpus) 
  let emlog = em gr' _ROOT pruneLimit corpus 10 0
--   putStr $ unlines $ map show $ sortBy (compare `on` weight) $ grammarRules $ fst $ last emlog
--   print $ map snd emlog
  print $ map snd emlog
  let gr_final = fst . last $ emlog
  -- Test learned grammar by getting MAP parses on random samples from true grammar
  test_corpus <- replicateM 5 (sample liangGrammar _S)
  sequence $ do
    xs <- test_corpus
    let (prs, _) = withCharts gr0 _S pruneLimit xs $ getMapParse
    return $ print prs
  return ()

          


