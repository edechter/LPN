
module Main where

import GIJoe.Grammar
import GIJoe.Induction hiding (main)

import System.Random
import Control.Monad.Random
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Function



main = do
  xs <- readFile
        "/Users/edechter/Dropbox/Projects/GrammarInduction/data/corpuses/simpleNumbers.txt"
  let corpus = map stringToSentence . lines $ xs
  print corpus
  let lexicon = nub $ concat corpus
      _K = 6
      gr = initGrammar _ROOT lexicon _K
      prune_limit = 1000000000
  print lexicon
  gr' <- evalRandIO $ randomizeGrammar gr
  print gr'
  let emlog = em gr' _ROOT prune_limit corpus 10 0
  print $ map snd emlog
  let gr_final = fst . last $ emlog

  let test_corpus = corpus
  sequence $ do
    xs <- test_corpus
    let (prs, _) = withCharts gr_final _ROOT pruneLimit xs $ getMapParse
    return $ print prs
  return ()

          
