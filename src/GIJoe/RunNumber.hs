
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
      _K = 10
      gr = initGrammar _ROOT lexicon _K
      prune_limit = 1000000
  print lexicon
  gr' <- evalRandIO $ randomizeGrammar gr
  print gr'
  let emlog = em gr' _ROOT pruneLimit corpus 2 0
  print $ map snd emlog
  let gr_final = fst . last $ emlog

  let gr_out_path = "/Users/edechter/Dropbox/Projects/GrammarInduction/data/grammars/test1.grammar"

  writeGrammar gr_out_path "grammar induction, K=10, 2 iterations" gr_final
  
  let test_corpus = corpus
  sequence $ do
    xs <- test_corpus
    let (prs, _) = withCharts gr_final _ROOT pruneLimit xs $ getMapParse
    return $ print prs
  return ()

          
