
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
--  print corpus
  
  let _N0 = N 0 Nothing
  let lexicon = nub $ concat corpus
      _K = 6
      gr = initGrammar _N0 lexicon _K
--       prune_limit = 1000000
-- --
  -- print lexicon
--   gr <- readGrammar "data/grammars/test1.grammar"
  -- let corpus = map stringToSentence ["one", "two", "se ven", "se ven ty se ven"]
  gr' <- evalRandIO $ randomizeGrammar gr
  let emlog = em gr' _N0 100000 corpus 30 1e-3
  print $ map snd emlog
  -- print gr'
  -- print $ map nonTermName $allNonTerminals gr'
  -- print $ allNonTerminals gr'
  -- let emlog = em gr' _N0 pruneLimit [stringToSentence "se ven"] 10 0
  --print $ map snd
  let gr_final = fst . last $ emlog
  -- let gr_out_path = "/Users/edechter/Dropbox/Projects/GrammarInduction/data/grammars/test1.grammar"

  -- writeGrammar gr_out_path "grammar induction, K=10, 2 iterations" gr_final
  
  let test_corpus = corpus
  sequence $ do
    xs <- test_corpus
    let (prs, _) = withCharts gr_final _N0 pruneLimit xs $ getMapParse
    return $ print prs
  return ()

          
