
module Main where

import GIJoe.Grammar

import System.Random
import Control.Monad.Random
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Function


type Lexicon = [Symbol]

initGrammar :: Symbol
            -> Lexicon
            -> Int     -- ^ K
            -> Grammar
initGrammar start lexicon _K = grammarFromRules $ binary_rules ++ unary_rules
  where binary_rules = do
          i <- [0.._K-1]
          j <- [0.._K-1]
          k <- [0.._K-1]
          return $! BinaryRule (N i Nothing) (N j Nothing) (N k Nothing) 1.0
        unary_rules = [UnaryRule (N i Nothing) l 1.0  | i <- [0.._K-1], l <- lexicon]


_ROOT = N 0 Nothing
short = stringToSentence "a dog ate a mouse"
shortCorpus = [short]
pruneLimit = 100

main = do
  let corpus_size = 100
  liangGrammar <- readGrammar "/Users/edechter/Dropbox/Projects/GrammarInduction/data/grammars/liangGrammar.txt"
  
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
    let (prs, _) = withCharts gr_final _ROOT pruneLimit xs $ getMapParse
    return $ print prs
  return ()

          


