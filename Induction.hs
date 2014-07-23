

module Induction where

import System.Random
import Control.Monad.Random
import Control.Monad
import Grammar
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
  return ()

          


