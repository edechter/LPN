
module Main where

import GIJoe.Grammar
import GIJoe.Parse
import GIJoe.InsideOutside

import System.Random
import Control.Monad.Random
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Function

main = do
  xs <- readFile
       "/Users/edechter/Dropbox/Projects/GrammarInduction/data/corpuses/emails.txt"
  let corpus = take 1 $  map stringToSentence . lines $ xs
  let _N0 = N 0 Nothing
  let lexicon = nub $ concat corpus
      _K = 5
      gr = initGrammar _N0 lexicon _K
      prune_limit = 1000000
  gr' <- evalRandIO $ randomizeGrammar gr
  let emlog = em gr' _N0 100000 corpus 1 1e-3
  print $ map snd emlog
  let gr_final = fst . last $ emlog
  let gr_out_path = "/Users/edechter/Dropbox/Projects/GrammarInduction/data/grammars/emailGrammar1.grammar"

  writeGrammar gr_out_path "grammar induction, K=10, 30 iterations" gr_final


  -- ys <- readFile
  --      "/Users/edechter/Dropbox/Projects/GrammarInduction/data/corpuses/emails.txt"
  -- let test_corpus = map stringToSentence . lines $ ys
  -- sequence $ do
  --   xs <- test_corpus
  --   let (prs, _) = withCharts gr_final _N0 prune_limit xs $ getMapParse
  --   return $ print prs
  -- return ()

          

