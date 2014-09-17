
module Main where

import Prelude hiding (lookup)

import GIJoe.Grammar
import GIJoe.Parse
import GIJoe.InsideOutside

import System.Random
import Control.Monad.Random
import Control.Monad
import Control.Monad.Trans
import Data.List hiding (lookup)
import Data.Function

main = do
  let n = 50
  let xs = stringToSentence $ ('a':) $ concat $ replicate (n-1) " a"
  let lexicon = [T "a"]
  let _K = 5
  let _S = N 0 Nothing
  let gr = initGrammar _S lexicon _K
  let alphaChart = {-# SCC "alphaChart" #-} alphas gr 1000000 xs
  putStrLn "Complexity Test"
  putStrLn $ "K: " ++ show _K
  putStrLn $ "n: " ++ show n
  putStrLn $ "xs: " ++ show xs
  forM [(i, j, t) | i <-[1..n], j<-[i..n], t <- allNonTerminals gr] $ \(i, j, t) ->
    putStrLn $ "alpha " ++ show i ++ " " ++ show j ++ " n " ++ show t  ++ " : " ++ show (lookup i j t alphaChart)
  -- let betaChart = {-# SCC "betaChart" #-} betas gr _S 1000000 xs alphaChart      
  -- forM [1..n] $ \i -> putStrLn $ "betas 1 n _S: " ++ show (lookup i i _S betaChart)
