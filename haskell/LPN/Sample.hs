
module LPN.Sample where

import LPN.Type

import Control.Monad
import Control.Monad.Random
import Control.Applicative ((<$>))

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Debug.Trace

---- SAMPLING -------

sampleRule :: MonadRandom m => RewriteSystem -> Rule -> m ComplexTerm
sampleRule rs (headTerm :<-: []) = return headTerm
sampleRule rs (headTerm :<-: bodyTerms) = do
  subst <- Map.unions `liftM` mapM (sampleTermSubst rs) bodyTerms
  return $ headTerm `substCTermVars` subst
  where vars = cTermVars headTerm

sampleTerm rs term@(SimpleTerm pred vars) = do
  subst <- sampleTermSubst rs term
  return $ (ComplexTerm pred (map (NTString . (:[]) . ElVar) vars))  `substCTermVars` subst
  
sampleTermSubst :: MonadRandom m => RewriteSystem -> SimpleTerm -> m (Map Var NonTerminalString)
sampleTermSubst rs (SimpleTerm pred vars) = do
    i <- sampleCategorical $ normalizeWeights weights
    let (WRule rule w) = matchingRules !! i
    (ComplexTerm _ xs) <- sampleRule rs rule
    return $ Map.fromList (zip vars xs)
  where matchingRules = filter ((== pred) . cTermPredicate . ruleHead . weightedRuleRule)
                          $ rsRules rs
        weights = map weightedRuleWeight matchingRules

normalizeWeights ws= map (/_Z) ws
  where _Z = sum ws

sampleCategorical :: (MonadRandom m, Num a, Ord a, Random a) => [a] -> m Int
sampleCategorical ws = do
  r <- getRandomR (0, 1)
  return $ go r $ zip (cumsum ws) [0..]
  where go r ((w, i):[]) = i
        go r ((w, i):ws) | r < w = i
                         | otherwise = go r ws 
        
cumsum xs = reverse $ go [] xs
  where go ys [] = ys
        go [] (x:xs) = go [x] xs
        go zs@(y:_) (x:xs) = go (x+y:zs) xs
