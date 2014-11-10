{-# Language BangPatterns, LambdaCase #-}

module GIJoe.ApproximateInference.MaxPath where

import GIJoe.Grammar
import GIJoe.Types
import GIJoe.Parse

import GIJoe.ApproximateInference.HashTable (HashTable)
import qualified GIJoe.ApproximateInference.HashTable as HashTable

import Numeric.Log

import Data.Maybe
import Data.List (nub)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.Loop

import Control.Monad
import Control.Monad.Trans
import Control.Monad.ST
import Control.Applicative
import Control.Monad.State.Strict

import Data.PSQueue (PSQ, Binding(..))
import qualified Data.PSQueue as PSQ

import Debug.Trace

type WeightTable s = HashTable s (Symbol, Symbol) Double'


mkChildWeights :: Grammar -> ST s (WeightTable s)
-- | Return a hashtable which maps a (_A, _B) to the probability that
-- either direct child of _A is a _B. We collapse all terminals into
-- one special terminal (T "!").
mkChildWeights gr = do
  !ht <- HashTable.newSized $ k * (k + h) 
  exec_ $ do
    rule <- forEach $ grammarRules gr
    case rule of
      BinaryRule h l r w ->
        do lift $ HashTable.insertWith ht (+) (h, l) w
           when (l /= r) $ 
             lift $ HashTable.insertWith ht (+) (h, r) w
      UnaryRule h _ w -> lift $ HashTable.insertWith ht (+) (h, t) w
  return ht 
  where
    -- because these are IntSets, we are not overcounting the rule _A
    -- -> _B_B
    nts = nonterminals gr
    terms = terminals gr
    k = length nts
    h = length terms
    t = T "!"

type NegLogProb = Double
data OpenElement = Root Int | NT (Symbol, Int) deriving (Ord, Show, Eq)
type Open =  PSQ OpenElement NegLogProb
type DistanceTable = HashMap (Symbol, Int) NegLogProb

singleSourceMaxWeight gr _A n =
  maxWeight gr (NT (_A, n))

maxWeight :: Grammar -> OpenElement -> ST s Double'
maxWeight gr openElement = do
  edge_ht <- mkChildWeights gr
  go edge_ht init_open >>= \case
    Nothing -> return 0
    Just w -> return $ Exp (negate w)
  where
    init_open = case openElement of
      Root n -> PSQ.fromList $ [c :-> 0 | c <- children (Root n)]
      NT (_A, n) -> PSQ.singleton (NT (_A, n)) 0
        
    go :: WeightTable s -> Open -> ST s (Maybe Double)
    go edge_ht open =
          let view = PSQ.minView open
          in case view of
            Nothing -> return Nothing
            Just (NT u@(T _, 0) :-> d_u, open') -> return $ Just d_u
            Just (NT u@(s_u, m_u) :-> d_u, open') ->
              let vs = children (NT u)
              in loop edge_ht open' vs
                where 
                   loop edge_ht open [] = go edge_ht open
                   loop edge_ht open (v:vs) = do
                     let (NT (s_v, m_v)) = v
                     HashTable.lookup edge_ht (s_u, s_v) >>= \case
                       Nothing -> error $ "maxPaths/go/2: edge " ++ show (s_v, s_u) ++ " not found in edge hashtable"
                       Just (Exp negW_uv) ->
                         let w_uv = negate negW_uv 
                             open' = PSQ.alter (relax d_u w_uv) v open
                         in loop edge_ht open' vs

    relax d_u w_uv Nothing = Just $ d_u + w_uv
    relax d_u w_uv (Just d_v) | d_v > d_v' = Just d_v'
                              | otherwise  = Just d_v
      where d_v' = d_u + w_uv

    children :: OpenElement -> [OpenElement]
    children (Root n) = [NT (_A, n) | _A <- nonterminals gr]
    children (NT (_, 0)) = []
    children (NT (_A, m)) = map (\a -> NT (a, m-1)) . nub . concat $ do
      r <- rulesHeadedBy gr _A
      case r of
        (BinaryRule _ _B _C _) -> return [_B, _C]
        (UnaryRule _ _B _) -> return [t]
        
    k = length $ terminals gr
    t = T "!"
            
          
            
        

  
