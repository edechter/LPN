{-# Language BangPatterns, ParallelListComp, FlexibleInstances, DeriveGeneric, LambdaCase #-}

module GIJoe.ApproximateInference.InsideOutside where

import Prelude hiding (lookup, sum)
import qualified Prelude

import GIJoe.Types
import GIJoe.Grammar
import GIJoe.ApproximateInference.HashTable
import GIJoe.Parse

-- import GIJoe.ApproximateInference.LazyDequeue
-- import qualified GIJoe.ApproximateInference.LazyDequeue as LazyDequeue

import Numeric.Log

import Control.Monad
import Control.Monad.ST
import Control.Exception (assert)

import Data.Sequence (Seq, (><), (|>), (<|), ViewL(..))
import qualified Data.Sequence as Seq

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace
_DEBUG = True
debugM x = if _DEBUG then trace x $ return () else return ()

-- type Set = Set

type UpperBound = (Symbol, Int, Int) -> Double'
type Epsilon = Double'
type AlphaTable s = HashTable s (Symbol, Int, Int) Double'
type BetaTable s = HashTable s (Symbol, Int, Int) Double'

htSize :: Int -- ^ length of sentence
       -> Int -- ^ number of nonterminal symbols in grammar
       -> Int -- ^ size of hashTable needed
htSize n k = n * (n+1) `div` 2 * k 

approxInside :: Grammar
             -> Symbol -- ^ start symbol
             -> Sentence
             -> UpperBound
             -> Epsilon
             -> ST s (AlphaTable s)
approxInside gr start xs ubFunc epsilon = do
    let n = length xs 
    !ht <- newSized $ htSize n (numSymbols gr) -- generate empty hashtable
    traverseOR ht (start, 1, n)
    return ht
  where 
    traverseOR :: AlphaTable s -> (Symbol, Int, Int) -> ST s Double'
    traverseOR _ (sym, i, j) | i == j = return $ 
      sum $ map weight $ getUnaryRulesBySymbols gr sym (xs !! (i - 1))
    traverseOR ht (sym, i, j) = do alpha <- go 0 children
                                   -- debugM $ "inserting: " ++ show (sym, i, j) ++ " " ++ show alpha
                                   insert ht (sym, i, j) alpha
                                   -- showTable ht >>= debugM
                                   return alpha
      where children = getChildren (sym, i, j)
            go !lb [] = return lb
            go !lb (node:ns) = do v <- traverseAND ht node lb
                                  go (lb + v) ns
                                  
    traverseAND :: AlphaTable s
                -> (Rule, Int, Int, Int)
                -> Double'
                -> ST s Double'
    traverseAND ht (rule, i, k, j) lb = do
      let w = weight rule
          _B = leftChild rule
          _C = rightChild rule
          
      v_l <- lookup ht (_B, i, k)
      v_r <- lookup ht (_C, k+1, j)

      let hit_l = isJust v_l
          hit_r = isJust v_r
        
      let u_l = ubFunc (_B, i, k)
          u_r = ubFunc (_C, k+1, j)

      case True of
        _ | hit_l && hit_r  ->
             return $! w * fromJust v_l * fromJust v_r
        _ | hit_r -> if u_l < epsilon * lb / (w * fromJust v_r)
                        then return $! w * u_l * fromJust v_r
                        else do a_l <- traverseOR ht (_B, i, k)
                                return $! w * a_l * fromJust v_r
        _ | hit_l -> if u_r < epsilon * lb / (w * fromJust v_l)
                        then return $! w * u_r * fromJust v_l
                        else do a_r <- traverseOR ht (_C, k+1, j)
                                return $! w * a_r * fromJust v_l
        _         -> if u_r * u_l < epsilon * lb / w 
                        then return $! w * u_l * u_r
                        else do a_l <- traverseOR ht (_B, i, k)
                                a_r <- traverseOR ht (_C, k+1, j)
                                return $! w * a_l * a_r 

    getChildren :: (Symbol, Int, Int) -> [(Rule, Int, Int, Int)]
    getChildren (sym, i, j) = assert (i /= j) $
      [(r, i, k, j) | r <- rules, k<- ks]
      where ks = [i..j-1]
            rules = binaryRulesHeadedBy gr sym

-- each element of Open is an AND node and one of its parent OR nodes
 type Open = LazyDequeue ((Rule, Int, Int, Int), (Symbol, Int, Int))
-- set of elements already expanded and whose children have already been placed on the Open list
-- type Expanded = Set ((Symbol, Int, Int))
            
approxOutside :: Grammar
              -> Symbol -- ^ start symbol
              -> Sentence
              -> AlphaTable s -- ^ alpha table
              -> ST s (BetaTable s)
approxOutside gr start xs alphaTable = {-# SCC approxOutsideMainLoop #-} do
  let n = length xs 
  !betaTable <- newSized $ htSize n (numSymbols gr) -- generate empty hashtable
  insert betaTable (start, 1, n) 1.0 -- initialize start node cell
  -- initialize open 
  let open  = [(start, 1, n)] 
  loop betaTable Set.empty open  
  return betaTable
  where
    loop _ children [] = return ()
    loop betaTable children ((_A, i, j):open') = do
      debugM $ "(_A, i, j): " ++ show (_A, i, j)
      debugM $ "open': " ++ show open'                 
      debugM $ "children: " ++ show children
      
      let go children [] = return children
          go children ((r, k):splits) = do
            let (BinaryRule _ _B _C w) = r
            Just pa_beta <- lookup betaTable (_A, i, j)
        
            children' <- getAlphaValue (_B, i, k) >>= \case
              Nothing -> return children
              Just a_l -> do insertWith betaTable (+) (_C, k+1, j) (pa_beta * a_l * w)
                             if not $ (_C, k+1, j) `Set.member` children
                               then return $! (_C, k+1, j) `Set.insert` children
                               else return children
                         
            children'' <- getAlphaValue (_C, k+1, j) >>= \case
              Nothing -> return children'
              Just a_r -> do insertWith betaTable (+) (_B, i, k) (pa_beta * a_r * w)
                             if not $ (_B, i, k) `Set.member` children'
                               then return $! (_B, i, k) `Set.insert` children'
                               else return children'
            go children'' splits
          
      children' <- go children [(r, k) | r <- binaryRulesHeadedBy gr _A, k <- [i..j-1]] 
          
      if null open'
        then loop betaTable Set.empty (Set.toList children')
        else loop betaTable children' open'


    getAlphaValue (_A, i, j) | i == j -- ^ we are not storing unary spans in the alpha table, so need to go to grammar
           = return $ Just $ sum $ map weight $ getUnaryRulesBySymbols gr _A (xs !! (i - 1))
    getAlphaValue (_A, i, j) -- ^ if not a unary span 
           = lookup alphaTable (_A, i, j)

approxInsideOutside :: Grammar
                    -> Symbol -- ^ start symbol
                    -> Sentence
                    -> UpperBound
                    -> Epsilon
                    -> ST s (AlphaTable s, BetaTable s)
approxInsideOutside gr start xs ubFunc epsilon = do
  !alphaTable <- approxInside gr start xs ubFunc epsilon
  !betaTable <- approxOutside gr start xs alphaTable
  return $! (alphaTable, betaTable)

trivialUpperBound :: UpperBound
trivialUpperBound _ = read "Infinity"

likelihood :: Grammar
              -> Symbol
              -> Sentence
              -> UpperBound
              -> Epsilon
              -> ST s Double'
likelihood gr start xs ubFunc epsilon = do
  ht <- approxInside gr start xs ubFunc epsilon
  v <- lookup ht (start, 1, length xs)
  return $ maybe 0 id v
  
