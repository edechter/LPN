{-# Language BangPatterns, ParallelListComp, FlexibleInstances, DeriveGeneric, LambdaCase #-}

module GIJoe.ApproximateInference.InsideOutside where

import Prelude hiding (lookup, sum)
import qualified Prelude

import GIJoe.Types
import GIJoe.Grammar
import GIJoe.ApproximateInference.HashTable
import GIJoe.Parse

import GIJoe.ApproximateInference.LazyDequeue
import qualified GIJoe.ApproximateInference.LazyDequeue as LazyDequeue

import Numeric.Log

import Control.Monad.ST
import Control.Exception (assert)

import Data.Sequence (Seq, (><), (|>), (<|), ViewL(..))
import qualified Data.Sequence as Seq

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Debug.Trace
_DEBUG = True
debugM x = if _DEBUG then trace x $ return () else return ()

type Set = HashSet

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
type Expanded = Set ((Symbol, Int, Int)) 
approxOutside :: Grammar
              -> Symbol -- ^ start symbol
              -> Sentence
              -> AlphaTable s -- ^ alpha table
              -> ST s (BetaTable s)
approxOutside gr start xs alphaTable = do
  let n = length xs 
  !betaTable <- newSized $ htSize n (numSymbols gr) -- generate empty hashtable
  insert betaTable (start, 1, n) 1.0 -- initialize start node cell
  -- initialize open and expanded lists
  let (open, expanded) = addChildrenToOpen (start, 1, n) LazyDequeue.empty Set.empty
  loop betaTable open expanded 
  return betaTable
  where
    -- loop :: BetaTable s -> Open -> ST s ()
    loop betaTable open expanded | LazyDequeue.null open = return ()
                                 | otherwise  = do
      let ((((BinaryRule _A _B _C w), i, k, j), parent), open') = dequeue open -- pop element off left end of open
      -- debugM $ "OPEN: " ++ show open'
      -- debugM $ "Popped node: " ++ show ((((BinaryRule _A _B _C w), i, k, j), parent))

      (open1, expanded1) <- getAlphaValue (_B, i, k) >>= \case  -- this syntactic feature is enabled by the LambdaCase GHC extension 
        Nothing -> return (open', expanded) -- if no element in alpha table, then the node is not in graph and we can stop
        Just a_l -> do Just parentBeta <- lookup betaTable parent
                       insertWith betaTable (+) (_C, k+1, j) (parentBeta * a_l * w)
                       let (open1, expanded1) = addChildrenToOpen (_C, k+1, j) open' expanded
                       return (open1, expanded1)
                       
      (open2, expanded2) <- getAlphaValue (_C, k+1, j)  >>= \case
        Nothing -> return (open1, expanded1) -- if no element in alpha table, then the node is not in graph and we can stop
        Just a_r -> do Just parentBeta <- lookup betaTable parent
                       insertWith betaTable (+) (_B, i, k) (parentBeta * a_r * w)
                       -- debugM $ show (_B, i, k)                       
                       let (open2, expanded2) = addChildrenToOpen (_B, i, k) open1 expanded1
                       -- debugM $ "open2: " ++ show open2
                       return (open2, expanded2)
      -- debugM $ show open2                         
      loop betaTable open2 expanded2

    addChildrenToOpen :: (Symbol, Int, Int) -> Open -> Expanded -> (Open, Expanded)
    -- | get the AND children of an OR node and add them to the OPEN
    -- cue, if the OR node has not been previously expanded.
    addChildrenToOpen (sym, i, j) open expanded | (sym, i, j) `Set.member` expanded = (open, expanded)
                                                | otherwise                         = (open', expanded')
                                                                                       
      where getChildren (sym, i, j) | i == j = [] -- if OR node is unary, no more children
                                    | otherwise =  [((r, i, k, j), (sym, i, j)) |
                                                   r <- binaryRulesHeadedBy gr sym,
                                                   k <- [i..j-1]]
            open' = enqueueMany (getChildren (sym, i, j)) open
            expanded' = (sym, i, j) `Set.insert` expanded

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
  
