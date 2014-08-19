{-# Language BangPatterns,  FlexibleInstances, LambdaCase #-}

module GIJoe.ApproximateInference.InsideOutside where

import Prelude hiding (lookup, sum)
import qualified Prelude

import GIJoe.Types
import GIJoe.Grammar
import GIJoe.ApproximateInference.HashTable
import GIJoe.Parse
import GIJoe.ApproximateInference.BoundaryProbabilities

-- import GIJoe.ApproximateInference.LazyDequeue
-- import qualified GIJoe.ApproximateInference.LazyDequeue as LazyDequeue

import Numeric.Log

import Control.Monad.Loop

import Control.Monad
import Control.Monad.Trans
import Control.Monad.ST
import Control.Exception (assert)

import Data.Sequence (Seq, (><), (|>), (<|), ViewL(..))
import qualified Data.Sequence as Seq

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap


import Data.Set (Set)
import qualified Data.Set as Set

import Data.Function (on)
import Data.List (sortBy)


import System.IO.Unsafe
import Data.IORef 

------------------------------------------------------------
-- DEBUG
import Debug.Trace
_DEBUG = False
debugM x = when _DEBUG $ do !_ <- trace x $ return ()
                            return ()

-- *TOP LEVEL GlOBAL* Counter for debugging
_globalCounter :: IORef Int
{-# NOINLINE _globalCounter #-}
_globalCounter = unsafePerformIO (newIORef 17) 

_zeroCounter :: IORef Int -> ()
{-# NOINLINE _zeroCounter #-}
_zeroCounter ref = let x = unsafePerformIO $ writeIORef ref 0 in x

_incrCounter :: IORef Int -> ()
{-# NOINLINE _incrCounter #-}
_incrCounter ref = let x = unsafePerformIO $ atomicModifyIORef' ref $ \a -> (a+1, ()) in x

_readCounter :: IORef Int -> Int
{-# NOINLINE _readCounter #-}
_readCounter ref = let x = unsafePerformIO $ readIORef ref in x
------------------------------------------------------------
                            

type UpperBound = (Symbol, Int, Int) -> Double'
type Epsilon = Double'
type EdgeGenerator = (Symbol, Int, Int) -> [(Rule, Int, Int, Int)]
type AlphaTable s = HashTable s (Symbol, Int, Int) Double'
type BetaTable s = HashTable s (Symbol, Int, Int) Double'

htSize :: Int -- ^ length of sentence
       -> Int -- ^ number of nonterminal symbols in grammar
       -> Int -- ^ size of hashTable needed
htSize n k = n * (n+1) `div` 2 * k 

inside :: Grammar
          -> Symbol -- ^ start symbol
          -> Sentence
          -> UpperBound
          -> Epsilon
          -> EdgeGenerator
          -> ST s (AlphaTable s)
inside gr start xs ubFunc epsilon edgeGenerator = do
    let n = Seq.length xs 
    !ht <- newSized $ htSize n (numSymbols gr) -- generate empty hashtable
    traverseOR ht (start, 1, n)
    return ht
  where 
    traverseOR :: AlphaTable s -> (Symbol, Int, Int) -> ST s Double'
    traverseOR _ (sym, i, j) | i == j = do
         !_ <- _incrCounter _globalCounter `seq` return ()
         return $ sum $ map weight $ getUnaryRulesBySymbols gr sym (xs `Seq.index` (i - 1))
    traverseOR ht (sym, i, j) =          
                                do !_ <- _incrCounter _globalCounter `seq` return ()
                                   alpha <- go 0 children
                                   insert ht (sym, i, j) alpha
                                   return alpha
      where children = edgeGenerator (sym, i, j)
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

      debugM $ "DEBUG: " ++ "AND node: " ++ show (rule, i, k, j) ++ " lb: " ++ show lb
      case True of
        _ | hit_l && hit_r  -> do
             return $! w * fromJust v_l * fromJust v_r
        _ | hit_r -> do if u_l < epsilon * lb / (w * fromJust v_r)
                          then do debugM $ "DEBUG: short-circuit left"
                                  return $! w * u_l * fromJust v_r
                          else do debugM $ "DEBUG: no short-circuit"
                                  a_l <- traverseOR ht (_B, i, k)
                                  return $! w * a_l * fromJust v_r
        _ | hit_l -> do if u_r < epsilon * lb / (w * fromJust v_l)
                          then do return $! w * u_r * fromJust v_l
                          else do debugM $ "DEBUG: no short-circuit"
                                  a_r <- traverseOR ht (_C, k+1, j)
                                  return $! w * a_r * fromJust v_l
        _         -> do if u_r * u_l < epsilon * lb / w 
                          then do -- debugM $ "DEBUG: short-circuit left and right"
                                  return $! w * u_l * u_r
                          else do -- debugM $ "DEBUG: no short-circuit"
                                  a_l <- traverseOR ht (_B, i, k)
                                  if u_r < epsilon * lb / (w * a_l)
                                     then return $! w * a_l * u_r
                                     else do a_r <- traverseOR ht (_C, k+1, j)
                                             return $! w * a_l * a_r 
            
outside :: Grammar
              -> Symbol -- ^ start symbol
              -> Sentence
              -> AlphaTable s -- ^ alpha table
              -> ST s (BetaTable s)
outside gr start xs alphaTable = do
  let n = Seq.length xs 
  !betaTable <- newSized $ htSize n (numSymbols gr) -- generate empty hashtable
  insert betaTable (start, 1, n) 1.0 -- initialize start node cell
  -- initialize open
  exec_ $ do
    ell <- forEach [n-x | x <- [1..n-1]] -- span length
    i   <- forEach [1..n-ell]
    let j = i + ell
    _A  <- forEach $ allNonTerminals gr
    lift (getAlphaValue (_A, i, j)) >>= \case
      Nothing -> return ()
      Just _  -> do
        k <- forEach [i..j-1]
        (BinaryRule _ _B _C w) <- forEach $ binaryRulesHeadedBy gr _A
        Just pa_beta <- lift $ lookup betaTable (_A, i, j)
        
        lift (getAlphaValue (_B, i, k)) >>= \case
          Nothing -> return ()
          Just a_l -> lift $ insertWith betaTable (+) (_C, k+1, j) (pa_beta * a_l * w)
                         
        lift (getAlphaValue (_C, k+1, j)) >>= \case
          Nothing -> return ()
          Just a_r -> lift $ insertWith betaTable (+) (_B, i, k) (pa_beta * a_r * w)
  return betaTable
  where                   
    getAlphaValue (_A, i, j) | i == j -- ^ we are not storing unary spans in the alpha table, so need to go to grammar
           = return $ Just $ sum $ map weight $ getUnaryRulesBySymbols gr _A (xs `Seq.index` (i - 1))
    getAlphaValue (_A, i, j) -- ^ if not a unary span 
           = lookup alphaTable (_A, i, j)

insideOutside :: Grammar
                    -> Symbol -- ^ start symbol
                    -> Sentence
                    -> UpperBound
                    -> Epsilon
                    -> EdgeGenerator
                    -> ST s (AlphaTable s, BetaTable s)
insideOutside gr start xs ubFunc epsilon edgeGenerator = do
  !alphaTable <- inside gr start xs ubFunc epsilon edgeGenerator
  !betaTable <- outside gr start xs alphaTable
  return (alphaTable, betaTable)



------------------------------------------
-- Edge generator with ordering heuristics

-- Rule edge generator with rules sorted by decreasing conditional
-- weight
generateByRuleWeight :: Grammar -> (Symbol, Int, Int) -> [(Rule, Int, Int, Int)]
generateByRuleWeight gr (sym, i, j) = assert (i /= j) $ 
      [(r, i, k, j) | r <- rules, k<- ks]
      where ks = [i..j-1]
            rules = sortBy (flip compare `on` weight) (binaryRulesHeadedBy gr sym)

-- Rule edge generator with rules sorted according to boundary
-- probabilty table Omega: Omega_{A, x, y} = p(A ->* x..y)
generateByOmegaWeights :: Grammar
                       -> Sentence
                       -> OmegaTable
                       -> (Symbol, Int, Int)
                       -> [(Rule, Int, Int, Int)]
generateByOmegaWeights gr xs !omegaTable (_A, i, j)
  = map fst $ sortBy (flip compare `on` snd) $ do
     r@(BinaryRule _ _B _C w) <- binaryRulesHeadedBy gr _A
     k <- [i..j-1]
     let x_r = xs `Seq.index` (i - 1)
         x_l = xs `Seq.index` (k - 1)
         y_r = xs `Seq.index` k
         y_l = xs `Seq.index` (j - 1)
         omega_l = HashMap.lookupDefault 0 (_B, x_r, x_l) omegaTable
         omega_r = HashMap.lookupDefault 0 (_C, y_r, y_l) omegaTable
         v = w * omega_l * omega_r
     -- !_ <- _incrCounter _globalCounter `seq` return ()
     -- !_ <- _incrCounter _globalCounter `seq` return ()              
     -- (trace $ "k: " ++ show k) $ return ()
     -- (trace $ "x_r: " ++ show x_r) $ return ()
     -- (trace $ "x_l: " ++ show x_l) $ return ()
     -- (trace $ "y_r: " ++ show y_r) $ return ()
     -- (trace $ "y_l: " ++ show y_l) $ return ()
     -- (trace $ "omega_l: " ++ show omega_l) $ return ()
     -- (trace $ "omega_r: " ++ show omega_r) $ return ()
     -- (trace $ "r: " ++ show r) $ return ()
     return ((r, i, k, j), v)
     
     

                 

trivialUpperBound :: UpperBound
trivialUpperBound _ = read "Infinity"

unityUpperBound :: UpperBound
unityUpperBound _ = 1.0

likelihood :: Grammar
              -> Symbol
              -> Sentence
              -> UpperBound
              -> Epsilon
              -> EdgeGenerator
              -> ST s Double'
likelihood gr start xs ubFunc epsilon edgeGenerator = do
  ht <- inside gr start xs ubFunc epsilon edgeGenerator
  v <- lookup ht (start, 1, Seq.length xs)
  return $ maybe 0 id v
  

--- EXTRA HELPERS
  
-- | add a small amount of probability mass to every rule that does
-- not already exist in the grammar
addWideSupport :: Grammar -> Double' -> Grammar
addWideSupport gr eps = grammarFromRules $ grammarRules gr' ++ bs ++ us
  where gr' = normalizeGrammar gr
        lexicon = terminals gr'
        nts = nonterminals gr'
        -- binary rules not already in grammar
        bs = [BinaryRule h l r eps | h <- nts, r <- nts, l <- nts, 
                     getBinaryRulesBySymbols gr h l r == []]
        -- unary rules not already in grammar
        us  = [UnaryRule h l eps | h <- nts, l <- lexicon,
                     getUnaryRulesBySymbols gr h l == []]

        
  
