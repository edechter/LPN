{-# Language BangPatterns,  FlexibleInstances, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module GIJoe.ApproximateInference.InsideOutside where

import Prelude hiding (lookup, sum)
import qualified Prelude

import GIJoe.ApproximateInference.BoundaryProbabilities
import GIJoe.ApproximateInference.HashTable
import GIJoe.ApproximateInference.MaxPath
import GIJoe.Grammar
import GIJoe.Parse

import GIJoe.Types
import GIJoe.Utils

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
_DEBUG = 0
debugM level x = when (_DEBUG >= level) $ do !_ <- trace x $ return ()
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
                            

type UpperBound s = (Symbol, Int, Int) -> ST s Double'
type Epsilon = Double'
type EdgeGenerator s = (Symbol, Int, Int) -> ST s [(Rule, Int, Int, Int)]
type AlphaTable s = HashTable s (Symbol, Int, Int) Double'
type BetaTable s = HashTable s (Symbol, Int, Int) Double'

htSize :: Int -- ^ length of sentence
       -> Int -- ^ number of nonterminal symbols in grammar
       -> Int -- ^ size of hashTable needed
htSize n k = n * (n+1) `div` 2 * k 

inside :: Grammar
       -> Symbol -- ^ start symbol
       -> Sentence
       -> UpperBound s
       -> Epsilon
       -> EdgeGenerator s
       -> ST s (AlphaTable s)
inside gr start xs ubFunc epsilon edgeGenerator = do
    let n = Seq.length xs 
    !ht <- newSized $ htSize n (numSymbols gr) -- generate empty hashtable

    traverseOR ht (start, 1, n)
    return ht
  where 
    -- traverseOR :: forall s. AlphaTable s -> (Symbol, Int, Int) -> ST s Double'
    traverseOR _  (sym, i, j) | i == j = {-# SCC traverseOR #-} do
         let v = sum $ map weight $ getUnaryRulesBySymbols gr sym (xs `Seq.index` (i - 1))
         return v
    traverseOR ht  (sym, i, j) = {-# SCC traverseOR #-}
                                do children <- edgeGenerator (sym, i, j)
                                   alpha <- go 0 children -- collect alpha weights from children
                                   insert ht (sym, i, j) alpha -- insert alpha into hash table for this node
                                   return alpha
      where 
            go !a [] = return a
            go !a (node:ns) = do v <- traverseAND ht node a -- get alpha from this AND node
                                 go (a + v) ns
                                  
    traverseAND ht (rule, i, k, j) lb = do
      debugM 1 $ "entering edge: " ++ show (rule, i, k, j) 
      debugM 1 $ "current lb: " ++ show lb
      
      !_ <- _incrCounter _globalCounter `seq` return ()
      let w = weight rule
          _B = leftChild rule
          _C = rightChild rule
          
      v_l <- lookup ht (_B, i, k)
      v_r <- lookup ht (_C, k+1, j)

      let hit_l = isJust v_l
          hit_r = isJust v_r

      -- omegaTable <- mkOmegaTable gr
      u_l <- lambdaUpperBound gr (_B, i, k)      

      debugM 1 $ "u_l of " ++ show (_B, i, k) ++ ": " ++ show u_l
      u_r <- lambdaUpperBound gr (_C, k+1, j)
      -- u_r <- bookEndsBounds gr xs omegaTable (_C, k+1, j)      
      debugM 1 $ "u_r of " ++ show (_C, k+1, j) ++ ": " ++ show u_r 

      debugM 1 $ "left child: _B, i, k: " ++ show (_B, i, k) 
      debugM 1 $ "right child: _C, k+1, j: " ++ show (_C, k+1, j) 


      case True of
        _ | hit_l && hit_r -> do
            debugM 1 "hit_l && hit_r: backtrack, returning w * fromJust v_l * fromJust v_r" 
            return $! w * fromJust v_l * fromJust v_r
        _ | hit_l -> do
            debugM 1 "hit_l but not hit_r" 
            let ub = w * fromJust v_l * u_r
            debugM 1 $ "upperbound w * fromust v_l * u_r= " ++ show ub
            if ub < epsilon * lb
               then do debugM 1 $ "upperbound is less than epsilon * lb: " ++ show (epsilon * lb) 
                       debugM 1 $ "shortcircuit at " ++ show (_C, k+1, j)
                       return $ ub
               else do debugM 1 $ "upperbound is greater than than epsilon * lb: " ++ show (epsilon * lb) 
                       debugM 1 $ "drill down into right node: " ++ show (_C, k+1, j) 
                       a_r <- traverseOR ht (_C, k+1, j)
                       debugM 1 $ "pop up into edge: " ++ show (rule, i, k, j)
                       return $ w * fromJust v_l * a_r
        _ | hit_r -> do
            debugM 1 "hit_r but not hit_l" 
            let ub = w * fromJust v_r * u_l
            debugM 1 $ "upperbound w * fromust v_r * u_l= " ++ show ub
            if ub < epsilon * lb
               then do debugM 1 $ "upperbound is less than epsilon * lb: " ++ show (epsilon * lb)
                       debugM 1 $ "shortcircuit at " ++ show (_B, i, k)
                       return $ ub
               else do debugM 1 $ "upperbound is greater than than epsilon * lb: " ++ show (epsilon * lb)
                       debugM 1 $ "drill down into left node: " ++ show (_B, i, k)
                       a_l <- traverseOR ht (_B, i, k)
                       debugM 1 $ "pop up into edge: " ++ show (rule, i, k, j)
                       return $ w * fromJust v_r * a_l
        _        -> do
           debugM 1  "hit neither" 
           let ub = w * u_r * u_l
           debugM 1 $ "upperbound w * u_l * u_r = " ++ show ub
           if ub < epsilon * lb
               then do debugM 1 $ "upperbound is less than epsilon * lb: " ++ show (epsilon * lb)
                       debugM 1 $ "shortcircuit at " ++ show (_B, i, k)
                       return $ ub
               else do debugM 1 $ "upperbound is greater than than epsilon * lb: " ++ show (epsilon * lb)
                       debugM 1 $ "drill down into left node: " ++ show (_B, i, k)
                       a_l <- traverseOR ht (_B, i, k)
                       debugM 1 $ "a_l: "  ++ show a_l
                       debugM 1 $ "pop up into edge: " ++ show (rule, i, k, j)
                       debugM 1 $ "check whether w * a_l * u_r < epsilon * lb"
                       let ub' = w * a_l * u_r
                       debugM 1 $ "w * a_l * u_r: " ++ show ub'
                       if ub' < epsilon * lb
                          then do debugM 1 $ "upperbound is less than epislon * lb"
                                  debugM 1 $ "shortcircuit at " ++ show (_C, k+1, j)
                                  return $ ub'
                          else do debugM 1 $ "upperbound is greater than than epsilon * lb: " ++ show (epsilon * lb)
                                  debugM 1 $ "drill down into right node: " ++ show (_C, k+1, j)
                                  a_r <- traverseOR ht (_C, k+1, j)
                                  debugM 1 $ "pop up into edge: " ++ show (rule, i, k, j)
                                  return $ w * a_l * a_r
                       
        
      

      -- a_l <- lookup ht (_B, i, k) >>= \case
      --   Nothing -> do if trace ("should I drill into left node? w * u_l * u_r: " ++ show (w * u_l * u_r) ++ " epsilon * lb : " ++ show (epsilon * lb)) $ w * u_l < epsilon * lb
      --                    then do trace ("no, short-circuit: returning w*u_l = " ++ show (w * u_l)) $ return (w * u_l)
      --                    else do trace ("drill down into left node: " ++ show (_B, i, k) ) $ return ()
      --                            a_l <- traverseOR ht (_B, i, k)
      --                            trace ("pop up into edge: " ++ show (rule, i, k, j) ) $ return ()
      --                            return a_l
      --   Just v -> do trace ("hit node: " ++ show (_B, i, k)) $ return ()
      --                return v

      -- trace ("w * a_l: " ++ show (w * a_l*u_r) ++ " epsilon * lb : " ++ show (epsilon * lb )) $ return ()
      
      -- lookup ht (_C, k+1, j) >>= \case
      --   Nothing -> do if u_r * w * a_l < epsilon * lb
      --                   then return $! w * a_l * u_r
      --                   else do trace ("drill down into right node: " ++ show (_C, k+1, j) ) $ return ()
      --                           a_r <- traverseOR ht (_C, k+1, j)
      --                           trace ("pop up into edge: " ++ show (rule, i, k, j )) $ return ()                                
      --                           return $ w * a_l * a_r
      --   Just a_r -> do trace ("hit right node: " ++ show (_C, k+1, j)) $ return ()
      --                  return $ w * a_l * a_r
                 

                      
      -- case True of
      --   _ | hit_l && hit_r -> return $! w * fromJust v_l * fromJust v_r
      --   _ |
        
      -- u_l <- ubFunc (_B, i, k)
      -- u_r <- ubFunc (_C, k+1, j)

      -- case True of
      --   _ | hit_l && hit_r -> do
      --        return $! w * fromJust v_l * fromJust v_r
      --   _ | hit_r -> do if u_l < epsilon * lb / (w * fromJust v_r)
      --                     then do debugM $ "DEBUG: short-circuit left"
      --                             return $! w * u_l * fromJust v_r
      --                     else do debugM $ "DEBUG: no short-circuit"
      --                             a_l <- traverseOR ht  (_B, i, k)
      --                             return $! w * a_l * fromJust v_r
      --   _ | hit_l -> do if u_r < epsilon * lb / (w * fromJust v_l)
      --                     then do return $! w * u_r * fromJust v_l
      --                     else do debugM $ "DEBUG: no short-circuit"
      --                             a_r <- traverseOR ht  (_C, k+1, j)
      --                             return $! w * a_r * fromJust v_l
      --   _         -> do if u_r * u_l < epsilon * lb / w 
      --                     then do 
      --                             return $! w * u_l * u_r
      --                     else do 
      --                             a_l <- traverseOR ht  (_B, i, k)
      --                             if u_r < epsilon * lb / (w * a_l)
      --                                then do --(trace $ "YES: " ++ show [u_r, epsilon, lb, w, a_l] ) return ()
      --                                        --(trace $ show (_B, i, k) ++ " " ++ show (_C, k+1, j)) $ return ()
      --                                        return $! w * a_l * u_r
      --                                else do --(trace $ "NO: " ++ show [u_r, epsilon, lb, w, a_l] ) return ()
      --                                        --(trace $ show (_B, i, k) ++ " " ++ show (_C, k+1, j)) $ return ()
      --                                        a_r <- traverseOR ht  (_C, k+1, j)
      --                                        --(trace $ "a_r: " ++ show a_r) $ return ()
      --                                        return $! w * a_l * a_r 
            
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
                    -> UpperBound s
                    -> Epsilon
                    -> EdgeGenerator s
                    -> ST s (AlphaTable s, BetaTable s)
insideOutside gr start xs ubFunc epsilon edgeGenerator = do
  !alphaTable <- inside gr start xs ubFunc epsilon edgeGenerator
  !betaTable <- outside gr start xs alphaTable
  return (alphaTable, betaTable)



------------------------------------------
-- Edge generator with ordering heuristics


-- Rule edge generator with rules sorted by decreasing conditional
-- weight
generateByRuleWeight :: Grammar -> (Symbol, Int, Int) -> ST s [(Rule, Int, Int, Int)]
generateByRuleWeight gr (sym, i, j) = assert (i /= j) $ 
      return [(r, i, k, j) | r <- rules, k<- ks]
      where ks = [i..j-1]
            rules = sortBy (flip compare `on` weight) (binaryRulesHeadedBy gr sym)

-- Rule edge generator with rules sorted according to boundary
-- probabilty table Omega: Omega_{A, x, y} = p(A ->* x..y)
generateByOmegaWeights :: Grammar
                       -> Sentence
                       -> OmegaTable s
                       -> (Symbol, Int, Int) 
                       -> ST s [(Rule, Int, Int, Int)]
generateByOmegaWeights gr xs omegaTable (_A, i, j) = do
  let _LookAhead=10
  splits <- sequence $ do
     !r@(BinaryRule _ _B _C w) <- binaryRulesHeadedBy gr _A
     !k <- [i..(min (i+_LookAhead) (j-1))]
     let x_r = xs `Seq.index` (i - 1)
         x_l = xs `Seq.index` (k - 1)
         y_r = xs `Seq.index` k
         y_l = xs `Seq.index` (j - 1)
     return $ do lookup omegaTable (_B, x_r, x_l) >>= \case
                   Nothing -> return ((r, k), 0)
                   Just omega_l ->
                     lookup omegaTable (_C, y_r, y_l) >>= \case
                       Nothing -> return ((r, k), 0)
                       Just omega_r -> do
                         let !v = w * omega_l * omega_r
                         return $! ((r, k), v)
  let rest = [(r, i, k, j) | r <- binaryRulesHeadedBy gr _A,
                k <- [((min (i+_LookAhead) (j-1)) +1)..j-1]]
  let sorted = {-# SCC "sorted/sortBy" #-} sortBy (flip compare `on` snd) splits
  return $ [(r, i, k, j) | ((r, k), _) <- sorted] ++ rest


-- Rule edge generator with rules sorted by the product of the rule
-- weight and the resulting constitutent length upper bounds
generateByCompositeUpperBound ::
  Grammar -> Maybe Int -> (Symbol, Int, Int) -> ST s [(Rule, Int, Int, Int)]
generateByCompositeUpperBound gr n (_A, i, j) = assert (i /= j) $ do
      choices <- splits
      let edges = map fst (sortBy (flip compare `on` snd) choices)
      case n of
        Just m -> return $ take m edges
        Nothing -> return edges
      where ks = [i..j-1]
            splits = sequence $ do
              r@(BinaryRule _ _B _C w) <- binaryRulesHeadedBy gr _A
              k <- [i..j-1]
              let n_l = k-i+1
                  n_r = (j - i + 1) - n_l
              return $ do ul <- singleSourceMaxWeight gr _B n_l
                          ur <- singleSourceMaxWeight gr _C n_r
                          let !score = w * ul * ur
                          return $ ((r, i, k, j), score)

generateByLambdaUpperBound :: Grammar -> (Symbol, Int, Int) -> ST s [(Rule, Int, Int, Int)]
generateByLambdaUpperBound gr (_A, i, j) = return $ 
  map fst $ sortBy (flip compare `on` snd) choices
  where lambdaTable = mkLambdaTable gr (j-i)
        choices = do
          k <- [i..j-1]
          r@(BinaryRule _ _B _C w) <- binaryRulesHeadedBy gr _A
          let n_l = k-i+1
              n_r = (j - i + 1) - n_l
          let ul = HashMap.lookupDefault 0 (_B, n_l) lambdaTable
              ur = HashMap.lookupDefault 0 (_C, n_r) lambdaTable
              !score = w * ul * ur
          return $ ((r, i, k, j), score)
          
          
          
              
              


------------------------------------
-- Upper Bound Functions

trivialUpperBound :: UpperBound s
trivialUpperBound _ = return $ read "Infinity"

unityUpperBound :: UpperBound s
unityUpperBound _ = return $ 1.0


omegaUpperBound :: Grammar 
                -> Sentence
                -> OmegaTable s
                -> UpperBound s
omegaUpperBound w xs omegaTable (_A, i, j) = do
      lookup omegaTable (_A, xs `Seq.index` (i-1), xs `Seq.index` (j-1)) >>= \case
        Nothing -> return 1.0 
        Just x -> return $ x * 1.0

bookEndsBounds :: Grammar
               -> Sentence 
               -> OmegaTable s
               -> UpperBound s
bookEndsBounds gr xs omegaTable (_A, i, j) = do
      let n = j - i + 1
      p <- if n > 2
              then singleSourceMaxWeight gr _A n 
              else return 1
      lookup omegaTable (_A, xs `Seq.index` (i-1), xs `Seq.index` (j-1)) >>= \case
        Nothing -> error "bookEndsBounds/Nothing"
        Just x -> return $ x * p

constituentLengthBound :: Grammar
                       -> UpperBound s
constituentLengthBound gr (_A, i, j) = do
  let n = j - i + 1
  singleSourceMaxWeight gr _A ((1 + ceiling (logBase 2 (fromIntegral n))))

lambdaUpperBound :: Grammar -> UpperBound s
lambdaUpperBound gr (_A, i, j) = return $ HashMap.lookupDefault 0 (_A, n) table
  where n = j - i +1
        table = mkLambdaTable gr n 
      

likelihood :: Grammar
              -> Symbol
              -> Sentence
              -> UpperBound s
              -> Epsilon
              -> EdgeGenerator s
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

        
  
