{-# Language BangPatterns, ParallelListComp, FlexibleInstances, DeriveGeneric #-}

module GIJoe.InsideOutside where

import GIJoe.Types
import GIJoe.Utils
import GIJoe.Chart
import GIJoe.Grammar


import Prelude hiding (sum, lookup)
import qualified Prelude

import Numeric.Log 

import Control.Monad
import Data.List.Split (splitOn)
import Data.List (foldl', sortBy, sort, maximumBy, foldl1')
import qualified Data.Foldable as Foldable (toList)
import Control.Monad.Logic
import Data.Function (on)

import Control.Monad.State.Strict
import Control.Monad.Random
import Control.Monad.Identity
import Control.Parallel.Strategies 
import qualified Control.DeepSeq as DeepSeq (NFData, rnf)
import Numeric.SpecFunctions (digamma)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.IntSet (IntSet) 
import qualified Data.IntSet as IntSet

import GHC.Generics (Generic)
import Data.Hashable

import Data.Tree
import Data.Tree.Pretty 

import Debug.Trace

import qualified Data.Sequence as Seq

type Charts a = [Chart a]
newtype Chart a = Chart {unChart :: HashMap (Int, Int) (HashMap Symbol a)}
                    deriving Show

lookup :: Int -> Int -> Symbol -> Chart a -> Maybe a
lookup i j sym (Chart m) = do m_sym <- HashMap.lookup (i, j) m
                              HashMap.lookup sym m_sym

{-# INLINE lookupDefault #-}
lookupDefault :: Int -> Int -> Symbol -> a -> Chart a  -> a
lookupDefault i j sym def chart = case lookup i j sym chart of
  Nothing -> def
  Just a -> a

insert :: Show a => Int -> Int -> Symbol -> a -> Chart a -> Chart a
insert i j sym a (Chart m_ij) = Chart m_ij'
  where m_ij' =
          case HashMap.lookup (i, j) m_ij of
            Just m_sym -> let m_sym' = HashMap.insert sym a m_sym
                          in HashMap.insert (i, j) m_sym' m_ij
            Nothing ->
              HashMap.insert (i, j) (HashMap.singleton sym a) m_ij

lookupLoc :: Int -> Int -> Chart a -> Maybe (HashMap Symbol a)
lookupLoc i j !(Chart m_i) = HashMap.lookup (i, j) m_i

insertLoc :: Int -> Int -> HashMap Symbol a -> Chart a -> Chart a
insertLoc i j m_sym (Chart m_i) = Chart $ HashMap.insert (i, j) m_sym m_i

empty :: Chart a
empty = Chart $ HashMap.empty

singleton :: Int -> Int -> Symbol -> a -> Chart a
singleton i j sym a =
  Chart $ HashMap.singleton (i, j) (HashMap.singleton sym a)

showChart :: Show a => Chart a -> String
showChart (Chart mp) = unlines [show i ++ " " ++ show j ++ " " ++ show sym ++ ": " ++ show v
                                 | ((i, j), m_sym) <- HashMap.toList mp, (sym, v) <- HashMap.toList m_sym]


data ParseState = ParseState {alphaChartSt :: Chart Double',
                              betaChartSt  :: Chart Double',
                              grammarSt :: Grammar,
                              startSt :: Symbol,
                              sentenceSt :: Sentence}
                deriving Show

-- | Inside probabilities: alpha(A, i, j)
alphas :: Grammar
       -> Int -- ^ pruning size
       -> Sentence
       -> Chart Double'
alphas gr b xs = flip evalState empty $ trace ("top: " ++ show (1, m)) $
                 do _ <- alphas' 1 m
                    get
  where m = Seq.length xs

        alphas' :: Int -> Int -> State (Chart Double') (HashMap Symbol Double')
        alphas' i j | i == j = {-# SCC "alphas1" #-}  process_msgs
          where msgs = toStateT $ do
                  rule <- liftList $ rulesWithUnaryChild gr x
                  let sym = headSymbol rule
                  return $ (sym, weight rule)
                  where x = Seq.index xs (i-1)
                process_msgs = do
                  ks <- msgs -- :: [(Symbol, Double')]
                  let c = HashMap.fromList ks
                  modify $ \ch -> insertLoc i j c ch
                  return $ c

        alphas' i j =  {-# SCC "alphas2" #-} process_msgs
          where msgs = toStateT $ do
                  k <- liftList [i..j-1]
                  chart  <- get
                  !left_cell <- {-# SCC "lookup_alphas_left" #-} case lookupLoc i k chart of
                    Just lc -> return lc
                    Nothing -> lift $ do c <- alphas' i k
                                         return c
                  !right_cell <- {-# SCC "lookup_alphas_right" #-} case lookupLoc (k+1) j chart of
                      Just rc -> return rc
                      Nothing -> lift $ do c <- alphas' (k+1) j
                                           return c
                  (left_sym, left_alpha) <- liftList . HashMap.toList $  left_cell
                  (right_sym, right_alpha) <- liftList . HashMap.toList $  right_cell                  
                  rule <- liftList $ binaryRulesWithChildren gr (left_sym, right_sym)
                  let !a = weight rule
                          * left_alpha
                          * right_alpha
                  return $ (headSymbol rule, a)

                process_msgs = do
                  xs <- msgs
                  let m_sym =  {-# SCC "alphas_insertWith" #-}
                        foldl' (\m (s, w) -> HashMap.insertWith (+) s w m) HashMap.empty xs
                  modify $ \ch -> insertLoc i j m_sym ch
                  return m_sym

-- | Inside probabilities: alpha(A, i, j)
betas :: Grammar
       -> Symbol -- ^ start symbol
       -> Int -- ^ pruning limit
       -> Sentence
       -> Chart Double' -- ^ alpha table
       -> Chart Double'
betas gr start b xs !alphaChart = evalState (go >> get) empty
  where go = sequence $ do
          i <- [1..m]
          j <- [i..m]
          return $ betas' i j
        m = Seq.length xs
--        liftList = ListT . return
        
        betas' :: Int -> Int
                  -> State (Chart Double')
                  (HashMap Symbol Double')
        betas' i j | i == 1 && j == m = {-# SCC "betas1" #-} do
          modify $! \ch -> insert 1 m start 1  ch
          return $! HashMap.singleton start 1
          
        betas' i j = {-# SCC "betas2" #-}  process_msgs
          where leftCaseMsgs = toStateT $ do
                  !k <- liftList [j+1..m]
                  betaChart  <- lift get
                  !betaCell <- case lookupLoc i k betaChart of
                    Just bc -> return bc
                    Nothing -> lift $ do c <- betas' i k
                                         return c
                  !alphaCell <- case lookupLoc (j+1) k alphaChart of
                      Just rc -> return rc
                      Nothing -> return $ HashMap.empty

                  (head_sym, beta_msg) <- liftList . HashMap.toList $  betaCell
                  (right_sym, alpha_msg) <- liftList . HashMap.toList  $ alphaCell

                  let !r1 = ruleIdsWithRightChild gr right_sym
                      !r2 = rulesHeadedById gr head_sym
                      !rules = getRulesById gr $ IntSet.intersection r1 r2
                  !rule <- liftList $ rules
                  guard $ right_sym /= leftChild rule
                  let !l = weight rule *
                             beta_msg *
                             alpha_msg
                  return $! (leftChild rule, l)
                             
                rightCaseMsgs = toStateT $ do
                  !k <- liftList [1..i-1]
                  betaChart  <- lift get
                  !betaCell <- case lookupLoc k j betaChart of
                    Just bc -> return bc
                    Nothing -> lift $ do c <- betas' k j
                                         return c
                  !alphaCell <- case lookupLoc k (i-1) alphaChart of
                      Just rc -> return rc
                      Nothing -> return $ HashMap.empty

                  (head_sym, beta_msg)  <- liftList . HashMap.toList $  betaCell
                  (left_sym, alpha_msg) <- liftList . HashMap.toList $ alphaCell

                  let r1 = ruleIdsWithLeftChild gr left_sym
                      r2 = rulesHeadedById gr head_sym
                      rules = getRulesById gr $ IntSet.intersection r1 r2
                  rule <- liftList $ rules
                  let !r = weight rule *
                             beta_msg *
                             alpha_msg
                  return $! (rightChild rule, r)
                             
                process_msgs = do
                  ls <- leftCaseMsgs
                  rs <- rightCaseMsgs
                  let !allMsgs = ls ++ rs
                  let !m_sym = {-# SCC "betas_insertWith" #-} foldl' (\m (s, w) -> HashMap.insertWith (+) s w m) HashMap.empty allMsgs
                  modify $ \ch -> insertLoc i j m_sym ch
                  return $! m_sym


getMapParse :: Monad m => StateT ParseState m Parse
getMapParse = do
  xs <- gets sentenceSt
  let m = Seq.length xs
  start <- gets startSt
  gr <- gets grammarSt
  mapParse gr 1 m start
  
  where mapParse gr i j sym | i /= j = do
--          let liftList = ListT . return
          xs <- toStateT $ do
                k <- liftList [i..j]
                left_m <- lift $ meritSymbolSpan i k
                (left_sym, left_mu) <- liftList $ HashMap.toList left_m
                right_m <- lift $ meritSymbolSpan (k+1) j
                (right_sym, right_mu) <- liftList $ HashMap.toList right_m
                rule@(BinaryRule _ _ _ w) <- liftList $ getBinaryRulesBySymbols gr sym left_sym right_sym
                return $ (rule,
                          left_mu *
                          right_mu *
                          w, k)
          let snd3 (_, b, _) = b
              (rule, p, k) = maximumBy (compare `on` snd3) $ Foldable.toList xs
          lParse <- mapParse gr i k (leftChild rule) 
          rParse <- mapParse gr (k+1) j (rightChild rule)
          return $! Parse rule [lParse, rParse]
          
        mapParse gr i j sym | i == j = do
          xs <- gets sentenceSt
          let rule = head $ getUnaryRulesBySymbols gr sym (xs `Seq.index` (i - 1))
          return $ Parse rule []
            
          
makeCharts :: Grammar
           -> Symbol
           -> Int
           -> Sentence
           -> (Chart Double', Chart Double')
makeCharts gr start b xs = (alphaChart, betaChart)
  where !alphaChart = alphas gr b xs
        !betaChart  = betas gr start b xs alphaChart
        m = Seq.length xs

withChartsT :: Monad m =>
              Grammar
           -> Symbol
           -> Int 
           -> Sentence
           -> StateT ParseState m a
           -> m (a, ParseState)
withChartsT gr start b xs m = runStateT m
                            $ ParseState alphaChart betaChart gr start xs
  where (alphaChart, betaChart) = makeCharts gr start b xs

withCharts gr start b xs m  = runIdentity $ withChartsT gr start b xs m

meritSymbolSpan :: Monad m =>
             Int -> Int -> StateT ParseState m (HashMap Symbol Double')
meritSymbolSpan i j = do
  alphaChart <- gets alphaChartSt
  betaChart <- gets betaChartSt
  gr <- gets grammarSt
  lnZ <- loglikelihood  
  let collectMus = do
       a_m_sym <- lookupLoc i j alphaChart
       b_m_sym <- lookupLoc i j betaChart
       let xs = do
            (sym, a) <- HashMap.toList a_m_sym
            let b = case HashMap.lookup sym b_m_sym of
                  Nothing -> 0
                  Just b -> b
            return $! (sym, a * b)
       Just $ foldl' (\m (r, c) -> HashMap.insertWith (+) r c m) HashMap.empty xs
  case collectMus of
      Just xs -> return xs
      Nothing -> return HashMap.empty

meritRuleIKJ :: Monad m => Int -> Int -> Int -> StateT ParseState m (HashMap Rule Double')
meritRuleIKJ i k j = do
    alphaChart <- gets alphaChartSt
    betaChart <- gets betaChartSt
    gr <- gets grammarSt
    let collectMus = do
         left_m_sym <- lookupLoc i k alphaChart
         right_m_sym <- lookupLoc (k+1) j alphaChart
         head_m_sym <- lookupLoc i j betaChart
         let xs = do
              (h, bh) <- HashMap.toList head_m_sym
              (r, ar) <- HashMap.toList right_m_sym
              (l, al) <- HashMap.toList left_m_sym
              rule <- getBinaryRulesBySymbols gr h l r 
              return $! (rule, bh * weight rule * ar * al)
         Just $ foldl' (\m (r, c) -> HashMap.insertWith (+) r c m) HashMap.empty xs
    case collectMus of
      Just xs -> return xs
      Nothing -> return HashMap.empty

meritRuleIJ :: Monad m => Int -> Int -> StateT ParseState m (HashMap Rule Double')
meritRuleIJ i j | i == j = do
  mp <- meritSymbolSpan i j
  xs <- gets sentenceSt
  gr <- gets grammarSt
  let rs = [(r, val) | (sym, val) <- HashMap.toList mp,
                  r <- getUnaryRulesBySymbols gr sym (xs `Seq.index` (i - 1))]
  return $ HashMap.fromList rs
meritRuleIJ i j = do
  mps <- sequence $ [meritRuleIKJ i k j | k <- [i..j]]
  if null mps
     then return HashMap.empty
     else return $ foldl1' (HashMap.unionWith (+)) mps

alpha :: Monad m => Int -> Int -> StateT ParseState m (HashMap Symbol Double')
alpha i j = do
  alphaChart <- gets alphaChartSt
  case lookupLoc i j alphaChart of
    Nothing -> return HashMap.empty
    Just m_sym -> return $! m_sym

beta :: Monad m => Int -> Int -> StateT ParseState m (HashMap Symbol Double')
beta i j = do
  betaChart <- gets betaChartSt
  case lookupLoc i j betaChart of
    Nothing -> return HashMap.empty
    Just m_sym -> return $! m_sym

probRuleIJ :: Monad m => Int -> Int -> StateT ParseState m (HashMap Rule Double')
probRuleIJ i j = do
  lnZ <- loglikelihood
  mp <- meritRuleIJ i j
  let z = Exp lnZ
  return $ HashMap.map (/z) mp

debugProbs :: Monad m => StateT ParseState m [(Int, Int, Rule, Double')]
debugProbs = do
  xs <- gets sentenceSt
  let m = Seq.length xs
  ys <- toStateT $ do
    i <- liftList $ [1..m]
    j <- liftList $ [i..m]    
    mp <- lift $ probRuleIJ i j
    (r, p) <- liftList $ HashMap.toList mp
    return $ (i, j, r, p)
  return $ filter (\(_, _, _, p) -> p > 1) ys
  


loglikelihood :: Monad m => StateT ParseState m Double
-- | This is only the loglikelihood of the sentence if the grammar is normalized
loglikelihood = do
      xs <- gets sentenceSt
      alphaChart <- gets alphaChartSt
      start <- gets startSt
      let m = Seq.length xs
          (Just (Exp lnZ)) = lookup 1 m start alphaChart
      return $ lnZ

loglikelihoodCorpus gr sym b corpus = Prelude.sum [go x | x <- corpus]
  where go x = fst $ withCharts gr sym b x $ loglikelihood

expectedCounts :: State ParseState (HashMap Rule Double')
expectedCounts = do m1 <- binary_case_mp
                    m2 <- unary_case_mp
                    return $ HashMap.unionWith (+) m1 m2
  where binary_case_mp = do
          xs <- gets sentenceSt
          start <- gets startSt
          alphaChart <- gets alphaChartSt
          let m = Seq.length xs
              (Just _Z) = lookup 1 m start alphaChart
          mu_maps <- toStateT $ do 
            i <- liftList $ [1..m-1]
            k <- liftList $ [i..m-1]
            j <- liftList $ [k+1..m]
            lift $ meritRuleIKJ i k j
          if null mu_maps
            then return HashMap.empty
            else return $ HashMap.map (/_Z) $ foldl1 (HashMap.unionWith (+)) mu_maps
        unary_case_mp = do
          xs <- gets sentenceSt
          start <- gets startSt
          alphaChart <- gets alphaChartSt
          gr <- gets grammarSt
          let m = Seq.length xs
              (Just _Z) = lookup 1 m start alphaChart
          vs <- toStateT $ do
            (sym, i) <- liftList $ zip (Foldable.toList xs) [1..m]
            mu_i <- lift $ meritSymbolSpan i i
            rule <- liftList $ rulesWithUnaryChild gr sym
            let c = case HashMap.lookup (headSymbol rule) mu_i of
                  Nothing -> 0
                  Just x -> x
            return $ (rule, c)
          let mp = foldl' (\m (r, c) -> HashMap.insertWith (+) r c m) HashMap.empty vs
          return $ HashMap.map (/_Z) mp
          

showCounts :: HashMap Rule Double' -> String
showCounts mp = unlines $ map show' $ HashMap.toList mp
  where show' (r, v) = showRulePart r ++ ": " ++ "ln " ++ show (ln v)
          

emIteration :: Grammar
            -> Symbol -- ^ start symbol
            -> Int -- ^ pruning limit
            -> Corpus
            -> (Grammar, -- ^ output grammar
                Double, -- ^ loglikelihood of corpus in resulting grammar
                Double)  -- ^ entropy of parses
-- | @emIteration gr start b corpus@ runs a single iteration of EM on
-- the corpus. Returns a resulting grammar and its associated
-- loglikelihood.
emIteration gr start b corpus = (gr', ll, h)
  where (css, lls, hs) = unzip3 $ withStrategy (parList rdeepseq) $ do
           xs <- corpus
           trace (show xs) $ return ()
           return $ fst $ withCharts gr start b xs $ do
             ll <- loglikelihood
             cs <- expectedCounts
             h <- entropyParses
             return (cs, ll, h)
        !ll = Prelude.sum lls
        !h = Prelude.sum hs
        !counts = foldl1' (HashMap.unionWith (+)) css
        !_K = fromIntegral $ length (allNonTerminals gr)
        !_Ws = meanFieldDirMultRules gr (1.0/(_K**2)) counts
        
        gr' = modifyRules gr reweight 
          where reweight r = r{weight = maybe 0 id (HashMap.lookup r _Ws)}

type EMLog = [(Grammar, Double)]

em :: Grammar
   -> Symbol
   -> Int -- ^ prune limit
   -> Corpus
   -> Int -- ^ max number of iterations
   -> Double -- ^ tolerance e.g. 1e-6, or set to 0 to use all iterations
   -> EMLog
em gr start b corpus maxIter tol = go gr negInfty maxIter 
  where go _ _ 0 = []
        go g best_ll i = infoOut i $ (g, ll) : go gr' ll (i - 1) 
          where (gr', ll, h) = emIteration g start b corpus
                infoOut i = trace $ unlines
                            ["-------------",
                             "em: iteration " ++ show i ++ " loglike: " ++ show best_ll,
                             "H: " ++ show h]
        negInfty = read "-Infinity"


meanFieldDirMult :: Double' -- ^ concentration parameter
                 -> [Double'] -- ^ counts c_i
                 -> [Double'] -- ^ weights w_i
{-# INLINE meanFieldDirMult #-}                 
meanFieldDirMult alpha counts
-- Calculate the mean field weights of 
  = ws
  where _K = toDouble' $ fromIntegral $ length counts
        psiAll = digamma' $  _K * alpha + sum counts 
        psis = map (digamma' . (+alpha)) counts
        ws = [Exp (psi - psiAll ) | psi <- psis]
        digamma' = digamma . fromDouble'

-- pow (Exp x) y = Exp $ x * y
-- digamma' = toDouble' . digamma . fromDouble'
-- expDigammaSmall x = (Exp $ digamma (x' + 6))
--                     + Exp 

meanFieldDirMultRules :: Grammar -- ^ nonterminal symbols in grammar
                      -> Double' -- ^ alpha, concentration parameter
                      -> HashMap Rule Double' -- ^ counts for each rule
                      -> HashMap Rule Double' -- ^ mean field weights for each rule
meanFieldDirMultRules gr alpha m_counts =
  HashMap.fromList $ concat $ map go (allNonTerminals gr)
  where go sym = zip rules ws
          where rules = rulesHeadedBy gr sym
                cs = [maybe 0 id (HashMap.lookup r m_counts) | r <- rules]
                ws = meanFieldDirMult alpha cs
                
entropyParses :: Monad m => StateT ParseState m Double
-- | Compute the entropy of the distribution over parses, q(z).
-- | O(n^2 * K) 
entropyParses = do
  xs <- gets sentenceSt
  let m = Seq.length xs
--      liftList = ListT . return 
  vs <- toStateT $ do
    i <- liftList $ [1..m] 
    j <- liftList $ [i..m]
    mp <- lift $ probRuleIJ i j
    (Exp lnp) <- liftList $ HashMap.elems mp
    let p = exp lnp
    return $ negate $ if p == 0 then 0 else p * lnp
  return $! Prelude.sum vs

--- Utils ---
