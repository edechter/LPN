{-# Language BangPatterns, ParallelListComp, FlexibleInstances #-}

module Grammar where

import Prelude hiding (sum, lookup)
import qualified Prelude

import Numeric.Log 

import Control.Monad
import Data.List.Split (splitOn)
import Data.List (foldl', sortBy)
import Data.Function (on)
import Control.Monad.List
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Identity
import Control.Parallel.Strategies 
import qualified Control.DeepSeq as DeepSeq (NFData, rnf)
import Numeric.SpecFunctions (digamma)

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.PQueue.Prio.Max as MaxPQueue

import Debug.Trace

import qualified Data.Sequence as Seq

import qualified Data.MemoCombinators as Memo

import Text.Parsec.Prim hiding (parse, State)
import Text.Parsec hiding (parse, State)
import Text.ParserCombinators.Parsec.Number

data Symbol = N Int (Maybe String) --  non terminal
            | T String -- terminal
            | E -- empty symbol
            deriving (Ord, Eq)
instance Show Symbol where
  show (N _ (Just s)) = s
  show (N i Nothing) = "N" ++ show i
  show (T s) = s
  show E = "E"

type Double' = Log Double

memoSymbol :: Memo.Memo Symbol
memoSymbol f = table (m (f . uncurry N)) (m' (f . T)) f  
  where m = Memo.pair Memo.integral (Memo.maybe (Memo.list Memo.char))
        m' = Memo.list Memo.char
        table n t e (N i s) = n (i, s)
        table n t e (T s)   = t s
        table n t e E = e E


mkNonTerm :: Int -> String -> Symbol
mkNonTerm i s = N i (Just s)

data Rule = UnaryRule {headSymbol :: Symbol,
                       child :: Symbol,
                       weight :: Double'}
            | BinaryRule {headSymbol :: Symbol,
                        leftChild :: Symbol,
                        rightChild :: Symbol,
                        weight :: Double'}
          deriving (Ord, Eq)
instance Show Rule where
  show (UnaryRule s l w) = show s ++ " --> " ++  show l ++ " :: " ++ show w
  show (BinaryRule s l r w) = show s ++ " --> " ++  show l ++ " " ++ show r   ++ " :: " ++ show w

instance DeepSeq.NFData Rule where
  rnf (UnaryRule h c w) = h `seq` c `seq` w `seq` ()
  rnf (BinaryRule h l r w) = h `seq` l `seq` r `seq` w `seq` ()

instance NFData Rule where

isUnary :: Rule -> Bool
isUnary UnaryRule{} = True
isUnary _ = False

isBinary :: Rule -> Bool
isBinary = not . isUnary
                                                 
(->-) :: Symbol -> (Symbol, Symbol, Double') -> Rule
s ->- (l, r, w) = BinaryRule s l r w

(->>-) :: Symbol -> (Symbol, Double') -> Rule
s ->>- (l, w) = UnaryRule s l w

data Grammar = Grammar {headSymbolTable :: Map Symbol IntSet,
                        leftSymbolTable :: Map Symbol IntSet,
                        rightSymbolTable :: Map Symbol IntSet,
                        unarySymbolTable :: Map Symbol IntSet,
                        binaryRuleIds :: IntSet,
                        unaryRuleIds :: IntSet,
                        ruleIndex :: IntMap Rule 
                       } deriving (Ord, Eq)

grammarRules :: Grammar -> [Rule]
grammarRules gr = IntMap.elems . ruleIndex $ gr

getRuleById :: Grammar -> Int -> Maybe Rule
getRuleById gr i = IntMap.lookup i (ruleIndex gr)

getRulesById :: Grammar -> IntSet -> [Rule]
getRulesById gr ids = go [] $ IntSet.toList ids
  where go acc [] = acc
        go acc (x:xs) = case getRuleById gr x of
          Just x -> go (x:acc) xs
          Nothing -> go acc xs

getBinaryRuleIdsBySymbols :: Grammar -> Symbol -> Symbol -> Symbol -> IntSet
getBinaryRuleIdsBySymbols gr h l r = idsH `IntSet.intersection` idsL
                                     `IntSet.intersection` idsR
  where idsH = rulesHeadedById gr h
        idsL = ruleIdsWithLeftChild gr l
        idsR = ruleIdsWithRightChild gr r

getBinaryRulesBySymbols :: Grammar -> Symbol -> Symbol -> Symbol -> [Rule]
getBinaryRulesBySymbols gr h l r = getRulesById gr $
                                   getBinaryRuleIdsBySymbols gr h l r

rulesHeadedBy :: Grammar -> Symbol -> [Rule]
rulesHeadedBy gr sym = case Map.lookup sym (headSymbolTable gr) of
  Nothing -> []
  Just xs -> filterJusts $ map (getRuleById gr) $ IntSet.toList xs
  where filterJusts ((Just x):xs) = x:(filterJusts xs)
        filterJusts (Nothing:xs) = (filterJusts xs)
        filterJusts [] = []

rulesHeadedById :: Grammar -> Symbol -> IntSet
rulesHeadedById gr sym = case Map.lookup sym (headSymbolTable gr) of
  Nothing -> IntSet.empty
  Just xs -> xs

unaryRulesHeadedBy :: Grammar -> Symbol -> [Rule]
unaryRulesHeadedBy gr sym = getRulesById gr ruleIds
  where headedByIds = rulesHeadedById gr sym
        ruleIds = IntSet.intersection headedByIds (unaryRuleIds gr) 
        

binaryRulesHeadedBy :: Grammar -> Symbol -> [Rule]
binaryRulesHeadedBy gr sym = getRulesById gr ruleIds
  where headedByIds = rulesHeadedById gr sym
        ruleIds = IntSet.intersection headedByIds (binaryRuleIds gr)

binaryRulesWithChildren :: Grammar -> (Symbol, Symbol) -> [Rule]
binaryRulesWithChildren gr (l, r) = getRulesById gr $ binaryRuleIdsWithChildren gr (l, r)

binaryRuleIdsWithChildren :: Grammar -> (Symbol, Symbol) -> IntSet
binaryRuleIdsWithChildren gr (left_sym, right_sym) = IntSet.intersection r1 r2
  where r1 = ruleIdsWithLeftChild gr left_sym
        r2 = ruleIdsWithRightChild gr right_sym

ruleIdsWithLeftChild :: Grammar -> Symbol -> IntSet
ruleIdsWithLeftChild gr sym = 
  case Map.lookup sym (leftSymbolTable gr) of
    Nothing -> IntSet.empty
    Just ids -> ids

ruleIdsWithRightChild :: Grammar -> Symbol -> IntSet
ruleIdsWithRightChild gr sym = 
  case Map.lookup sym (rightSymbolTable gr) of
    Nothing -> IntSet.empty
    Just ids -> ids

ruleIdsWithUnaryChild :: Grammar -> Symbol -> IntSet
ruleIdsWithUnaryChild gr sym = 
  case Map.lookup sym (unarySymbolTable gr) of
    Nothing -> IntSet.empty
    Just ids -> ids

rulesWithLeftChild :: Grammar -> Symbol -> [Rule]
rulesWithLeftChild gr sym = getRulesById gr $ ruleIdsWithLeftChild gr sym

rulesWithRightChild :: Grammar -> Symbol -> [Rule]
rulesWithRightChild gr sym = getRulesById gr $ ruleIdsWithRightChild gr sym

rulesWithUnaryChild :: Grammar -> Symbol -> [Rule]
rulesWithUnaryChild gr sym = getRulesById gr $ ruleIdsWithUnaryChild gr sym


grammarFromRules :: [Rule] -> Grammar
grammarFromRules rs = foldl' insertRule emptyGrammar (zip rs [0..])
  where emptyGrammar = Grammar
                       Map.empty
                       Map.empty
                       Map.empty
                       Map.empty
                       IntSet.empty
                       IntSet.empty
                       IntMap.empty
        insertRule (Grammar hTable lTable rTable uTable bSet uSet index) (rule, i)
          = Grammar hTable' lTable' rTable' uTable' bSet' uSet' index'
          where hTable' = Map.insertWith (IntSet.union)
                         (headSymbol rule) (IntSet.singleton i) hTable
                index' = IntMap.insert i rule index
                (lTable', rTable', uTable', bSet', uSet') =
                  case rule of
                    BinaryRule{} -> (lTable', rTable', uTable, bSet', uSet)
                      where lTable' = Map.insertWith IntSet.union (leftChild rule)
                                      (IntSet.singleton i) lTable
                            rTable' = Map.insertWith IntSet.union (rightChild rule)
                                      (IntSet.singleton i) rTable
                            bSet' = IntSet.insert i bSet 
                    UnaryRule{} -> (lTable, rTable, uTable', bSet, uSet')
                      where uTable' = Map.insertWith IntSet.union (child rule)
                                      (IntSet.singleton i) uTable
                            uSet' = IntSet.insert i uSet 

allNonTerminals :: Grammar -> [Symbol]
allNonTerminals gr = Map.keys . headSymbolTable $  gr

modifyRuleWeightWithId :: Grammar -> Int -> (Double' -> Double') -> Grammar
modifyRuleWeightWithId gr i f = gr{ruleIndex=IntMap.adjust g i (ruleIndex gr)}
  where g r = r{weight= f (weight r)}

modifyRules :: Grammar -> (Rule -> Rule) -> Grammar
modifyRules gr f = gr{ruleIndex = foldl' go index (IntMap.keys index)}
  where go index i = IntMap.adjust f i index
        index = ruleIndex gr

getSymbolNormalization :: Grammar -> Symbol -> Double'
getSymbolNormalization gr sym = sum $ do rule <- rulesHeadedBy gr sym
                                         return $ weight rule

instance Show Grammar where
  show grammar = unlines $ map show (grammarRules grammar)

showSortedGrammar gr = unlines $ map show $ take 20 $ reverse $ sortBy (compare `on` weight) $ grammarRules $ normalizeGrammar gr

normalizeGrammar :: Grammar -> Grammar
normalizeGrammar gr = foldl' go gr (allNonTerminals gr)
  where go g sym = IntSet.foldl' (\g r -> modifyRuleWeightWithId g r (/_Z)) g ruleIds
          where _Z = getSymbolNormalization g sym
                ruleIds = rulesHeadedById gr sym

type Sentence = [Symbol]
type Corpus   = [Sentence]

data And a b = And a [b] deriving Show
data Or a b = Or a [b] deriving Show
newtype AndOr a b = AndOr (Or a (And b (AndOr a b))) 

instance (Show a, Show b) => Show (AndOr a b) where
  show d = showOr (0::Int) (0::Int) d
    where showOr i l (AndOr (Or s xs))
            = indent i $ show l ++ ") " ++ show s ++ "\n"
              ++ if null xs then []
                 else unlines $ map (indent i . showAnd (i+5) (l+1)) xs
          showAnd i l (And s xs) = (indent i $ show l ++ ") " ++ show s ++ "\n")
                                   ++ (unlines $
                                      [indent i $ showOr (i+5) (l+k) o|
                                       o <- xs | k <- [0..]])
          indent i = ((replicate i ' ') ++)

type ParseSet = AndOr (Symbol, Int, Int) Rule

recognize :: Grammar -> Symbol -> Sentence -> Bool
recognize gr sym xs = case parse gr xs sym 1 (length xs) of
  Nothing -> False
  Just _ -> True

parse :: Grammar
      -> Sentence
      -> Symbol
      -> Int
      -> Int
      -> Maybe ParseSet
parse gr xs = memo_parse  
  where memo = Memo.memo3 memoSymbol Memo.integral Memo.integral
        memo_parse = memo parse'
        xs' = Seq.fromList xs
        m = Seq.length xs'

        parse' :: Symbol
               -> Int
               -> Int
               -> Maybe ParseSet
        -- if sentence has has single element, look for matching rule
        parse' sym i j | j == i = if null parses then Nothing
                                     else Just $ AndOr $ Or (sym, i, j) parses
          where parses = do
                  rule <- grammarRules gr
                  guard $ isUnary rule
                  guard $ headSymbol rule == sym
                  guard $ child rule == x
                  return $! And rule []
                x = Seq.index xs' (i - 1)
                
        -- else
        parse' sym i j = 
          if null parses then Nothing
             else Just $ AndOr $ Or (sym, i, j) parses
          where parses = do
                  k <- [i..j-1]
                  rule <- grammarRules gr
                  guard $ isBinary rule
                  guard $ headSymbol rule == sym
                  let left_sym = leftChild rule
                      right_sym = rightChild rule
                  let left_parse = memo_parse left_sym i k
                  guard $ isJust left_parse
                  let right_parse = memo_parse right_sym (k+1) j
                  guard $ isJust right_parse
                  return $ And rule [fromJust left_parse, fromJust right_parse]


stringToSentence :: String -> Sentence
stringToSentence = map T . words

stringToCorpus :: String -> Corpus
stringToCorpus = map stringToSentence . splitOn "."

symbolMapToSymbolQueue m_sym = MaxPQueue.fromList . Map.toList $ m_sym

type Charts a = [Chart a]
newtype Chart a = Chart {unChart :: IntMap (IntMap (Map Symbol a, MaxPQueue Symbol a))}
                    deriving Show

lookup :: Int -> Int -> Symbol -> Chart a -> Maybe a
lookup i j sym (Chart m_i) = do m_j <- IntMap.lookup i m_i
                                (m_sym, _) <- IntMap.lookup j m_j
                                Map.lookup sym m_sym

{-# INLINE lookupDefault #-}
lookupDefault :: Int -> Int -> Symbol -> a -> Chart a  -> a
lookupDefault i j sym def chart = case lookup i j sym chart of
  Nothing -> def
  Just a -> a


insert :: Int -> Int -> Symbol -> a -> Chart a -> Chart a
insert = insertWith const  

insertWith :: (a -> a -> a) -> Int -> Int -> Symbol -> a -> Chart a -> Chart a
insertWith f i j sym a m          
  = insertWithKey (\_ _ _ sym' m' -> f sym' m') i j sym a m


insertWithKey :: (Int -> Int -> Symbol -> a -> a -> a)
                 -> Int -> Int -> Symbol -> a -> Chart a -> Chart a
insertWithKey f i j sym a (Chart m_i) = Chart m_i'
  where m_i' = IntMap.alter h i m_i
          where h Nothing = Just $ m_j_default
                h (Just m_j) = Just $ IntMap.alter g j  m_j
                  where g Nothing = Just $ (m_sym_default, MaxPQueue.singleton sym a)
                        g (Just (m_sym, syms))
                          = Just (Map.insertWithKey (f i j) sym a m_sym,
                                  MaxPQueue.insert sym a syms)
                m_j_default = IntMap.singleton j (m_sym_default, MaxPQueue.singleton sym a)
                m_sym_default = Map.singleton sym a

lookupLoc :: Int -> Int -> Chart a -> Maybe (Map Symbol a, MaxPQueue Symbol a)
lookupLoc i j (Chart m_i) = do  m_j <- IntMap.lookup i m_i
                                (m_sym, syms) <- IntMap.lookup j m_j
                                return $ (m_sym, syms)

insertLoc :: Int -> Int -> (Map Symbol a, MaxPQueue Symbol a) -> Chart a -> Chart a
insertLoc i j (m_sym, syms) (Chart m_i) =
  case IntMap.lookup i m_i of
    Nothing -> Chart $ IntMap.insert j m_j_def m_i
    Just m_j -> Chart $ IntMap.insert i (IntMap.insert j (m_sym, syms) m_j) m_i
  where m_j_def = IntMap.singleton j (m_sym, syms)

empty :: Chart a
empty = Chart $ IntMap.empty

singleton :: Int -> Int -> Symbol -> a -> Chart a
singleton i j sym a = insert i j sym a empty


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
alphas gr b xs = evalState (alphas' 1 m >> get) empty
  where xs' = Seq.fromList xs
        m = Seq.length xs'
        liftList = ListT . return

        alphas' :: Int -> Int -> State (Chart Double') (Map Symbol Double', MaxPQueue Symbol Double')
        alphas' i j | i == j = process_msgs
          where msgs =  runListT $ do
                  rule <- liftList $ rulesWithUnaryChild gr x
                  let sym = headSymbol rule
                  return $! (sym, weight rule)
                  where x = Seq.index xs' (i-1)
                process_msgs = do
                  ks <- msgs -- :: [(Symbol, Double')]
                  let c = Map.fromList ks
                      syms = symbolMapToSymbolQueue c
                  modify $ \ch -> insertLoc i j (c, syms) ch
                  return $! (c, syms)

        alphas' i j = process_msgs
          where msgs = runListT $ do
                  k <- liftList [i..j-1]
                  chart  <- get
                  left_cell <- case lookupLoc i k chart of
                    Just lc -> return lc
                    Nothing -> lift $ do c <- alphas' i k
                                         return c
                  right_cell <- case lookupLoc (k+1) j chart of
                      Just rc -> return rc
                      Nothing -> lift $ do c <- alphas' (k+1) j
                                           return c
                  left_sym <- fmap fst . liftList . MaxPQueue.take b . snd $ left_cell
                  let left_alpha = (fst left_cell) ! left_sym
                  right_sym <- fmap fst . liftList . MaxPQueue.take b . snd $ right_cell
                  let right_alpha = (fst right_cell) ! right_sym
                  rule <- liftList $ binaryRulesWithChildren gr (left_sym, right_sym)
                  return $! (headSymbol rule, weight rule * left_alpha * right_alpha)
                  
                process_msgs = do
                  xs <- msgs
                  let m_sym = foldl' (\m (s, w) -> Map.insertWith (+) s w m) Map.empty xs
                      syms = symbolMapToSymbolQueue m_sym
                  modify $ \ch -> insertLoc i j (m_sym, syms) ch
                  return $! (m_sym, syms)

-- | Inside probabilities: alpha(A, i, j)
betas :: Grammar
       -> Symbol -- ^ start symbol
       -> Int -- ^ pruning limit
       -> Sentence
       -> Chart Double' -- ^ alpha table
       -> Chart Double'
betas gr start b xs alphaChart
  = evalState (sequence [betas' i i | i <-[1..m]] >> get) empty
  where xs' = Seq.fromList xs
        m = Seq.length xs'
        liftList = ListT . return
        
        betas' :: Int -> Int
                  -> State (Chart Double')
                  (Map Symbol Double', MaxPQueue Symbol Double')
        betas' i j | i == 1 && j == m = do
          put $ insert 1 m start 1 empty
          return $ (Map.singleton start 1,
                    MaxPQueue.singleton start 1)
        
        betas' i j = process_msgs
          where leftCaseMsgs = runListT $ do
                  k <- liftList [j+1..m]
                  betaChart  <- lift get
                  betaCell <- case lookupLoc i k betaChart of
                    Just bc -> return bc
                    Nothing -> lift $ do c <- betas' i k
                                         return c
                  alphaCell <- case lookupLoc (j+1) k alphaChart of
                      Just rc -> return rc
                      Nothing -> return $ (Map.empty, MaxPQueue.empty)
                      
                  (head_sym, beta_msg) <- liftList $ Map.toList $ fst betaCell
                  (right_sym, alpha_msg) <- liftList $ Map.toList $ fst alphaCell
                  let r1 = ruleIdsWithRightChild gr right_sym
                      r2 = rulesHeadedById gr head_sym
                      rules = getRulesById gr $ IntSet.intersection r1 r2
                  rule <- liftList $ rules
                  guard $ right_sym /= leftChild rule
                  return $! (leftChild rule, weight rule * beta_msg * alpha_msg)

                rightCaseMsgs = runListT $ do
                  k <- liftList [1..i-1]
                  betaChart  <- lift get
                  betaCell <- case lookupLoc k j betaChart of
                    Just bc -> return bc
                    Nothing -> lift $ do c <- betas' k j
                                         return c
                  alphaCell <- case lookupLoc k (i-1) alphaChart of
                      Just rc -> return rc
                      Nothing -> return $ (Map.empty, MaxPQueue.empty)

                  head_sym <- fmap fst . liftList . MaxPQueue.take b . snd $ betaCell
                  let beta_msg = (fst betaCell) ! head_sym
                  left_sym <- fmap fst . liftList . MaxPQueue.take b . snd $ alphaCell
                  let alpha_msg = (fst alphaCell) ! left_sym

                  let r1 = ruleIdsWithLeftChild gr left_sym
                      r2 = rulesHeadedById gr head_sym
                      rules = getRulesById gr $ IntSet.intersection r1 r2
                  rule <- liftList $ rules
                  return $! (rightChild rule, weight rule * beta_msg * alpha_msg)

                process_msgs = do
                  ls <- leftCaseMsgs
                  rs <- rightCaseMsgs
                  let allMsgs = ls ++ rs
                  let m_sym = foldl' (\m (s, w) -> Map.insertWith (+) s w m) Map.empty
                              allMsgs
                      syms = symbolMapToSymbolQueue m_sym                              
                  modify $ \ch -> insertLoc i j (m_sym, syms) ch
                  return (m_sym, syms)

makeCharts :: Grammar
           -> Symbol
           -> Int
           -> Sentence
           -> (Chart Double', Chart Double')
makeCharts gr start b xs = (alphaChart, betaChart)
  where alphaChart = alphas gr b xs
        betaChart  = betas gr start b xs alphaChart
        m = length xs

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

musSymbol :: Monad m =>
             Int -> Int -> StateT ParseState m (Map Symbol Double')
musSymbol i j = do
  alphaChart <- gets alphaChartSt
  betaChart <- gets betaChartSt
  gr <- gets grammarSt
  let collectMus = do
      (a_m_sym, _) <- lookupLoc i j alphaChart
      (b_m_sym, _) <- lookupLoc i j betaChart
      let xs = do
          (sym, a) <- Map.toList a_m_sym
          let b = case Map.lookup sym b_m_sym of
                Nothing -> 0
                Just b -> b
          return $! (sym, a * b)
      Just $ foldl' (\m (r, c) -> Map.insertWith (+) r c m) Map.empty xs
  case collectMus of
      Just xs -> return xs
      Nothing -> return Map.empty
     

mus :: Monad m => Int -> Int -> Int -> StateT ParseState m (Map Rule Double')
mus i k j = do
    alphaChart <- gets alphaChartSt
    betaChart <- gets betaChartSt
    gr <- gets grammarSt
    let collectMus = do
         (left_m_sym, _) <- lookupLoc i k alphaChart
         (right_m_sym, _) <- lookupLoc (k+1) j alphaChart
         (head_m_sym, _) <- lookupLoc i j betaChart
         let xs = do
             (h, bh) <- Map.toList head_m_sym
             (r, ar) <- Map.toList right_m_sym
             (l, al) <- Map.toList left_m_sym
             rule <- getBinaryRulesBySymbols gr h l r 
             return $! (rule, bh * weight rule * ar * al)
         Just $ foldl' (\m (r, c) -> Map.insertWith (+) r c m) Map.empty xs
    case collectMus of
      Just xs -> return xs
      Nothing -> return Map.empty
    

loglikelihood :: State ParseState Double
loglikelihood = do
      xs <- gets sentenceSt
      alphaChart <- gets alphaChartSt
      start <- gets startSt
      let m = length xs
          (Just (Exp lnZ)) = lookup 1 m start alphaChart
      return $ lnZ

expectedCounts :: State ParseState (Map Rule Double')
expectedCounts = do m1 <- binary_case_mp
                    m2 <- unary_case_mp
                    return $ Map.unionWith (+) m1 m2
  where binary_case_mp = do
          xs <- gets sentenceSt
          start <- gets startSt
          alphaChart <- gets alphaChartSt
          let m = length xs
              (Just _Z) = lookup 1 m start alphaChart
          mu_maps <- runListT $ do 
            i <- liftList $ [1..m-1]
            k <- liftList $ [i..m-1]
            j <- liftList $ [k+1..m]
            lift $ mus i k j
          return $ Map.map (/_Z) . Map.unionsWith (+) $ mu_maps
        unary_case_mp = do
          xs <- gets sentenceSt
          start <- gets startSt
          alphaChart <- gets alphaChartSt
          gr <- gets grammarSt
          let m = length xs
              (Just _Z) = lookup 1 m start alphaChart
          vs <- runListT $ do
            (sym, i) <- liftList $ zip xs [1..m]
            mu_i <- lift $ musSymbol i i
            rule <- liftList $ rulesWithUnaryChild gr sym
            let c = case Map.lookup (headSymbol rule) mu_i of
                  Nothing -> 0
                  Just x -> x
            return $ (rule, c)
          let mp = foldl' (\m (r, c) -> Map.insertWith (+) r c m) Map.empty vs
          return $ Map.map (/_Z) mp
          

emIteration :: Grammar
            -> Symbol -- ^ start symbol
            -> Int -- ^ pruning limit
            -> Corpus
            -> (Grammar, -- ^ output grammar
                Double)  -- ^ loglikelihood of corpus in resulting grammar
-- | @emIteration gr start b corpus@ runs a single iteration of EM on
-- the corpus. Returns a resulting grammar and its associated
-- loglikelihood.
emIteration gr start b corpus = (gr', ll)
  where (css, lls) = unzip $ withStrategy (parList rdeepseq) $ do
           xs <- corpus
           return $ fst $ withCharts gr start b xs $ do
             ll <- loglikelihood
             cs <- expectedCounts
             trace (show xs) $ return ()                        
             return (cs, ll)
        ll = Prelude.sum lls
        counts = Map.unionsWith (+) css
        _K = fromIntegral $ Map.size counts
        -- FIXME: meanFieldDirMult does the wrong thing here. It's
        -- calculating the weights based on a flat multinomial instead
        -- of computing the weights independently within each rule!
        _Ws = meanFieldDirMultRules gr (1.0/_K) counts
        gr' = modifyRules gr (\r -> r{weight = maybe 0 id (Map.lookup r _Ws)})

instance DeepSeq.NFData (Map Rule Double') where
  rnf mp = map DeepSeq.rnf (Map.toList mp) `seq` ()
  
instance NFData (Map Rule Double') 

instance DeepSeq.NFData (Log Double) where
  rnf (Exp z) = DeepSeq.rnf z

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
          where (gr', ll) = emIteration g start b corpus 
                infoOut i = trace $ unlines
                            ["-------------",
                             "em: iteration " ++ show i ++ " loglike: " ++ show best_ll,
                             "gr : " ++ showSortedGrammar gr']
        negInfty = read "-Infinity"

---- SAMPLING -------

sample :: MonadRandom m => Grammar -> Symbol -> m Sentence
sample _ sym@T{} = return $! [sym]
sample gr sym@N{} = do
  i <- sampleCategorical weights
  let rule = matchingRules !! i
  case rule of
    BinaryRule h l r _ -> do xl <- sample gr l
                             xr <- sample gr r
                             return $! xl ++ xr
    UnaryRule h c _ -> sample gr c                                 
  where matchingRules = filter ((==sym) . headSymbol) (grammarRules gr)
        weights = map weight matchingRules

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

randomizeGrammar :: MonadRandom m => Grammar -> m Grammar 
randomizeGrammar gr = do
  let index = ruleIndex gr
  xs <- getRandomRs (0, 1)
  let index' = IntMap.fromList $ [(i, r{weight=x}) | (i, r) <- IntMap.toList index | x <- xs]
  return $ normalizeGrammar $ gr{ruleIndex=index'}

meanFieldDirMult :: Double' -- ^ concentration parameter
                 -> [Double'] -- ^ counts c_i
                 -> [Double'] -- ^ weights w_i
meanFieldDirMult alpha counts
-- Calculate the mean field weights of 
  = map ((/denom) . toDouble' . exp . digamma . fromDouble' . (+ alpha)) counts
  where _K = toDouble' $ fromIntegral $ length counts
        denom = toDouble' . exp . digamma . fromDouble' $ _K * alpha + sum counts

meanFieldDirMultRules :: Grammar -- ^ nonterminal symbols in grammar
                      -> Double' -- ^ alpha, concentration parameter
                      -> Map Rule Double' -- ^ counts for each rule
                      -> Map Rule Double' -- ^ mean field weights for each rule
meanFieldDirMultRules gr alpha m_counts = Map.fromList $ concatMap go (allNonTerminals gr)
  where go sym = zip rules ws
          where rules = rulesHeadedBy gr sym
                cs = [maybe 0 id (Map.lookup r m_counts) | r <- rules]
                ws = meanFieldDirMult alpha cs
                

fromDouble' :: Double' -> Double
fromDouble' (Exp lnX) = exp lnX

toDouble' :: Double -> Double'
toDouble' x = Exp (log x)

fromListAccum :: Ord a => [(a, b)] -> (b -> b -> b) -> Map a b -> Map a b
fromListAccum pairs f mp = foldl' (\m (r, c) -> Map.insertWith f r c m) mp pairs

---- UTILITIES ------

readGrammar :: FilePath -> IO Grammar
readGrammar fpath = do xs <- readFile fpath
                       case parseGrammar fpath xs of
                         Right gr -> return gr
                         Left err -> error $ show err
                         
parseGrammar :: String -- ^ source name
            -> String -- ^ grammar content
            -> Either ParseError Grammar
parseGrammar source xs = runParser grammarParser (Map.empty, 0) source xs

comment = do spaces
             char '#'
             _ <- manyTill anyToken eol
             return ()
             
emptyLine = spaces >> eol >> return ()

eol = char '\n'

grammarParser = do rs <- manyTill lineParser eof
                   return $! grammarFromRules rs

lineParser = skipMany (comment <|> emptyLine) >> ruleParser

ruleParser    = do spaces
                   h <- nonterminal
                   spaces
                   string "-->"
                   spaces
                   ss <- symbols
                   spaces
                   string "::"
                   spaces
                   d <- floating2 False
                   many (char ' ')
                   optional eol
                   case ss of
                     (s:[]) -> return $! UnaryRule h s (Exp (log d))
                     (s1:s2:[]) -> return $! BinaryRule h s1 s2 (Exp (log d))
                     _ -> unexpected $ "Too many symbols on left hnd side of rule."

nonterminal = do char '$'
                 xs <- many1 alphaNum
                 mp <- liftM fst getState                 
                 i <- liftM snd getState
                 case Map.lookup xs mp of
                   Just j -> return $ N j (Just xs)
                   Nothing -> do
                     modifyState (\(m, i) -> (Map.insert xs i m, i+1))
                     return $ N i (Just xs)

terminal = do xs <- many1 alphaNum
              return $ T xs

symbols = sepEndBy1 (try terminal <|> nonterminal) spaces


-- test
-- non terminals
_S = mkNonTerm 0 "S"
_NP = mkNonTerm 1 "NP"
_VP = mkNonTerm 2 "VP"
_PP = mkNonTerm 3 "PP"
_P = mkNonTerm 4 "P"
_V = mkNonTerm 5 "V"
-- terminals
with = T "with"
saw  = T "saw"
astronomers = T "astronomers"
ears = T "ears"
stars = T "stars"
telescopes = T "telescopes"

-- rules
r1 = _S ->- (_NP,  _VP, 1)
r2 = _PP ->- (_P, _NP, 1)
r3 = _VP ->- (_V, _NP, 0.7)
r4 = _VP ->- (_VP, _PP, 0.3)
r5 = _P ->>- (with,  1)
r6 = _V ->>- (saw,  1)
r7 = _NP ->- (_NP, _PP, 0.4)
r8 = _NP ->>- (astronomers,  0.1)
r9 = _NP ->>- (ears,  0.18)
r10 = _NP ->>- (saw,  0.04)
r11 = _NP ->>- (stars,  0.18)
r12 = _NP ->>- (telescopes, 0.1)

gr0 = grammarFromRules [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12]

-- k1 = _S ->>- (ears, 0.4)
-- k2 = _S ->>- (stars, 0.6)
-- xs = stringToCorpus $ "ears. stars. ears. ears."
-- gr1 = Grammar [k1, k2]
                 

-- gr = Grammar [(r1, 0)]
-- s = [T "A", T "B"]


          
isJust (Just _) = True
isJust _ = False

fromJust (Just x) = x
fromJust _ = error "FromJust."

liftList = ListT . return

instance (Floating a, Random a) => Random (Log a) where
  random g = let (r, g') = random g
             in (Exp (log r), g')
  randomR (Exp a, Exp b) g = let (r, g') = randomR ( exp a, exp b) g
                     in (Exp (log r), g')
