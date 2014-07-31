{-# Language BangPatterns, ParallelListComp, FlexibleInstances, DeriveGeneric #-}

module GIJoe.Grammar where

import GIJoe.Types
import GIJoe.Utils
import GIJoe.Chart

import Prelude hiding (sum, lookup)
import qualified Prelude

import Numeric.Log 

import Control.Monad
import Data.List.Split (splitOn)
import Data.List (foldl', sortBy, sort, maximumBy, foldl1')
import Data.Function (on)
--import Control.Monad.List
import GIJoe.ListT
import Control.Monad.State.Strict
import Control.Monad.Random
import Control.Monad.Identity
import Control.Parallel.Strategies 
import qualified Control.DeepSeq as DeepSeq (NFData, rnf)
import Numeric.SpecFunctions (digamma)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.PQueue.Prio.Max as MaxPQueue

import GHC.Generics (Generic)
import Data.Hashable

import Data.Tree
import Data.Tree.Pretty 

import Debug.Trace

import qualified Data.Sequence as Seq

import qualified Data.MemoCombinators as Memo

import Text.Parsec.Prim hiding (parse, State)
import Text.Parsec hiding (parse, State)
import Text.ParserCombinators.Parsec.Number

data Symbol = N {nonTermIndex :: Int, nonTermName :: (Maybe String)} --  non terminal
            | T String -- terminal
            | E -- empty symbol
            deriving (Ord, Eq, Generic)
                     
instance Show Symbol where
  show (N _ (Just s)) = s
  show (N i Nothing) = "N" ++ show i
  show (T s) = s

instance Hashable Symbol where 
-- instance Hashable Symbol where
--   hashWithSalt salt (N _ Nothing) = hashWithSalt salt Nothing
--   hashWithSalt salt (N i (Just s)) = hashWithSalt salt i
--   hashWithSalt salt (T s) = hashWithSalt salt s

  


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
          deriving (Ord, Eq, Generic)
                   
instance Show Rule where
  show (UnaryRule s l w) = show s ++ " --> " ++  show l ++ " :: " ++ show w
  show (BinaryRule s l r w) = show s ++ " --> " ++  show l ++ " " ++ show r   ++ " :: " ++ show w

instance Hashable Rule where

instance DeepSeq.NFData Rule where
  rnf (UnaryRule h c w) = h `seq` c `seq` w `seq` ()
  rnf (BinaryRule h l r w) = h `seq` l `seq` r `seq` w `seq` ()

-- instance NFData Rule where

-- instance NFData (HashMap Rule Double') where

isUnary :: Rule -> Bool
isUnary UnaryRule{} = True
isUnary _ = False

isBinary :: Rule -> Bool
isBinary = not . isUnary
                                                 
(->-) :: Symbol -> (Symbol, Symbol, Double') -> Rule
s ->- (l, r, w) = BinaryRule s l r w

(->>-) :: Symbol -> (Symbol, Double') -> Rule
s ->>- (l, w) = UnaryRule s l w

data Grammar = Grammar {headSymbolTable :: HashMap Symbol IntSet,
                        leftSymbolTable :: HashMap Symbol IntSet,
                        rightSymbolTable :: HashMap Symbol IntSet,
                        unarySymbolTable :: HashMap Symbol IntSet,
                        binaryRuleIds :: IntSet,
                        unaryRuleIds :: IntSet,
                        ruleIndex :: IntMap Rule 
                       } 

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


getUnaryRuleIdsBySymbols :: Grammar -> Symbol -> Symbol -> IntSet
getUnaryRuleIdsBySymbols gr h c = idsH `IntSet.intersection` idsC
  where idsH = rulesHeadedById gr h
        idsC = ruleIdsWithUnaryChild gr c

getUnaryRulesBySymbols :: Grammar -> Symbol -> Symbol -> [Rule]
getUnaryRulesBySymbols gr h c = getRulesById gr $
                                getUnaryRuleIdsBySymbols gr h c

rulesHeadedBy :: Grammar -> Symbol -> [Rule]
rulesHeadedBy gr sym = case HashMap.lookup sym (headSymbolTable gr) of
  Nothing -> []
  Just xs -> filterJusts $ map (getRuleById gr) $ IntSet.toList xs
  where filterJusts ((Just x):xs) = x:(filterJusts xs)
        filterJusts (Nothing:xs) = (filterJusts xs)
        filterJusts [] = []

rulesHeadedById :: Grammar -> Symbol -> IntSet
rulesHeadedById gr sym = case HashMap.lookup sym (headSymbolTable gr) of
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
  case HashMap.lookup sym (leftSymbolTable gr) of
    Nothing -> IntSet.empty
    Just ids -> ids

ruleIdsWithRightChild :: Grammar -> Symbol -> IntSet
ruleIdsWithRightChild gr sym = 
  case HashMap.lookup sym (rightSymbolTable gr) of
    Nothing -> IntSet.empty
    Just ids -> ids

ruleIdsWithUnaryChild :: Grammar -> Symbol -> IntSet
ruleIdsWithUnaryChild gr sym = 
  case HashMap.lookup sym (unarySymbolTable gr) of
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
                       HashMap.empty
                       HashMap.empty
                       HashMap.empty
                       HashMap.empty
                       IntSet.empty
                       IntSet.empty
                       IntMap.empty
        insertRule (Grammar hTable lTable rTable uTable bSet uSet index) (rule, i)
          = Grammar hTable' lTable' rTable' uTable' bSet' uSet' index'
          where hTable' = HashMap.insertWith (IntSet.union)
                         (headSymbol rule) (IntSet.singleton i) hTable
                index' = IntMap.insert i rule index
                (lTable', rTable', uTable', bSet', uSet') =
                  case rule of
                    BinaryRule{} -> (lTable', rTable', uTable, bSet', uSet)
                      where lTable' = HashMap.insertWith IntSet.union (leftChild rule)
                                      (IntSet.singleton i) lTable
                            rTable' = HashMap.insertWith IntSet.union (rightChild rule)
                                      (IntSet.singleton i) rTable
                            bSet' = IntSet.insert i bSet 
                    UnaryRule{} -> (lTable, rTable, uTable', bSet, uSet')
                      where uTable' = HashMap.insertWith IntSet.union (child rule)
                                      (IntSet.singleton i) uTable
                            uSet' = IntSet.insert i uSet 

allNonTerminals :: Grammar -> [Symbol]
allNonTerminals gr = HashMap.keys . headSymbolTable $  gr

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

showSortedGrammar gr = unlines $ map showRulePart $ take 20 $ reverse $ sortBy (compare `on` weight) $ grammarRules $ normalizeGrammar gr

showRulePart :: Rule -> String
showRulePart (BinaryRule h l r _) = show h ++ " -> " ++ show l ++ " " ++ show r
showRulePart (UnaryRule h c _) = show h ++ " -> " ++ show c

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

data Parse = Parse {parseRule :: Rule,
                    parseChildren :: [Parse]}
             
instance Show Parse where
  show = drawVerticalTree . parseToTree
   where parseToTree (Parse r xs) = Node (showRulePart r) (map parseToTree xs)

  

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

symbolMapToSymbolQueue m_sym = MaxPQueue.fromList . HashMap.toList $ m_sym

type Charts a = [Chart a]
newtype Chart a = Chart {unChart :: HashMap (Int, Int) (HashMap Symbol a, MaxPQueue Symbol a)}
                    deriving Show

lookup :: Int -> Int -> Symbol -> Chart a -> Maybe a
lookup i j sym (Chart m) = do (m_sym, _) <- HashMap.lookup (i, j) m
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
            Just (m_sym, syms) -> let m_sym' = HashMap.insert sym a m_sym
                                      syms' = if HashMap.member sym m_sym then syms else MaxPQueue.insert sym a syms
                                  in HashMap.insert (i, j) (m_sym', syms') m_ij
            Nothing ->
              HashMap.insert (i, j) (HashMap.singleton sym a, MaxPQueue.singleton sym a) m_ij


-- insertWith :: (a -> a -> a) -> Int -> Int -> Symbol -> a -> Chart a -> Chart a
-- insertWith f i j sym a m          
--   = insertWithKey (\_ _ _ sym' m' -> f sym' m') i j sym a m


-- insertWithKey :: (Int -> Int -> Symbol -> a -> a -> a)
--                  -> Int -> Int -> Symbol -> a -> Chart a -> Chart a
-- insertWithKey f i j sym a (Chart m_i) = Chart m_i'
--   where m_i' = IntMap.alter h i m_i
--           where h Nothing = Just $ m_j_default
--                 h (Just m_j) = Just $ IntMap.alter g j  m_j
--                   where g Nothing = Just $ (m_sym_default, MaxPQueue.singleton sym a)
--                         g (Just (m_sym, syms))
--                           = Just (HashMap.insertWithKey (f i j) sym a m_sym,
--                                   MaxPQueue.insert sym a syms)
--                 m_j_default = IntMap.singleton j (m_sym_default, MaxPQueue.singleton sym a)
--                 m_sym_default = HashMap.singleton sym a

lookupLoc :: Int -> Int -> Chart a -> Maybe (HashMap Symbol a, MaxPQueue Symbol a)
lookupLoc i j (Chart m_i) = HashMap.lookup (i, j) m_i

insertLoc :: Int -> Int -> (HashMap Symbol a, MaxPQueue Symbol a) -> Chart a -> Chart a
insertLoc i j (m_sym, syms) (Chart m_i) = Chart $ HashMap.insert (i, j) (m_sym, syms) m_i

empty :: Chart a
empty = Chart $ HashMap.empty

singleton :: Int -> Int -> Symbol -> a -> Chart a
singleton i j sym a =
  Chart $ HashMap.singleton (i, j) (HashMap.singleton sym a, MaxPQueue.singleton sym a)

showChart :: Show a => Chart a -> String
showChart (Chart mp) = unlines [show i ++ " " ++ show j ++ " " ++ show sym ++ ": " ++ show v
                                 | ((i, j), (m_sym, _)) <- HashMap.toList mp, (sym, v) <- HashMap.toList m_sym]


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
alphas gr b xs = flip evalState empty $ do alphas' 1 m
                                           get
  where xs' = Seq.fromList xs
        m = Seq.length xs'

        alphas' :: Int -> Int -> State (Chart Double') (HashMap Symbol Double', MaxPQueue Symbol Double')
        alphas' i j | i == j = process_msgs
          where msgs = toStateT $ do
                  rule <- liftList $ rulesWithUnaryChild gr x
                  let sym = headSymbol rule
                  return $! (sym, weight rule)
                  where x = Seq.index xs' (i-1)
                process_msgs = do
                  ks <- msgs -- :: [(Symbol, Double')]
                  let c = HashMap.fromList ks
                      syms = symbolMapToSymbolQueue c
                  modify $ \ch -> insertLoc i j (c, syms) ch
                  return $! (c, syms)

        alphas' i j = process_msgs
          where msgs = toStateT $ do
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
                  let !a = weight rule
                          * left_alpha
                          * right_alpha
                  return $! (headSymbol rule, a)

                process_msgs = do
                  xs <- msgs
                  let m_sym = foldl' (\m (s, w) -> HashMap.insertWith (+) s w m) HashMap.empty xs
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
betas gr start b xs !alphaChart = evalState (go >> get) empty
  where go = sequence $ do
          i <- [1..m]
          j <- [i..m]
          return $ betas' i j
        xs' = Seq.fromList xs
        m = Seq.length xs'
--        liftList = ListT . return
        
        betas' :: Int -> Int
                  -> State (Chart Double')
                  (HashMap Symbol Double', MaxPQueue Symbol Double')
        betas' i j | i == 1 && j == m = do
          modify $ \ch -> insert 1 m start 1  ch
          return $ (HashMap.singleton start 1,
                    MaxPQueue.singleton start 1)
        
        betas' i j = process_msgs
          where leftCaseMsgs = toStateT $ do
                  k <- liftList [j+1..m]
                  betaChart  <- lift get
                  betaCell <- case lookupLoc i k betaChart of
                    Just bc -> return bc
                    Nothing -> lift $ do c <- betas' i k
                                         return c
                  alphaCell <- case lookupLoc (j+1) k alphaChart of
                      Just rc -> return rc
                      Nothing -> return $ (HashMap.empty, MaxPQueue.empty)

                  head_sym <- fmap fst . liftList . MaxPQueue.take b . snd $ betaCell
                  let beta_msg = (fst betaCell) ! head_sym
                  right_sym <- fmap fst . liftList . MaxPQueue.take b . snd $ alphaCell
                  let alpha_msg = (fst alphaCell) ! right_sym

                  let r1 = ruleIdsWithRightChild gr right_sym
                      r2 = rulesHeadedById gr head_sym
                      rules = getRulesById gr $ IntSet.intersection r1 r2
                  rule <- liftList $ rules
                  guard $ right_sym /= leftChild rule
                  let !l = weight rule *
                             beta_msg *
                             alpha_msg
                  return $! (leftChild rule, l)
                             
                rightCaseMsgs = toStateT $ do
                  k <- liftList [1..i-1]
                  betaChart  <- lift get
                  betaCell <- case lookupLoc k j betaChart of
                    Just bc -> return bc
                    Nothing -> lift $ do c <- betas' k j
                                         return c
                  alphaCell <- case lookupLoc k (i-1) alphaChart of
                      Just rc -> return rc
                      Nothing -> return $ (HashMap.empty, MaxPQueue.empty)

                  head_sym <- fmap fst . liftList . MaxPQueue.take b . snd $ betaCell
                  let beta_msg = (fst betaCell) ! head_sym
                  left_sym <- fmap fst . liftList . MaxPQueue.take b . snd $ alphaCell
                  let alpha_msg = (fst alphaCell) ! left_sym

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
                  let allMsgs = ls ++ rs
                  let m_sym = foldl' (\m (s, w) -> HashMap.insertWith (+) s w m) HashMap.empty allMsgs
                      syms = symbolMapToSymbolQueue m_sym                              
                  modify $ \ch -> insertLoc i j (m_sym, syms) ch
                  betaChart <- get
                  return (m_sym, syms)


getMapParse :: Monad m => StateT ParseState m Parse
getMapParse = do
  xs <- gets sentenceSt
  let m = length xs
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
              (rule, p, k) = maximumBy (compare `on` snd3) xs
          lParse <- mapParse gr i k (leftChild rule) 
          rParse <- mapParse gr (k+1) j (rightChild rule)
          return $! Parse rule [lParse, rParse]
          
        mapParse gr i j sym | i == j = do
          xs <- gets sentenceSt
          let rule = head $ getUnaryRulesBySymbols gr sym (xs !! (i - 1))
          return $ Parse rule []
            
          
makeCharts :: Grammar
           -> Symbol
           -> Int
           -> Sentence
           -> (Chart Double', Chart Double')
makeCharts gr start b xs = (alphaChart, betaChart)
  where !alphaChart = alphas gr b xs
        !betaChart  = betas gr start b xs alphaChart
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

meritSymbolSpan :: Monad m =>
             Int -> Int -> StateT ParseState m (HashMap Symbol Double')
meritSymbolSpan i j = do
  alphaChart <- gets alphaChartSt
  betaChart <- gets betaChartSt
  gr <- gets grammarSt
  lnZ <- loglikelihood  
  let collectMus = do
       (a_m_sym, _) <- lookupLoc i j alphaChart
       (b_m_sym, _) <- lookupLoc i j betaChart
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
         (left_m_sym, _) <- lookupLoc i k alphaChart
         (right_m_sym, _) <- lookupLoc (k+1) j alphaChart
         (head_m_sym, _) <- lookupLoc i j betaChart
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
                  r <- getUnaryRulesBySymbols gr sym (xs !! (i - 1))]
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
    Just (m_sym, _) -> return $! m_sym

beta :: Monad m => Int -> Int -> StateT ParseState m (HashMap Symbol Double')
beta i j = do
  betaChart <- gets betaChartSt
  case lookupLoc i j betaChart of
    Nothing -> return HashMap.empty
    Just (m_sym, _) -> return $! m_sym

probRuleIJ :: Monad m => Int -> Int -> StateT ParseState m (HashMap Rule Double')
probRuleIJ i j = do
  lnZ <- loglikelihood
  mp <- meritRuleIJ i j
  let z = Exp lnZ
  return $ HashMap.map (/z) mp

debugProbs :: Monad m => StateT ParseState m [(Int, Int, Rule, Double')]
debugProbs = do
  xs <- gets sentenceSt
  let m = length xs
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
      let m = length xs
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
          let m = length xs
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
          let m = length xs
              (Just _Z) = lookup 1 m start alphaChart
          vs <- toStateT $ do
            (sym, i) <- liftList $ zip xs [1..m]
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
           return $ fst $ withCharts gr start b xs $ do
             ll <- loglikelihood
             cs <- expectedCounts
             h <- entropyParses
             return (cs, ll, h)
        !ll = Prelude.sum lls
        !h = Prelude.sum hs
        !counts = foldl1' (HashMap.unionWith (+)) css
        !_K = fromIntegral $ length (allNonTerminals gr)
        !_Ws = meanFieldDirMultRules gr (2.0/_K) counts
        
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
  let m = length xs
--      liftList = ListT . return 
  vs <- toStateT $ do
    i <- liftList $ [1..m] 
    j <- liftList $ [i..m]
    mp <- lift $ probRuleIJ i j
    (Exp lnp) <- liftList $ HashMap.elems mp
    let p = exp lnp
    return $ negate $ if p == 0 then 0 else p * lnp
  return $! Prelude.sum vs

underFlow :: Double' -> Double'
underFlow x | x < _MIN_VAL = _MIN_VAL
            | otherwise = x
  where _MIN_VAL = 1e-2

sumAcc :: [Double'] -> Double'
sumAcc xs = out
  where !bs = map ln xs -- get exponents
        !bMin = minimum bs -- minimum exponent
        !bs' = map (subtract bMin) bs -- subtract out minimum exponent
        !ys = sum $ map Exp bs'
        !out = ys * Exp bMin
        
---- UTILITIES ------

readGrammar :: FilePath -> IO Grammar
readGrammar fpath = do xs <- readFile fpath
                       case parseGrammar fpath xs of
                         Right gr -> return gr
                         Left err -> error $ show err

writeGrammar fpath info gr = writeFile fpath (unlines (comment:ls))
  where ls = map go (grammarRules gr)
        go (BinaryRule h l r w) =
          "$" ++ show h ++ " --> " ++ "$" ++ show l ++ " " ++ "$" ++ show r ++ " :: " ++ show w
        go (UnaryRule h c w) =
          "$" ++ show h ++ " --> " ++ show c ++ " :: " ++ show w
        comment = '#':info

                        
                         
parseGrammar :: String -- ^ source name
            -> String -- ^ grammar content
            -> Either ParseError Grammar
parseGrammar source xs = runParser grammarParser (HashMap.empty, 0) source xs

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
                 case HashMap.lookup xs mp of
                   Just j -> return $ N j (Just xs)
                   Nothing -> do
                     modifyState (\(m, i) -> (HashMap.insert xs i m, i+1))
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
r3 = _VP ->- (_V, _NP, 0.3)
r4 = _VP ->- (_VP, _PP, 0.7)
r5 = _P ->>- (with,  1)
r6 = _V ->>- (saw,  1)
r7 = _NP ->- (_NP, _PP, 0.4)
r8 = _NP ->>- (astronomers,  0.1)
r9 = _NP ->>- (ears,  0.18)
r10 = _NP ->>- (saw,  0.04)
r11 = _NP ->>- (stars,  0.18)
r12 = _NP ->>- (telescopes, 0.1)

gr0 = grammarFromRules [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12]

_A = mkNonTerm 0 "A"
_B = mkNonTerm 1 "B"
t1 = _A ->- (_A, _B, 1000000)
t2 = _A ->- (_A, _A, 1e-9)
t3 = _A ->- (_B, _A, 1)
t4 = _B ->- (_A, _B, 1)
t5 = _B ->- (_A, _A, 1)
t6 = _B ->- (_B, _A, 1)
t7 = _A ->>- (T "a", 50000)
t8 = _B ->>- (T "a", 5e-9)
t9 = _A ->>- (T "b", 1e-10)
t10 = _B ->>- (T "b", 2e-50)

gr1 = grammarFromRules [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10]

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

-- liftList = ListT . return

instance (Floating a, Random a) => Random (Log a) where
  random g = let (r, g') = random g
             in (Exp (log r), g')
  randomR (Exp a, Exp b) g = let (r, g') = randomR ( exp a, exp b) g
                     in (Exp (log r), g')
