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
import Data.List (foldl', sortBy, sort, maximumBy, foldl1', nub)
import Control.Monad.Logic
import Data.Function (on)

import Control.DeepSeq

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

import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.PQueue.Prio.Max as MaxPQueue

import GHC.Generics (Generic)
import Data.Hashable

import Data.Tree
import Data.Tree.Pretty 

import Debug.Trace

import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

import qualified Data.MemoCombinators as Memo

data Symbol = N {nonTermIndex :: Int, nonTermName :: (Maybe String)} --  non terminal
            | T String -- terminal
            deriving (Ord, Eq, Generic)
                     
instance Show Symbol where
  show (N _ (Just s)) = s
  show (N i Nothing) = "N" ++ show i
  show (T s) = s

instance NFData Symbol where
  rnf (N i t) = i `seq` t `seq` N i t `seq` ()
  rnf (T s) = s `seq` T s `seq` ()


instance Hashable Symbol where

memoSymbol :: Memo.Memo Symbol
memoSymbol f = table (m (f . uncurry N)) (m' (f . T)) f  
  where m = Memo.pair Memo.integral (Memo.maybe (Memo.list Memo.char))
        m' = Memo.list Memo.char
        table n t e (N i s) = n (i, s)
        table n t e (T s)   = t s

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

unaryRules :: Grammar -> [Rule]
unaryRules gr = getRulesById gr $ unaryRuleIds gr

binaryRules :: Grammar -> [Rule]
binaryRules gr = getRulesById gr $ binaryRuleIds gr

getRuleById :: Grammar -> Int -> Maybe Rule
getRuleById gr i = IntMap.lookup i (ruleIndex gr)

getRulesById :: Grammar -> IntSet -> [Rule]
getRulesById gr ids = go [] $ IntSet.toList ids
  where go acc [] = acc
        go acc (x:xs) = case getRuleById gr x of
          Just y -> go (y:acc) xs
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

nonterminals :: Grammar -> [Symbol]
nonterminals = allNonTerminals

numSymbols :: Grammar -> Int
numSymbols gr = HashMap.size . headSymbolTable $  gr

allSymbols :: Grammar -> [Symbol]
allSymbols gr = allNonTerminals gr ++ lexicon gr

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

type Lexicon = [Symbol]

lexicon :: Grammar -> Lexicon
lexicon gr = nub $ map child unaryRules
  where unaryRules = getRulesById gr (unaryRuleIds gr)

terminals = lexicon

initGrammar :: Symbol
            -> Lexicon
            -> Int     -- ^ K
            -> Grammar
initGrammar start lexicon _K = grammarFromRules $ binary_rules ++ unary_rules
  where binary_rules = do
          i <- [0.._K-1]
          j <- [0.._K-1]
          k <- [0.._K-1]
          return $! BinaryRule (N i Nothing) (N j Nothing) (N k Nothing) 1.0
        unary_rules = [UnaryRule (N i Nothing) l 1.0  | i <- [0.._K-1], l <- lexicon]

type Sentence = Seq Symbol
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
recognize gr sym xs = case parse gr xs sym 1 (Seq.length xs) of
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
        m = Seq.length xs

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
                x = Seq.index xs (i - 1)
                
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
stringToSentence = Seq.fromList . map T . words

stringToCorpus :: String -> Corpus
stringToCorpus = map stringToSentence . splitOn "."

---- SAMPLING -------

sample :: MonadRandom m => Grammar -> Symbol -> m Sentence
sample _ sym@T{} = return $! Seq.singleton sym
sample gr sym@N{} = do
  i <- sampleCategorical weights
  let rule = matchingRules !! i
  case rule of
    BinaryRule h l r _ -> do xl <- sample gr l
                             xr <- sample gr r
                             return $! xl >< xr
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

----- LogicT -----
liftList :: Monad m => [a] -> LogicT m a
liftList [] = mzero
liftList xs = foldl1' mplus (map return xs)
        

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
