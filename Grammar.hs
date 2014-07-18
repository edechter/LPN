{-# Language BangPatterns, ParallelListComp #-}

module Grammar where

import Prelude hiding (sum)
import Numeric.Log 

import Control.Monad
import Data.List.Split (splitOn)
import Data.List (foldl', isPrefixOf)
import Control.Monad.State
import Control.Applicative ((<*>))
import Control.Monad.Random

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Debug.Trace

import qualified Data.Sequence as Seq

import qualified Data.MemoCombinators as Memo

import Text.Parsec.Prim hiding (parse)
import Text.Parsec hiding (parse)
import qualified Text.Parsec as P 
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

rulesWithLeftChild :: Grammar -> Symbol -> [Rule]
rulesWithLeftChild gr sym = getRulesById gr $ ruleIdsWithLeftChild gr sym

rulesWithRightChild :: Grammar -> Symbol -> [Rule]
rulesWithRightChild gr sym = getRulesById gr $ ruleIdsWithRightChild gr sym


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



-- | Inside probabilities: alpha(A, i, j)
alphas :: Grammar
       -> Sentence
       -> Symbol -> Int -> Int -> Double'
alphas gr xs = memo_alphas -- note: the lack of explicit arguments
                                -- means that repeated calls to `alphas gr
                                -- xs` is a memo-table even outside of the function scope
  where memo_alphas = memo alphas'
        memo = Memo.memo3 memoSymbol Memo.integral Memo.integral
        xs' = Seq.fromList xs

        -- base case
        alphas' sym i j | i == j = sum $ do
          rule <- unaryRulesHeadedBy gr sym
          guard $ child rule == x
          return $! weight rule
          where x = Seq.index xs' (i-1) 

        -- inductive case
        alphas' sym i j = sum $ do
          k <- [i..j-1]
          rule <- binaryRulesHeadedBy gr sym
          let left_sym = leftChild rule
              right_sym = rightChild rule
              left_alpha = memo_alphas left_sym i k              
              right_alpha = memo_alphas right_sym (k+1) j
          case True of
            _ | right_alpha == 0 -> return 0
            _ | left_alpha  == 0 -> return 0
            _ -> return $! weight rule * right_alpha * left_alpha
          

-- -- | Outside probabilities
betas :: Grammar -> Symbol -> Sentence -> Symbol -> Int -> Int -> Double'
betas gr start xs = betas_table
  where betas_table = memo betas'
        alphas_table = alphas gr xs
        memo = Memo.memo3 memoSymbol Memo.integral Memo.integral
        xs' = Seq.fromList xs
        m = Seq.length xs'
        rs = grammarRules gr

        -- base case
        betas' sym i j | i == 1 && j == m  = if sym == start then 1 else 0

        -- inductive case
        betas' sym i j = left_case + right_case
          where
            left_case = sum $ do -- B -> sym C
              rule <- rulesWithLeftChild gr sym
              let right_sym = rightChild rule
                  head_sym  = headSymbol rule
              guard $ right_sym /= sym
              k <- [j+1..m]
              let alpha = alphas_table right_sym (j+1) k
                  beta  = betas_table head_sym i k
              return $ weight rule * alpha * beta 

            right_case = sum $ do -- B -> C sym
              rule <- rulesWithRightChild gr sym
              let left_sym = leftChild rule
                  head_sym  = headSymbol rule
              k <- [1..i-1]
              let alpha = alphas_table left_sym k (i-1)
                  beta  = betas_table head_sym k j
              return $ weight rule * alpha * beta 


type Chart a = (Symbol -> Int -> Int -> a)
type Charts a = [Chart a]

musSymbol :: Chart Double' -- ^ alpha table
          -> Chart Double' -- ^ beta table
          -> Symbol -> Int -> Int -> Double'
musSymbol alphaTable betaTable sym i j =
  alphaTable sym i j * betaTable sym i j

mus :: (Symbol -> Int -> Int -> Double') -- ^ alpha table
    -> (Symbol -> Int -> Int -> Double') -- ^ beta table
    -> Rule -> Int -> Int -> Int -> Double'
mus alphaTable betaTable= go
  where go rule i k j = betaTable _A i j
                        * w * alphaTable _B i k
                        * alphaTable _C (k+1) j
          where _A = headSymbol rule
                _B = leftChild rule
                _C = rightChild rule
                w = weight rule

expectedCounts :: Charts Double' -- ^ alpha tables
               -> Charts Double' -- ^ beta tables
               -> Symbol
               -> Corpus 
               -> Rule
               -> Double'
expectedCounts alphaTables betaTables start corpus = go -- note: eta-reduced to ensure memoization
 where go rule = sum $ [g i rule | i <- [0..n-1]]
       corpus' = Seq.fromList corpus
       n = Seq.length corpus'
       g i = expectedCountsSingle (alphaTables !! i) (betaTables !! i) start (corpus !! i)

expectedCountsSingle :: Chart Double' -- ^ alpha table
                     -> Chart Double' -- ^ beta table
                     -> Symbol -- ^ start symbol
                     -> Sentence
                     -> Rule
                     -> Double'
expectedCountsSingle alphaTable betaTable start xs = go 
  where go rule =
          case rule of
            BinaryRule{} ->
              sum ([mus alphaTable betaTable rule i k j
                   | i <- [1..m-1], k <- [i..m-1], j <- [k+1..m]]) / _Z
            UnaryRule{} ->
              sum ([musSymbol alphaTable betaTable (headSymbol rule) i i
                   | i <- [1..m], Seq.index xs' (i-1) == (child rule)]) / _Z
        _Z = alphaTable start 1 m
        m = length xs
        xs' = Seq.fromList xs


expectationSingle :: Chart Double' -- ^ alphaTablen
                  -> Chart Double' -- ^ betaTable
                  -> Grammar
                  -> Symbol
                  -> Sentence
                  -> (Rule -> Double')  -> Double'
expectationSingle alphaTable betaTable gr start xs = go -- ^ note: this is eta-reduced to ensure memoization
  where go func = numerator / _Z
          where calc_binary rule@BinaryRule{} i j k
                  = mus alphaTable betaTable rule i k j * func rule
                calc_binary _ _ _ _ = error $ "calc_binary: tried to call calc_binary on UnaryRule"
                calc_unary rule@UnaryRule{} i
                  = musSymbol alphaTable betaTable (headSymbol rule) i i * func rule
                calc_unary _ _ = error $ "calc_unary: tried to call calc_binary on BinaryRule."

                numerator = sum $ 
                   do rule <- grammarRules gr
                      if isBinary rule
                         then do i <- [1..m-1]
                                 k <- [i..m-1]
                                 j <- [k+1..m]
                                 return $! calc_binary rule i j k
                         else do i <- [1..m]
                                 guard $ Seq.index xs' (i-1) == child rule
                                 return $! calc_unary rule i
                           
        _Z = alphaTable start 1 m
        m = length xs
        xs' = Seq.fromList xs

emIteration :: Grammar
            -> Symbol -- ^ start symbol
            -> Charts (Maybe ParseSet)
            -> Corpus
            -> (Grammar, -- ^ output grammar
                Double)  -- ^ loglikelihood of corpus in resulting grammar
-- | @emIteration gr start corpus@ runs a single iteration of EM on
-- the corpus. Returns a resulting grammar and its associated
-- loglikelihood.
emIteration gr start _ corpus = (gr', ll)
  where gr' = normalizeGrammar $ modifyRules gr (\r -> r{weight=go r})
        alphaTables = [alphas gr (corpus !! i)   | i <- [0..length corpus - 1]]
        betaTables  = [betas gr start (corpus !! i) | i <- [0..length corpus - 1]]
        ll = loglikelihood alphaTables start corpus
        go = expectedCounts alphaTables betaTables start corpus -- note: eta reduced to ensure memoization

type EMLog = [(Grammar, Double)]

em :: Grammar
   -> Symbol
   -> Corpus
   -> Int -- ^ max number of iterations
   -> Double -- ^ tolerance e.g. 1e-6, or set to 0 to use all iterations
   -> EMLog
em gr start corpus maxIter tol = go gr negInfty maxIter 
  where go _ _ 0 = []
        go g best_ll i = infoOut i $
                         if ll < best_ll
                              then error $ unlines
                                   $ ["em: the impossible happened. ",
                                     "em returned smaller loglikelihood on iteration: " ++ show (maxIter-i),
                                     "loglikelihood: " ++ show ll,
                                     "previous loglikelihood: " ++ show best_ll ++ "\n",
                                     "grammar: " ++ show g]
                              else if (ll - best_ll) < tol
                                      then [(g, ll)]
                                      else (g, ll) : go gr' ll (i - 1) 
                                                          
          where (gr', ll) = emIteration g start parseCharts corpus
                infoOut i = trace $ unlines ["-------------",
                                             "em: iteration " ++ show i ++ " loglike: " ++ show best_ll]
                
        negInfty = read "-Infinity"
        parseCharts = map (parse gr) corpus 

loglikelihood :: Charts Double' -- ^ alpha tables
              -> Symbol 
              -> Corpus
              -> Double
loglikelihood alphaTables start corpus = v
  where (Exp v) = product [(alphaTables !! i) start 1 (length x)| (x, i) <- zip corpus [0..]]

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
             
empty = spaces >> eol >> return ()

eol = char '\n'

grammarParser = do rs <- manyTill lineParser eof
                   return $! grammarFromRules rs

lineParser = skipMany (comment <|> empty) >> ruleParser

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
                     (s:[]) -> return $! UnaryRule h s d
                     (s1:s2:[]) -> return $! BinaryRule h s1 s2 d
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
r3 = _VP ->- (_V, _NP, 0.6)
r4 = _VP ->- (_VP, _PP, 0.4)
r5 = _P ->>- (with,  1)
r6 = _V ->>- (saw,  1)
r7 = _NP ->- (_NP, _PP, 0.2)
r8 = _NP ->>- (astronomers,  0.1)
r9 = _NP ->>- (ears,  0.18)
r10 = _NP ->>- (saw,  0.24)
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
                                                 

instance (Floating a, Random a) => Random (Log a) where
  random g = let (r, g') = random g
             in (Exp (log r), g')
  randomR (Exp a, Exp b) g = let (r, g') = randomR ( exp a, exp b) g
                     in (Exp (log r), g')
