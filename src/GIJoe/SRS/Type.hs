{-# Language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

module GIJoe.SRS.Type where

import GIJoe.ApproximateInference.HashTable

import Data.Text (Text)
import qualified Data.Text as Text

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (intercalate)

import System.Random

import Numeric.Log

type Double' = Log Double

data Predicate = Predicate {predName :: Text, predArity :: Int} deriving (Show, Ord, Eq)

type Var = Text 

type Symbol = Text

data Element = ElVar {elToVar :: Var} | ElSym {elToSym :: Symbol} deriving (Ord, Eq)
instance Show Element where
  show (ElVar v) = Text.unpack v
  show (ElSym s) = Text.unpack s

newtype NonTerminalString = NTString {unNT :: [Element]} deriving (Ord, Eq)
instance Show NonTerminalString where
  show (NTString xs) = intercalate " " (map show xs)

type TerminalString = [Symbol]

data ComplexTerm = ComplexTerm {cTermPredicate :: Predicate, ctermArgs :: [NonTerminalString]} deriving (Ord, Eq)

instance Show ComplexTerm where
  show (ComplexTerm p as) = Text.unpack (predName p) ++ "(" ++ intercalate ", " (map show as) ++ ")"
  
data SimpleTerm  = SimpleTerm {sTermPredicate :: Predicate, stermArgs :: [Var]} deriving (Ord, Eq)
instance Show SimpleTerm where
  show (SimpleTerm p as) = Text.unpack (predName p) ++ "(" ++ intercalate ", " (map Text.unpack as) ++ ")"

data Term = CTerm ComplexTerm | STerm SimpleTerm deriving (Show, Ord, Eq)

data Rule = ComplexTerm :<-: [SimpleTerm] deriving (Ord, Eq)
instance Show Rule where
  show (h :<-: []) = show h ++ "."
  show (h :<-: bs) = show h ++ " <-- " ++ intercalate ", " (map show bs)

data WeightedRule = WRule {weightedRuleRule :: Rule, weightedRuleWeight :: Double'}
instance Show WeightedRule where
  show (WRule r w) = show r ++ " :: " ++ show w

data RewriteSystem = RewriteSystem {rsRules :: [WeightedRule]} 
instance Show RewriteSystem where
  show (RewriteSystem rules) = unlines $ map show rules

data CompiledRewriteSystem = CRewriteSystem {ruleIdTable :: IntMap Rule,
                                             headSymbolToRuleIdTable :: HashMap Symbol IntSet,
                                             argSymbolToRuleIdTable :: HashMap Symbol IntSet}
                             
type TermTable s a = HashTable s Term a

ruleHead :: Rule -> ComplexTerm
ruleHead (t :<-: _) = t

ruleRhs :: Rule -> [SimpleTerm]
ruleRhs (_ :<-: rhs) = rhs

systemFromRules :: [WeightedRule] -> RewriteSystem
systemFromRules = RewriteSystem

cTermVars (ComplexTerm _ args) = concatMap (argVars . unNT) args
  where argVars ((ElVar v):as) = v : argVars as
        argVars (_:as) = argVars as
        argVars [] = []

substCTermVars :: ComplexTerm -> Map Var NonTerminalString -> ComplexTerm
substCTermVars (ComplexTerm pred args) subst = ComplexTerm pred args'
  where args' = map (NTString . concatMap go . unNT) args
        go (ElVar v) = case Map.lookup v subst of
          Nothing -> [ElVar v]
          Just (NTString s) -> s
        go x = [x]


instance (Floating a, Random a) => Random (Log a) where
  random g = let (r, g') = random g
             in (Exp (log r), g')
  randomR (Exp a, Exp b) g = let (r, g') = randomR ( exp a, exp b) g
                     in (Exp (log r), g')
