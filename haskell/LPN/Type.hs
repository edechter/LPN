{-# Language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}


module LPN.Type where

-- import GIJoe.ApproximateInference.HashTable

import Prelude hiding (head)

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

import Data.List hiding (head)
import qualified Data.List as List
import Data.Function (on)

import System.Random

data Predicate = Predicate {predName :: String, predArity :: Int} deriving (Eq, Ord)
instance Show Predicate where
  show (Predicate n a) = n ++ "_" ++ show a

type Var = String
type Symbol = String

data Argument = Argument {elements :: [Element]} deriving Eq
instance Show Argument where
  show (Argument es) = intercalate " " (map show es)

data Element = ElVar {elToVar :: Var} | ElSym {elToSym :: Symbol} deriving (Eq)
instance Show Element where
  show (ElVar v) = v
  show (ElSym s) = s

data Term = Term {termPredicate :: Predicate, termArgs :: [Argument]} deriving (Eq)

instance Show Term where
  show (Term p as) = predName p ++ "(" ++ intercalate ", " (map show as) ++ ")"

data Rule = Term :<-: [Term] deriving Eq
data WRule = Term :<=: ([Term], Double) deriving Eq

class IsRule a where
  head :: a -> Term
  body :: a -> [Term]
  headPredicate :: a -> Predicate
  headPredicate = termPredicate . head
  weight :: a -> Double
  

instance IsRule Rule where
  head (h :<-: _) = h
  body (_ :<-: bs) = bs
  weight _ = 1

instance IsRule WRule where
  head (h :<=: _) = h
  body (_ :<=: (bs,_)) = bs
  weight (_ :<=: (_, w)) = w
                        
ruleWeight (_ :<=: (_, w)) = w

dropRuleWeight (h :<=: (bs, w)) = h :<-: bs

instance Show Rule where
  show (h :<-: []) = show h
  show (h :<-: bs) = show h ++ " <-- " ++ intercalate ", " (map show bs)

instance Show WRule where
  show wr = show (dropRuleWeight wr) ++ " :: " ++ show (ruleWeight wr)


data RewriteSystem = RewriteSystem {rsRules :: [WRule]} 
instance Show RewriteSystem where
  show (RewriteSystem rules) = unlines $ map ((++".") . show) rules


----- Utility Functions --------
ruleWithHeadPredicate :: RewriteSystem -> Predicate -> [WRule]
ruleWithHeadPredicate (RewriteSystem rs) p = filter (\r -> headPredicate r == p) rs

groupRulesByHeadPredicate (RewriteSystem rs) =
  Map.fromList [(f, rules) | rules <- groups, f <- [headPredicate . List.head $ rules]]
  where sameHead r1 r2 = headPredicate r1 == headPredicate r2
        groups = groupBy sameHead $ sortBy (compare `on` headPredicate) rs


