{-# LANGUAGE TemplateHaskell #-}

module GIJoe.MCFG where

import Control.Lens
import Data.List (intercalate)
import Numeric.Log

type Double' = Log Double

data Var = Var {_varInt :: Int} deriving (Eq, Ord)
makeLenses ''Var

instance Show Var where
  show (Var i) = "x" ++ show i
  
type Symbol = String
data Atom = Atom {_atomPred :: Symbol,
                  _atomVars :: [Var]} deriving (Eq, Ord)
makeLenses ''Atom                                               
instance Show Atom where
  show (Atom s vs) = s ++ "(" ++ intercalate " " (map show vs) ++ ")"
  
data Rule = Rule {_lhs :: Atom,
                  _rhs :: [Atom]} deriving (Eq, Ord)
makeLenses ''Rule                                           
instance Show Rule where
  show (Rule l rs) = show l ++ " <- " ++ intercalate " " (map show rs)

data WeightedRule = WRule {_wRule :: Rule,
                           _wRuleWeight :: Double'} deriving Show
makeLenses ''WeightedRule

data MCFG = MCFG {_start :: Symbol,
                  _rules :: [WeightedRule]} deriving Show
makeLenses ''MCFG

 

                                                     

