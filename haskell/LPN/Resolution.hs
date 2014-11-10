{-# Language FlexibleContexts #-}

module LPN.Resolution where

import Control.Monad.Error.Class

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

type Map = HashMap

-- | Implementation of resolution algorithm for function-free
-- Horn clause logic with string concatenation. 

data Var = Var String deriving (Show, Eq, Ord)
type Sym = String
data Element = ElVar Var | ElSym Sym deriving (Show, Eq)
type ElementStr = [Element]
data Predicate = Predicate {predName :: String,
                            predArity :: Int} deriving (Show, Eq)
                                                       
data Term = Term {termPred :: Predicate,
                  termArgs :: [Elements]} deriving (Show, Eq)

type Subst = Map Var Term

nullSubst :: Subst
nullSubst = Map.empty

data UnificationError = ConflictError Term Term
                        | OccursCheckError Var Term deriving Eq

instance Show UnificationError where 
    show (ConflictError t1 t2) = "UnificationError.ConflictError: attempting to unify_ " ++ show t1 ++ " and " ++ show t2 ++ "."
    show (OccursCheckError var term) = "UnificationError.OccursCheckError:: variable " ++ show var ++ " cannot unify with term " ++ show term ++ "."
                                                             
mgu :: MonadError UnificationError m => Term -> Term -> m Subst
-- | Most general unifier of two terms
mgu (Term _P xs) (Term _Q ys) | _P /= _Q = throwError $ ConflictError _P _Q
                              | otherwise = foldM_ mgu_
       



