{-# Language BangPatterns, LambdaCase, RankNTypes #-}

module GIJoe.ApproximateInference.BoundaryProbabilities
       (mkOmegaTable,
        mkGammaThetaTables,
        mkLambdaTable, 
        OmegaTable,
        GammaTable,
        ThetaTable,
        LambdaTable
       ) where

import Prelude hiding (sum)
import GIJoe.Grammar
import GIJoe.Types

import GIJoe.ApproximateInference.HashTable (HashTable)
import qualified GIJoe.ApproximateInference.HashTable as HashTable

import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Monad.ST

import Numeric.LinearAlgebra.HMatrix
import Data.Packed.Matrix
import Numeric.Log

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Control.Monad.Loop

import Data.List hiding (sum)

import Debug.Trace

type OmegaTable s = HashTable s (Symbol, Symbol, Symbol) Double'
type GammaTable s = HashTable s (Symbol, Symbol) Double'
type ThetaTable s = HashTable s (Symbol, Symbol) Double'


mkOmegaTable :: Grammar -> ST s (HashTable s (Symbol, Symbol, Symbol) Double')
mkOmegaTable gr = trace ("Making OmegaTable") $ do
  (gamma_ht, theta_ht) <- mkGammaThetaTables_ gr si
  omega_ht <- HashTable.newSized $ (numSymbols gr)^3
  exec_ $ do 
    _A <- forEach $ nonterminals gr 
    x  <- forEach $ terminals gr
    y  <- forEach $ terminals gr
    let !v0 = if x == y
                 then sum $ map weight $ getUnaryRulesBySymbols gr _A x
                 else 0
    lift $ HashTable.insertWith omega_ht (+) (_A, x, y) v0
    let rs = binaryRulesHeadedBy gr _A
    BinaryRule _ _B _C w <- forEach rs
    lift (HashTable.lookup gamma_ht (_B, x)) >>= \case
      Nothing -> return ()
      Just gamma_Bx ->
        lift (HashTable.lookup theta_ht (_C, y) ) >>= \case
          Nothing -> return ()
          Just theta_Cy ->
            lift $ HashTable.insertWith omega_ht (+) (_A, x, y) (w * gamma_Bx * theta_Cy)
  return omega_ht
  where si = mkSymbolIndex gr

mkGammaThetaTables :: Grammar -> ST s (HashTable s (Symbol, Symbol) Double', HashTable s (Symbol, Symbol) Double')
mkGammaThetaTables gr = mkGammaThetaTables_ gr (mkSymbolIndex gr)

mkGammaThetaTables_ :: Grammar -> SymbolIndex -> ST s (HashTable s (Symbol, Symbol) Double', HashTable s (Symbol, Symbol) Double')
mkGammaThetaTables_ gr si = do 
  let !gamma = fromMaybe (error "mkGammaThetaTables_: could not solve singular linear system for gamma." ) $ linearSolve (ident n - phi_l) psi
      !theta = fromMaybe (error "mkGammaThetaTables_: could not solve singular linear system for theta." ) $ linearSolve (ident n - phi_r) psi
  gamma_ht <- matrixToTable si gamma
  theta_ht <- matrixToTable si theta 
  return (gamma_ht, theta_ht)
  where (phi_l, phi_r) = buildPhiMatrices gr si
        psi = buildPsiMatrix gr si
        (n, m) = size psi
        

mkNonTermIndex :: Grammar -> HashMap Symbol Int
mkNonTermIndex gr = HashMap.fromList [(s, i) | (s, i) <- zip (nonterminals gr) [0..]]

mkTermIndex :: Grammar -> HashMap Symbol Int
mkTermIndex gr = HashMap.fromList [(s, i) | (s, i) <- zip (terminals gr) [0..]]

data SymbolIndex = SI {nonTerminalIndex :: HashMap Symbol Int,
                       terminalIndex :: HashMap Symbol Int} deriving Show

mkSymbolIndex gr = SI (mkNonTermIndex gr) (mkTermIndex gr)                                                                     


buildPhiMatrices gr si = (phi_l, phi_r)
  where !n = length . nonterminals $ gr
        !binaryRules = getRulesById gr $ binaryRuleIds gr
        phi_l_init = (n><n) $ repeat 0
        phi_r_init = (n><n) $ repeat 0

        phi_l = accum phi_l_init (+) lvs
        phi_r = accum phi_r_init (+) rvs

        (lvs, rvs) = foldl' h ([], []) binaryRules
          where h (ls, rs) (BinaryRule h l r (Exp lnw))
                  = (((i, j),w'):ls, ((i, k), w'):rs)
                  where i = (nonTerminalIndex si) HashMap.! h
                        j = (nonTerminalIndex si) HashMap.! l
                        k = (nonTerminalIndex si) HashMap.! r
                        w' = exp lnw

buildPsiMatrix gr si = psi
  where !n = length . nonterminals $ gr
        !m = length . terminals $ gr        
        !unaryRules = getRulesById gr $ unaryRuleIds gr
        psi_init = (n><m) $ repeat 0
        psi = accum psi_init (+) $ do
          UnaryRule h c (Exp lnw) <- unaryRules
          let i = (nonTerminalIndex si) HashMap.! h
              j = (terminalIndex si) HashMap.! c
              w' = exp lnw
          return $! ((i, j), w')

matrixToTable :: SymbolIndex -> Matrix Double -> ST s (HashTable s (Symbol, Symbol) Double')
matrixToTable si m = HashTable.fromList $ do
  (_A, iA) <- HashMap.toList $ nonTerminalIndex si
  (x, ix) <- HashMap.toList $ terminalIndex si
  return $ ((_A, x), Exp . log $ m @@> (iA, ix))

-------------------------------------------------------------------
-- Calculate lambda(_A, n): the probability of _A yielding a string of
-- length n.
type LambdaTable = HashMap (Symbol, Int) Double'
mkLambdaTable :: Grammar -> Int -> HashMap (Symbol, Int) Double'
mkLambdaTable gr n = loop initLambdaTable 2
  where
    initLambdaTable :: HashMap (Symbol, Int) Double'
    initLambdaTable
          = foldl' (\mp (UnaryRule _A t w) -> HashMap.insertWith (+) (_A, 1) w mp)
                   HashMap.empty (unaryRules gr)
    loop :: HashMap (Symbol, Int) Double' -> Int -> HashMap (Symbol, Int) Double'
    loop !table m | m > n = table
                  | otherwise = loop table' (m+1)
          where table' = foldl' go table $ [(k, r) | k <- [1..m-1], r <- binaryRules gr]
                go table (k, BinaryRule _A _B _C w) = 
                  HashMap.insertWith (+) (_A, m)  (w * lambda_B * lambda_C) table
                  where !lambda_B = HashMap.lookupDefault 0 (_B, k) table
                        !lambda_C = HashMap.lookupDefault 0 (_C, m-k) table           
         

  
