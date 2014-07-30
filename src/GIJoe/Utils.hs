{-# Language BangPatterns, ParallelListComp, FlexibleInstances, DeriveGeneric #-}

module GIJoe.Utils where

import GIJoe.Types

import Prelude hiding (sum)
import Numeric.Log
import qualified Numeric.Sum as Sum
import Numeric.SpecFunctions (digamma, logGammaL)
import Data.Hashable
import Data.List (foldl', sortBy, maximumBy, foldl1')

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap


fromDouble' :: Double' -> Double
{-# INLINE fromDouble' #-}
fromDouble' (Exp lnX) = exp lnX

toDouble' :: Double -> Double'
{-# INLINE toDouble' #-}
toDouble' x = Exp (log x)

{-# RULES "toDouble'/fromDouble'" forall x. toDouble' (fromDouble' x) = x #-}
{-# RULES "fromDouble'/toDouble'" forall x. fromDouble' (toDouble' x) = x #-}

fromListAccum :: (Eq a, Hashable a) => [(a, b)] -> (b -> b -> b) -> HashMap a b -> HashMap a b
fromListAccum pairs f mp = foldl' (\m (r, c) -> HashMap.insertWith f r c m) mp pairs

-- Stats Functions --------
klDirichlet :: [Double'] -> [Double'] -> Double'
klDirichlet qs' ps' = toDouble' $ 
                        logGammaL qs_tot - logGammaL ps_tot
                        + Sum.sum Sum.kbn [logGammaL p - logGammaL q | (p, q) <- zip ps qs]
                        + Sum.sum Sum.kbn [ (q - p) * (digamma q - digamma_q_tot) | (p, q) <- zip ps qs]
  where !qs_tot = Sum.sum Sum.kbn qs
        !ps_tot = Sum.sum Sum.kbn ps
        !digamma_q_tot = digamma qs_tot
        !qs = map fromDouble' qs'
        !ps = map fromDouble' ps'

entropyDirichlet :: [Double] -> Double
entropyDirichlet !qs = logMultinomialBeta qs
                       + (qs_tot - _K) * digamma qs_tot
                       - Sum.sum Sum.kbn [(q - 1) * digamma q | q <- qs]
  where !qs_tot = Sum.sum Sum.kbn qs
        !_K = fromIntegral $ length qs
        
logMultinomialBeta :: [Double] -> Double
logMultinomialBeta !as = Sum.sum Sum.kbn [logGammaL a | a <- as]
                         - logGammaL (Sum.sum Sum.kbn as)
                  


