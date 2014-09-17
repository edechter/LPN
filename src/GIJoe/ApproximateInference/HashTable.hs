{-# Language BangPatterns, LambdaCase #-}

module GIJoe.ApproximateInference.HashTable
       (HashTable,
        new,
        newSized,
        insert,
        insertWith,
        delete,
        lookup,
        lookupDefault,        
        foldM,
        mapM_,
        fromList,
        toList,
        showTable
        )
       where

import Prelude hiding (lookup, mapM_)
import Data.HashTable.Class hiding (HashTable)
import qualified Data.HashTable.ST.Basic as H
import Control.Monad.ST
import Data.Hashable (Hashable(..))

type HashTable s k v = H.HashTable s k v

insertWith :: (Eq k, Hashable k) => HashTable s k v -> (v -> v -> v) -> k -> v -> ST s ()
insertWith htRef f k v = do
  res <- lookup htRef k
  case res of
    Nothing -> insert htRef k v
    Just w -> let !v' = f w v in insert htRef k v'

lookupDefault :: (Eq k, Hashable k) => HashTable s k v -> v -> k -> ST s v
lookupDefault h d k = lookup h k >>= \case
  Nothing -> return d
  Just x -> return x
    


showTable :: (Eq k, Hashable k, Show k, Show v) => HashTable s k v -> ST s String
showTable htRef = do rows <- toList htRef
                     return $ unlines $ map show rows

