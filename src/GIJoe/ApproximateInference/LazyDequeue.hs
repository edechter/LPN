
module GIJoe.ApproximateInference.LazyDequeue
       (LazyDequeue(..),
        empty,
        enqueue,
        enqueueMany,
        dequeue,
        fromList,
        toList,
        null)
       where

import Prelude hiding (null)
import GIJoe.ApproximateInference.DList (DList)
import qualified GIJoe.ApproximateInference.DList as DList

import Debug.Trace

data LazyDequeue a = LazyDequeue (DList a) (DList a) deriving (Show, Eq, Ord)

-- | An empty queue
empty :: LazyDequeue a
{-# INLINE empty #-}
empty = LazyDequeue DList.empty DList.empty

null :: LazyDequeue a -> Bool
null (LazyDequeue l r) = DList.null l && DList.null r

enqueue :: a -> LazyDequeue a -> LazyDequeue a
{-# INLINE enqueue #-}
enqueue a (LazyDequeue l r) = LazyDequeue (DList.cons a l) r

enqueueMany :: [a] -> LazyDequeue a -> LazyDequeue a
{-# INLINE enqueueMany #-}
enqueueMany xs q = let (LazyDequeue l r) = shift q 
  in LazyDequeue l (DList.concat (DList.fromList xs) r)

-- | Dequeue a single element. If the right list is empty, reverses
-- the first list and sticks the result in the second list.
dequeue :: LazyDequeue a -> (a, LazyDequeue a)
dequeue (LazyDequeue l r) | DList.null r = dequeue $ shift (LazyDequeue l r)
                          | otherwise    = (a, LazyDequeue l r')
  where (a, r') = DList.pop r

toList :: Show a => LazyDequeue a -> [a]
toList q | null q = []
         | otherwise = let (x, q') = dequeue q in x: toList q'

fromList :: [a] -> LazyDequeue a
{-# INLINE fromList #-}
fromList xs = enqueueMany xs empty

------------------------
-- Private functions

shift :: LazyDequeue a -> LazyDequeue a
shift (LazyDequeue l r) = LazyDequeue DList.empty (DList.concat r (DList.reverse l))



                                 









