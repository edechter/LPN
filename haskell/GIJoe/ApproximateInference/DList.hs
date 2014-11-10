
module GIJoe.ApproximateInference.DList where

import Prelude hiding (head, reverse, concat, break, null)
import qualified Prelude

import Debug.Trace

data BTree a = Node (BTree a) (BTree a) |
               Leaf a deriving (Show, Ord, Eq)

type DList a = BTree [a] 

empty :: DList a
empty = Leaf []

null :: DList a -> Bool
null (Node l r) = null l && null r
null (Leaf []) = True
null _ = False

-- | Add element
cons :: a -> DList a -> DList a
cons x (Leaf xs) = Leaf (x:xs)
cons x (Node l r) = Node (cons x l) r

-- | Pop off first element
pop :: DList a -> (a, DList a)
pop dlist = case pop' dlist of
  Nothing -> error "pop: empty list"
  Just res -> res
  where pop' (Leaf (x:xs)) = Just (x, Leaf xs)
        pop' (Node l r) = case pop' l of
          Just (x, l') -> Just (x, concat l' r)
          Nothing -> pop' r
        pop' (Leaf []) = Nothing

        -- debug x = trace "pop'!" x


-- | Remove a single element from head of list
head :: DList a -> a
head (Leaf xs) = Prelude.head xs
head (Node l r) = head l

-- | Return everything but first element
tail :: DList a -> DList a
tail xs = let (_, xs) = pop xs in xs

-- | Concatenate two DLists
concat :: DList a -> DList a -> DList a
concat = Node

-- | Reverse a DList
reverse :: DList a -> DList a
reverse (Leaf xs) = Leaf (Prelude.reverse xs)
reverse (Node l r) = Node (reverse r) (reverse l) -- this enables reverse without evaluating intermediate lists

fromList :: [a] -> DList a
fromList = Leaf
