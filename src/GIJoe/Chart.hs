
module GIJoe.Chart where

import GIJoe.ListT

import Control.Monad.State.Strict
import Control.Monad.Identity


type StateListT s m a = ListT (StateT s m) a

runStateListT :: MonadState s m => ListT m a -> s -> m [a]
runStateListT t s = foldListT f (return []) t
  where f a m = do as <- m
                   return (a:as)
                   
toStateT :: MonadState s m => ListT m a -> m [a]
toStateT t = do s <- get
                as <- runStateListT t s
                return as

test :: StateListT Int Identity (Int, Int, Int)
test = do i <- liftList [0..5]
          j <- liftList [i..5]
          modify $ \s -> s `div` 2
          s <- get
          return $ (i, j, s)



