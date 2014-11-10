
module GIJoe.Chart where

import Control.Monad.Logic

import Control.Monad.State.Strict
import Control.Monad.Identity


type StateLogicT s m a = LogicT (StateT s m) a

runStateListT :: MonadState s m => LogicT m a -> s -> m [a]
runStateListT logT s = observeAllT logT 

toStateT :: MonadState s m => LogicT m a -> m [a]
toStateT t = do s <- get
                as <- runStateListT t s
                return as

-- test :: StateListT Int Identity (Int, Int, Int)
-- test = do i <- liftList [0..5]
--           j <- liftList [i..5]
--           modify $ \s -> s `div` 2
--           s <- get
--           return $ (i, j, s)



