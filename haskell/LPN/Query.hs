
module GIJoe.SRS.Query where 

import GIJoe.SRS.Type
import Control.Monad.Logic

import Data.Map (Map)
import qualified Data.Map as Map
  
debug = False 

write :: a -> String -> a 
write x  s = if debug then trace s x else x

match :: MonadLogic m => Term -> Term -> m a 
