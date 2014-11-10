
module Main where

import System.Environment
import LPN.Prismify

-- executable main function: 
main = do
  args <- getArgs
  progName <- getProgName
  let go fin fout = prismify fin fout
  let prismify_usage = unlines [
        "Usage: prismify input-sys-file output-psm-file."
        ]
  case args of
    [] -> error $ unlines ["no arguments supplied.", prismify_usage]
    [fin] -> go fin (fin ++ ".psm")
    [fin, fout] -> go fin fout
    _ -> error $ unlines ["too many arguments.", prismify_usage]
    
