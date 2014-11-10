
module Verbs where

import Data.List
import System.Random.Shuffle (shuffle', shuffleM)
import Control.Monad.Random.Class
import Control.Monad


import GIJoe.SRS.Parse
import GIJoe.SRS.Prismify
import GIJoe.SRS.Prismify

ipaLexicon = do xs <- readFile "./experiments/prism/englishPhones.txt"
                return $ lines xs


buildGrammar :: Int -- ^ number of hidden predicates
             -> [String] -- ^ lexicon
             -> [String] -- ^ list of observed unary predicates
             -> FilePath
             -> IO ()
buildGrammar n lexicon observed outpath = do
  writeFile outpath $ show rs
  writeFile (outpath ++ ".psm") $ prismClauses rs
  where rs = fromRight $ parseSystem "buildGrammar" $ systemString
        fromRight (Right x) = x
        fromRight (Left err) = error $ show err
        systemString = unlines $ 
                       (concat [rules i j k | i <- [1..n], j <-[1..n], k <- [j..n]])++
                       [ruleLex i w v | i <- [1..n],
                        (wi, w) <- zip [0..] lexicon,
                        (vi, v) <- zip [0..] lexicon,
                        vi >= wi] ++ 
                       [ruleLex i w "null" | i <- [1..n], w <- lexicon] ++ 
                       [ruleLex i "null" w | i <- [1..n], w <- lexicon] ++
                       [pred ++ "(X Y) <-- A" ++ show i ++ "(X, Y)." | (pred, i) <- zip observed [1..]]
        rules i j k | j /= k  = do
          [v1, v2, v3, v4] <- permutations ["X", "Y", "U", "V"]
          return $ "A" ++ show i ++ "(X Y, U V) <-- " ++
            "A" ++ show j ++ "(" ++ v1 ++ ", " ++ v2 ++ "), " ++
            "A" ++ show k ++ "(" ++ v3 ++ ", " ++ v4 ++ ")."
        rules i j k | j == k  = do
          [v1, v2, v3, v4] <- permutations ["X", "Y", "U", "V"]
          guard $ (v1, v2) <= (v3, v4)
          return $ "A" ++ show i ++ "(X Y, U V) <-- " ++
            "A" ++ show j ++ "(" ++ v1 ++ ", " ++ v2 ++ "), " ++
            "A" ++ show k ++ "(" ++ v3 ++ ", " ++ v4 ++ ")."
        -- rule2 i j k =   "A" ++ show i ++ "(X Y, Z) <-- " ++
        --                 "A" ++ show j ++ "(Z, X), " ++
        --                 "A" ++ show k ++ "(Z, Y)." 
        -- rule3 i j k =   "A" ++ show i ++ "(X Y, Z) <-- " ++
        --                 "A" ++ show j ++ "(X, Y), " ++
        --                 "A" ++ show k ++ "(Z, Z)."
        -- rule4 i j k =   "A" ++ show i ++ "(X Y, Z) <-- " ++
        --                 "A" ++ show j ++ "(Y, X), " ++
        --                 "A" ++ show k ++ "(Z, Z)."
        ruleLex i w v = "A" ++ show i ++ "(" ++ w ++", " ++ v ++ ")."

