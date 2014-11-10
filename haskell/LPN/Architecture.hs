
module LPN.Architecture where

import Data.List
import System.Random.Shuffle (shuffle', shuffleM)
import Control.Monad.Random.Class
import Control.Monad

import LPN.Parse
import LPN.Prismify


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
        ruleLex i w v = "A" ++ show i ++ "(" ++ w ++", " ++ v ++ ")."


predicateNetwork :: Int -- ^ R: predicates per base level
                 -> Int -- ^ S: predicates per level
                 -> Int -- ^ K: levels
                 -> [String] -- ^ lexicon
                 -> [String] -- ^ list of observed unary predicates
                 -> FilePath
                 -> IO ()
predicateNetwork _R _S _K lexicon observed outpath = do
  writeFile outpath $ show rs
  writeFile (outpath ++ ".psm") $ prismClauses rs
  where rs = fromRight $ parseSystem "buildGrammar" $ systemString
        fromRight (Right x) = x
        fromRight (Left err) = error $ show err
        systemString = unlines $
                       [shuffleRule level i j k | level <- [1.._K], i <- [1.._S], j <-[1.._S], k <- [j.._S]] ++
                       -- [reverseRule level i j | level <- [1.._K], i <- [1.._S], j <-[1.._S]] ++
                       -- [idRule level i j | level <- [1.._K], i <- [1.._S], j <-[1.._S]] ++                       
                       [ruleLex i w v | i <- [1.._R], w <- lexicon, v <-lexicon, w <= v] ++
                       [ruleLex i w "null" | i <- [1.._R], w <- lexicon] ++
                       [ruleLex i "null" w | i <- [1.._R], w <- lexicon] ++
                       [ruleLex i"null" "null" | i <- [1.._R]] ++ 
                       [pred ++ "(X Y) <-- A" ++ show _K ++ "i" ++ show i ++ "(X, Y)."
                         | pred <- observed, i <- [1.._S]]
        shuffleRule level i j k = unlines $ [
            "A" ++ show level ++ "i" ++ show i ++ "(X Y, U V) <-- " ++
            "A" ++ show (level-1) ++ "i" ++ show j ++ "(X, Y)," ++
            "A" ++ show (level-1) ++ "i" ++ show k ++ "(U, V).", 
            "A" ++ show level ++ "i" ++ show i ++ "(X Y, U V) <-- " ++
            "A" ++ show (level-1) ++ "i" ++ show j ++ "(X, U)," ++
            "A" ++ show (level-1) ++ "i" ++ show k ++ "(Y, V).", 
            "A" ++ show level ++ "i" ++ show i ++ "(X Y, U V) <-- " ++
            "A" ++ show (level-1) ++ "i" ++ show j ++ "(X, V)," ++ 
            "A" ++ show (level-1) ++ "i" ++ show k ++ "(Y, U).",
            "A" ++ show level ++ "i" ++ show i ++ "(X Y, U V) <-- " ++
            "A" ++ show (level-1) ++ "i" ++ show j ++ "(Y, X)," ++ 
            "A" ++ show (level-1) ++ "i" ++ show k ++ "(U, V).",
            "A" ++ show level ++ "i" ++ show i ++ "(X Y, U V) <-- " ++
            "A" ++ show (level-1) ++ "i" ++ show j ++ "(Y, X)," ++ 
            "A" ++ show (level-1) ++ "i" ++ show k ++ "(V, U)."
            ]
        -- reverseRule level i j = 
        --     "A" ++ show level ++ "i" ++ show i ++ "(X, Y) <-- " ++
        --     "A" ++ show (level-1) ++ "i" ++ show j ++ "(Y, X)."
        -- idRule level i j = 
        --     "A" ++ show level ++ "i" ++ show i ++ "(X, Y) <-- " ++
        --     "A" ++ show (level-1) ++ "i" ++ show j ++ "(X, Y)."
        ruleLex i w v = "A0i" ++ show i ++ "(" ++ w ++", " ++ v ++ ")."

