module LPN.Network where

import Data.List (permutations)

writeGrammar :: FilePath -> [String] -> IO ()
writeGrammar outpath g = writeFile outpath $ unlines g

-- the full grammar is:
-- for all i,j,k \in [1..n], w1,w2 \in lexicon, A,B,C,D \in permutations [X,Y,U,V]
-- A_i(XY,UV) <-- A_j(A,B),A_k(C,D).
-- A_i([w1],[w2]).
-- S(XY) <-- A_1(X,Y).
krrLPN :: Int -> [String] -> [String]
krrLPN n lexicon =
    reorderRules ++
    reorderRulesEqual ++
    binaryRelationRules ++
    unaryRelationRules ++
    axiom
  where
    reorderRules = ["A" ++ show i ++ "(X Y, U V) <-- A" ++ show j ++ "(" ++
                    (ls !! 0) ++ "," ++ (ls !! 1) ++ "), A" ++ show k ++
                    "(" ++ (ls !! 2) ++ "," ++ (ls !! 3) ++ ")." |
                    i <- [1..n], j <- [1..n], k <- [1..n],
                    ls <- (permutations ["X","Y","U","V"]), j < k]
    reorderRulesEqual = ["A" ++ show i ++ "(X Y, U V) <-- A" ++ show j ++ "(" ++
                         (ls !! 0) ++ "," ++ (ls !! 1) ++ "), A" ++ show j ++
                         "(" ++ (ls !! 2) ++ "," ++ (ls !! 3) ++ ")." |
                         i <- [1..n], j <- [1..n], ls <-
                           ((map (\x -> "X":x) (permutations ["Y","U","V"])) ++
                            (map (\y -> "Y":y) (permutations ["X","U","V"])))]
    binaryRelationRules = ["A" ++ show i ++ "(" ++ (lexicon !! w1) ++ "," ++
                           (lexicon !! w2) ++ ")." | i <- [1..n],
                           w1 <- [0..((length lexicon) - 1)],
                           w2 <- [0..((length lexicon) - 1)], w1 <= w2]
    unaryRelationRules = ["A" ++ show i ++ "(" ++ (lexicon !! w1) ++ ",null)." |
                          i <- [1..n], w1 <- [0..((length lexicon) - 1)] ]
    axiom = ["S(X Y) <-- A1(X,Y)."]

-- other potential architectures?
-- predicateNetwork :: Int -- ^ R: predicates per base level
--              -> Int -- ^ S: predicates per level
--              -> Int -- ^ K: levels
--              -> [String] -- ^ lexicon
--              -> [String] -- ^ list of observed unary predicates
--              -> FilePath
--              -> IO ()
-- predicateNetwork _R _S _K lexicon observed outpath = do
--   writeFile outpath $ show rs
--   writeFile (outpath ++ ".psm") $ "srs(P-IN) :- reduce(P-IN,V), msw(P,V).\n\n" ++ prismClauses rs
--   where rs = fromRight $ parseSystem "buildGrammar" $ systemString
--         fromRight (Right x) = x
--         fromRight (Left err) = error $ show err
--         systemString = unlines $
--                        [shuffleRule level i j k | level <- [1.._K], i <- [1.._S], j <-[1.._S], k <- [j.._S]] ++
--                        [reverseRule level i j | level <- [1.._K], i <- [1.._S], j <-[1.._S]] ++
--                        [idRule level i j | level <- [1.._K], i <- [1.._S], j <-[1.._S]] ++                       
--                        [ruleLex i w v | i <- [1.._R], w <- lexicon, v <-lexicon, w <= v] ++
--                        [ruleLex i w "null" | i <- [1.._R], w <- lexicon] ++
--                        [ruleLex i "null" w | i <- [1.._R], w <- lexicon] ++
--                        [pred ++ "(X Y) <-- A" ++ show _K ++ "i" ++ show i ++ "(X, Y)." | (pred, i) <- zip observed [1..]]                       
--         shuffleRule level i j k = unlines $ [
--             "A" ++ show level ++ "i" ++ show i ++ "(X Y, U V) <-- " ++
--             "A" ++ show (level-1) ++ "i" ++ show j ++ "(X, Y)," ++
--             "A" ++ show (level-1) ++ "i" ++ show k ++ "(U, V).", 
--             "A" ++ show level ++ "i" ++ show i ++ "(X Y, U V) <-- " ++
--             "A" ++ show (level-1) ++ "i" ++ show j ++ "(X, U)," ++
--             "A" ++ show (level-1) ++ "i" ++ show k ++ "(Y, V).", 
--             "A" ++ show level ++ "i" ++ show i ++ "(X Y, U V) <-- " ++
--             "A" ++ show (level-1) ++ "i" ++ show j ++ "(X, V)," ++ 
--             "A" ++ show (level-1) ++ "i" ++ show k ++ "(Y, U)."]
--         reverseRule level i j = 
--             "A" ++ show level ++ "i" ++ show i ++ "(X, Y) <-- " ++
--             "A" ++ show (level-1) ++ "i" ++ show j ++ "(Y, X)."
--         idRule level i j = 
--             "A" ++ show level ++ "i" ++ show i ++ "(X, Y) <-- " ++
--             "A" ++ show (level-1) ++ "i" ++ show j ++ "(X, Y)."
--         ruleLex i w v = "A0i" ++ show i ++ "(" ++ w ++", " ++ v ++ ")."
--         
-- mkCategoryRule i j k = fromRight $ parseSystem "mkCategoryRule" $
--                       "A0p" ++ show i ++ "(X, Y) <-- " ++
--                       "C" ++ show j ++ "(X), " ++
--                       "C" ++ show k ++ "(Y)."
--   where fromRight (Right r) = r
--         fromRight (Left e) = error $ show e
-- 
-- mkLexiconRule w i = fromRight $ parseSystem "mkLexiconRule" $
--                       "C" ++ show i ++ "(" ++ w ++ ")."
--   where fromRight (Right r) = r
--         fromRight (Left e) = error $ show e
-- 
-- mkReverseRule level i j = fromRight $
--   parseSystem "predicateNetwork" $
--   "A" ++ show level ++ "p" ++ show i ++ "(X, Y) <-- " ++
--   "A" ++ show (level-1) ++ "p" ++ show j ++ "(Y, X)."
--   where fromRight (Right r) = r
-- 
-- mkShuffleRule1 level i j k =
--   fromRight $
--   parseSystem "mkShuffleRule1" $
--   "A" ++ show level ++ "p" ++ show i ++ "(X Y, U V) <-- " ++
--   "A" ++ show (level-1) ++ "p" ++ show j ++ "(X, Y), " ++
--   "A" ++ show (level-1) ++ "p" ++ show k ++ "(U, V)." 
--   where fromRight (Right r) = r
--         fromRight (Left e) = error $ show e
--                                                                     
-- mkShuffleRule2 level i j k =
--   fromRight $
--   parseSystem "mkShuffleRule1" $
--   "A" ++ show level ++ "p" ++ show i ++ "(X Y, U V) <-- " ++
--   "A" ++ show (level-1) ++ "p" ++ show j ++ "(X, U), " ++
--   "A" ++ show (level-1) ++ "p" ++ show k ++ "(Y, V)." 
--   where fromRight (Right r) = r
--         fromRight (Left e) = error $ show e
--                                   
-- mkShuffleRule3 level i j k =
--   fromRight $
--   parseSystem "mkShuffleRule1" $
--   "A" ++ show level ++ "p" ++ show i ++ "(X Y, U V) <-- " ++
--   "A" ++ show (level-1) ++ "p" ++ show j ++ "(X, V), " ++
--   "A" ++ show (level-1) ++ "p" ++ show k ++ "(Y, U)." 
--   where fromRight (Right r) = r
--         fromRight (Left e) = error $ show e
-- 
-- -- alternative grammar structure
-- 
-- buildGrammar :: Int -- ^ number of hidden predicates
--              -> [String] -- ^ lexicon
--              -> [String] -- ^ list of observed unary predicates
--              -> FilePath
--              -> IO ()
-- buildGrammar n lexicon observed outpath = do
--   writeFile outpath $ show rs
--   writeFile (outpath ++ ".psm") $ prismClauses rs
--   where rs = fromRight $ parseSystem "buildGrammar" $ systemString
--         fromRight (Right x) = x
--         fromRight (Left err) = error $ show err
--         systemString = unlines $ 
--                        (concat [rules i j k | i <- [1..n], j <-[1..n], k <- [j..n]])++
--                        [ruleLex i w v | i <- [1..n],
--                         (wi, w) <- zip [0..] lexicon,
--                         (vi, v) <- zip [0..] lexicon,
--                         vi >= wi] ++ 
--                        [ruleLex i w "null" | i <- [1..n], w <- lexicon] ++ 
--                        [ruleLex i "null" w | i <- [1..n], w <- lexicon] ++
--                        [pred ++ "(X Y) <-- A" ++ show i ++ "(X, Y)." | (pred, i) <- zip observed [1..]]
--         rules i j k | j /= k  = do
--           [v1, v2, v3, v4] <- permutations ["X", "Y", "U", "V"]
--           return $ "A" ++ show i ++ "(X Y, U V) <-- " ++
--             "A" ++ show j ++ "(" ++ v1 ++ ", " ++ v2 ++ "), " ++
--             "A" ++ show k ++ "(" ++ v3 ++ ", " ++ v4 ++ ")."
--         rules i j k | j == k  = do
--           [v1, v2, v3, v4] <- permutations ["X", "Y", "U", "V"]
--           guard $ (v1, v2) <= (v3, v4)
--           return $ "A" ++ show i ++ "(X Y, U V) <-- " ++
--             "A" ++ show j ++ "(" ++ v1 ++ ", " ++ v2 ++ "), " ++
--             "A" ++ show k ++ "(" ++ v3 ++ ", " ++ v4 ++ ")."
--         -- rule2 i j k =   "A" ++ show i ++ "(X Y, Z) <-- " ++
--         --                 "A" ++ show j ++ "(Z, X), " ++
--         --                 "A" ++ show k ++ "(Z, Y)." 
--         -- rule3 i j k =   "A" ++ show i ++ "(X Y, Z) <-- " ++
--         --                 "A" ++ show j ++ "(X, Y), " ++
--         --                 "A" ++ show k ++ "(Z, Z)."
--         -- rule4 i j k =   "A" ++ show i ++ "(X Y, Z) <-- " ++
--         --                 "A" ++ show j ++ "(Y, X), " ++
--         --                 "A" ++ show k ++ "(Z, Z)."
--         ruleLex i w v = "A" ++ show i ++ "(" ++ w ++", " ++ v ++ ")."
