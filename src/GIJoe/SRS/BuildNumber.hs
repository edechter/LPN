module GIJoe.SRS.BuildNumber where

import Data.List

-- for all i,j,k \in [1..n], w \in lexicon
-- A_i(X,Y) <- A_j(X),A_k(Y).
-- A_i([w]).
-- Number <-- A_1(X).

grammarOfNumber :: Int -> FilePath -> IO ()
grammarOfNumber k outpath = do
  let lexicon = ["one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
  let binaryRules = ["A" ++ show i ++ "(X Y) <-- A" ++ show j ++ "(X), A" ++ show ki ++ "(Y)." | i <- [1..k], j <- [1..k], ki <- [1..k]]
  let unaryRules = ["A" ++ show i ++ "("++ w ++ ")."| i <- [1..k], w <- lexicon]
  let constantRule = ["Number(X) <-- A(X)."]
  writeFile outpath $ unlines $ binaryRules ++ unaryRules ++ constantRule



-- even if we generalize, we don't need every rule
-- for all A,B,C
-- A(X,Y) <- B(Y,X).          # #4# flip
-- A(12,34) <- B(1,2),C(3,4). # #1# both from a single predicate
-- A(12,34) <- B(1,2),C(4,3). # =1, flip C
-- A(12,34) <- B(1,3),C(2,4). # #2# one from each predicate
-- A(12,34) <- B(1,3),C(4,2). # =2, flip C
-- A(12,34) <- B(1,4),C(2,3). # #3#
-- A(12,34) <- B(1,4),C(3,2). # =3, flip C
-- A(12,34) <- B(2,1),C(3,4). # =1, flip B
-- A(12,34) <- B(2,1),C(4,3). # =1, flip B, flip C
-- A(12,34) <- B(2,3),C(1,4). # =3, reordered
-- A(12,34) <- B(2,3),C(4,1). # =3, reordered, flip C
-- A(12,34) <- B(2,4),C(1,3). # =2, reordered
-- A(12,34) <- B(2,4),C(3,1). # =2, reordered, flip C
-- A(12,34) <- B(3,1),C(2,4). # =2, flip B
-- A(12,34) <- B(3,1),C(4,2). # =2, flip B, flip C
-- A(12,34) <- B(3,2),C(1,4). # =3, reordered, flip B
-- A(12,34) <- B(3,2),C(4,1). # =3, reordered, flip B, flip C
-- A(12,34) <- B(3,4),C(1,2). # =1, reordered
-- A(12,34) <- B(3,4),C(2,1). # =1, reordered, flip C
-- A(12,34) <- B(4,1),C(2,3). # =3, flip B
-- A(12,34) <- B(4,1),C(3,2). # =3, flip B, flip C
-- A(12,34) <- B(4,2),C(1,3). # =2, reordered, flip B
-- A(12,34) <- B(4,2),C(3,1). # =2, reordered, flip B, flip C
-- A(12,34) <- B(4,3),C(1,2). # =1, reordered, flip B
-- A(12,34) <- B(4,3),C(2,1). # =1, reordered, flip B, flip C

-- so, the full grammar is:
-- for all i,j,k \in [1..n], w \in lexicon
-- A_i(12,34) <-- A_j(1,2),A_k(3,4).
-- A_i(12,34) <-- A_j(1,3),A_k(2,4).
-- A_i(12,34) <-- A_j(1,4),A_k(2,3).
-- A_i(X,Y) <-- A_j(Y,X).
-- Number(X) <-- A_1(X,Y).
-- A_i([w],[w]).

grammarOfNumber2 :: Int -> FilePath -> IO ()
grammarOfNumber2 n outpath = do
  let lexicon = ["one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
--  let flipRules = ["A" ++ show i ++ "(X,Y) <-- A" ++ show j ++ "(Y,X)."| i <- [1..n], j <- [1..n] ]
  let reorderRules = ["A" ++ show i ++ "(X Y, U V) <-- A" ++ show j ++ "(" ++ (ls !! 0) ++ "," ++ (ls !! 1) ++ "), A" ++ show k ++ "(" ++ (ls !! 2) ++ "," ++ (ls !! 3) ++ ")." | i <- [1..n], j <- [1..n], k <- [1..n], ls <- (permutations ["X","Y","U","V"]) ]
--    let reorderRules = flipRules ++ reorder1s ++ reorder2s ++ reorder3s
  let terminalRules = ["A" ++ show i ++ "("++ w ++ "," ++ w ++ ")."| i <- [1..n], w <- lexicon]
  let numberRule = ["Number(X) <-- A1(X,X)."]
  writeFile outpath $ unlines $ reorderRules ++ terminalRules ++ numberRule
  
