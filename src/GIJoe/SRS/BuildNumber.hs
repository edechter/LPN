module GIJoe.SRS.BuildNumber where

grammarOfNumber :: Int -> FilePath -> IO ()
grammarOfNumber k outpath = do
  let lexicon = ["one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
  let binaryRules = ["A" ++ show i ++ "(X Y) <-- A" ++ show j ++ "(X), A" ++ show ki ++ "(Y)." | i <- [1..k], j <- [1..k], ki <- [1..k]]
  let unaryRules = ["A" ++ show i ++ "("++ w ++ ")."| i <- [1..k], w <- lexicon]
  let constantRule = ["Number(X) <-- A(X)."]
  writeFile outpath $ unlines $ binaryRules ++ unaryRules ++ constantRule
