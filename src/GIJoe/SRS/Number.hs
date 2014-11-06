module GIJoe.SRS.Number where

import Data.List (intercalate,permutations,zip4)

import GIJoe.SRS.TestTrain

-- 1. define the lexicon
ones = ["one","two","three","four","five","six","seven","eight","nine"]
tens = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
digits = ["0","1","2","3","4","5","6","7","8","9"]
functionals = ["than","is","equals","after","before","just","plus","minus","than","bigger","smaller","less","more","greater","lesser"]
lexicon = ones ++ tens ++ teens ++ digits ++ functionals

-- 2. define useful auxiliary lists
ns :: [[String]]
ns = first19 ++ theRest

parametricWeights :: Float -> Float -> Int -> [Float]
parametricWeights p w l = map (\n -> (w*(1-p)**((fromIntegral n-1))*p) + (1-w)/(fromIntegral l)) [1..l]
nWeights :: [Float]
nWeights = parametricWeights 0.5 0.75 99

first19 = [[o] | o <- ones] ++ [[t] | t <- teens]
theRest = concatMap theDecade tens
    where theDecade x = [[x]] ++ [[x,o] | o <- ones]

digitNs :: [[String]]
digitNs = (map (\x -> [x]) (drop 1 digits)) ++ concatMap theDecade (drop 1 digits)
  where theDecade x = [[x,o] | o <- digits]

-- 3. show the valid sentence types this lexicon generates

countSentence x n = take n $ drop (x-1) ns
afterSentence x y = [[["after"],(ns!!x),["is"],(ns!!y)],
                     [(ns!!y),["is"],["after"],(ns!!x)]]
beforeSentence x y = [[["before"],(ns!!x),["is"],(ns!!y)],
                      [(ns!!y),["is"],["before"],(ns!!x)]]
moreSentence x y = [[(ns!!x),["is"],[rel],["than"],(ns!!y)] |
                    rel <- ["bigger","more","greater"]]
lessSentence x y = [[(ns!!x),["is"],[rel],["than"],(ns!!y)] |
                    rel <- ["smaller","less","lesser"]]
plusSentence x y z = [(ns!!x),["plus"],(ns!!y),["is"],(ns!!z)]
minusSentence x y z = [(ns!!x),["minus"],(ns!!y),["is"],(ns!!z)]
ndSentence x = [[(o!!0), [eq], (o!!1)] |
                o <- permutations [(ns!!x),(digitNs!!x)],
                eq <- ["is","equals"]]

-- 4. construct all valid sentences

mkAllCount  = [countSentence x n | x <- [1..99], n <-[1..99], x+n <= 100]
mkAllMore   = concat [moreSentence x y | x <- [0..98], y <- [0..98], x > y]
mkAllLess   = concat [lessSentence x y | x <- [0..98], y <- [0..98], x < y]
mkAllAfter  = concat [afterSentence x (x+1) | x <- [0..97]]
mkAllBefore = concat [beforeSentence x (x-1) | x <- [1..98]]
mkAllPlus   = [plusSentence x y (x+y+1) | x <- [0..98], y <- [0..98], x+y+1 < 99]
mkAllMinus  = [minusSentence x y (x-y-1) | x <- [0..98], y <- [0..98], x > y]
mkAllND     = concat [ndSentence x | x <- [0..98] ]

-- 5. split sentences based on the numbers involved

sortByWords :: (Eq a) => [[a]]     -- a list of compound words
               -> [[[a]]]   -- a list of sentences broken into compound words
               -> [[[[a]]]] -- a list of sentences sorted by compound word membership
sortByWords ws ss = map (\w -> filter (elem w) ss) ws ++
                    [filter (\s -> all (flip notElem s) ws) ss]

rmCompounds :: [[[[a]]]] -> [[[a]]]
rmCompounds = map (map concat)

sentencesToUnaryPredicates = map (map (\x -> [x]))

-- 6. create train/test data

-- perhaps we just want a list of triples with sentence types, breakdowns, and

type NumberSpec = ([Example],[[String]],[PercTrain])
type NumberSpecs = [NumberSpec]

defaultNumberSpecs :: NumberSpecs
defaultNumberSpecs = zip3 allSentences (repeatN n first19) (repeatN n trainSplits)
  where
    n = length allSentences
    allSentences = [mkAllCount, mkAllMore, mkAllLess, mkAllAfter, mkAllBefore, mkAllND]
    trainSplits = (repeatN 19 0.8) ++ [0.1]



repeatN n x = take n $ repeat x

numberTrainTestData :: FilePath -> NumberSpecs -> IO ()
numberTrainTestData path specs = mkTrainTestDataFile path ttSpecs
  where
    ttSpecs = foldl ttSpecHelper [] specs
    ttSpecHelper :: [TrainTestSpec] -> NumberSpec -> [TrainTestSpec]
    ttSpecHelper acc (ss,bd,percs) = acc ++
      zip3 (repeatN (1 + (length bd)) "S_1") (sentencesToUnaryPredicates $ rmCompounds $ sortByWords bd ss) percs
