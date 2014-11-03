module GIJoe.SRS.Number where

import Data.List (intercalate,permutations)

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

first19 = [[o] | o <- ones] ++ [[t] | t <- teens]
theRest = concatMap theDecade tens
    where theDecade x = [[x]] ++ [[x,o] | o <- ones]

-- 3. show the valid sentence types this lexicon generates

mkSentence = intercalate " " . concat

countSentence x n = mkSentence $ take n $ drop (x-1) ns
afterSentence x y = map mkSentence [[["after"],x,["is"],y],
                                    [y,["is"],["after"],x]]
beforeSentence x y = map mkSentence [[["before"],x,["is"],y],
                                     [y,["is"],["before"],x]]
moreSentence x y = map mkSentence [[x,["is"],[rel],["than"],y] | rel <- ["bigger","more","greater"]]
lessSentence x y = map mkSentence [[x,["is"],[rel],["than"],y] | rel <- ["smaller","less","lesser"]]
plusSentence x y z = mkSentence [x,["plus"],y,["is"],z]
minusSentence x y z = mkSentence [x,["minus"],y,["is"],z]
ndSentence x y = map mkSentence [[(o!!0), [eq], (o!!1)] |
                                 o <- permutations [x,y],
                                 eq <- ["is","equals"]]

-- 4. construct all valid sentences

mkAllCount  = [countSentence x n | x <- [1..99], n <-[1..99], x+n <= 100]
mkAllMore   = concat [moreSentence (ns!!x) (ns!!y) |
                      x <- [0..98], y <- [0..98], x > y]
mkAllLess   = concat [lessSentence (ns!!x) (ns!!y) |
                      x <- [0..98], y <- [0..98], x < y]
mkAllAfter  = concat [afterSentence (ns!!x) (ns!!(x+1)) | x <- [0..97]]
mkAllBefore = concat [beforeSentence (ns!!x) (ns!!(x-1)) | x <- [1..98]]
mkAllPlus   = [plusSentence (ns!!x) (ns!!y) (ns!!(x+y+1)) |
             x <- [0..98], y <- [0..98], x+y+1 < 99]
mkAllMinus  = [minusSentence (ns!!x) (ns!!y) (ns!!(x-y-1)) |
             x <- [0..98], y <- [0..98], x > y]
mkAllND     = concat ndSentences
  where
    ndSentences = [ndSentence (ns!!(x10*10+x1-1)) (showNum x10 x1) |
           x10 <- [0..9], x1 <- [0..9], x10*10+x1 <= 99, x10*10+x1 >= 1 ]
    showNum x10 x1 = if x10 > 0 then [show x10, show x1] else [show x1]
