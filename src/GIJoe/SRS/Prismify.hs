module GIJoe.SRS.Prismify (prismify) where

import Data.List
import Data.Text (unpack)

import GIJoe.SRS.Type
import GIJoe.SRS.Parse

-- utils

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

weightedRuleName :: WeightedRule -> String
weightedRuleName (WRule ((ComplexTerm p _) :<-: _) _) = showPred p

showPred :: Predicate -> String
showPred (Predicate p i) = concat ["'",unpack p,"_",show i,"'"]

predTuplePair :: Predicate -> [String] -> String
predTuplePair p xs = showPred p ++ "-" ++ showCleanList xs

showCleanList xs = "[" ++ (intercalate "," xs) ++ "]"

labeledMap f xs =  map f $ zip xs $ take (length xs) ['A'..'Z']

-- heavy lifters

header :: RewriteSystem -> [String]
header slcfrs = ["%% Original SLCFRS"] ++ commented (rsRules slcfrs) ++ [""]
  where
    commented = map (\x -> "%% " ++ show x)

body :: RewriteSystem -> [String]
body slcfrs =
    srsClause ++ [""] ++ switchClauses ++ [""] ++
    probClauses ++ [""] ++ reductionClauses
  where
    groupedRules = groupBy sameHead (rsRules slcfrs)
    sameHead =  (\(WRule ((ComplexTerm p1 _) :<-: _) _)
                  (WRule ((ComplexTerm p2 _) :<-: _) _)
                 -> p1 == p2)
    srsClause =  [ "srs(P-IN) :- msw(P,V), reduce(P-IN,V)." ]
    switchClauses = map makeSwitch groupedRules
    probClauses = map makeProbs groupedRules
    reductionClauses = concatMap makeReductions groupedRules

makeSwitch :: [WeightedRule] -> String
makeSwitch rs =
    "values(" ++ weightedRuleName (head rs) ++ "," ++ show [1..(length rs)] ++ ")."

makeProbs :: [WeightedRule] -> String
makeProbs rs =
    ":- set_sw(" ++ weightedRuleName (head rs) ++ "," ++ show probList ++ ")."
  where
    probList = map pullProb rs
    pullProb = (\r -> read (show $ weightedRuleWeight r) :: Double)

makeReductions :: [WeightedRule] -> [String]
makeReductions rs = map makeReduction indexedRules
  where
    indexedRules = zip [1..(length rs)] (map weightedRuleRule rs)

makeReduction :: (Int,Rule) -> String
makeReduction (i,(h :<-: bs)) =
    "reduce(" ++ refactorHead h ++ "," ++ show i ++ ")" ++ anyAppends ++ "."
  where
    theAppends = intercalate ", " $ filter (not . null) [createSRSTerms bs, createAppendTerms h]
    anyAppends = if null (theAppends) then "" else " :- " ++ theAppends

createSRSTerms bs = if null termList then "!" else intercalate ", " termList
  where
    termList = map sTermToPair bs
    sTermToPair h = "srs(" ++ predTuplePair (sTermPredicate h) (map unpack (stermArgs h)) ++ ")"

refactorHead h@(ComplexTerm _ xs) = predTuplePair (cTermPredicate h) (renameTupleItems xs)
  where
    renameTupleItems xs = labeledMap renameTupleItem xs
    renameTupleItem ((NTString (x:[])),l) = case x of
      ElVar _ -> show x
      ElSym _ -> show [x]
    renameTupleItem ((NTString xs),l) = l:(show $ length xs)

createAppendTerms h@(ComplexTerm _ xs) =
    intercalate ", " $ filter (not . null) $ labeledMap createAppends xs
  where
    createAppends :: (NonTerminalString,Char) -> String
    createAppends ((NTString xs),l) =  intercalate ", " $
      trd3 $ foldl (\(i,prev,acc) curr -> (i+1,
                                           l:(show $ i+1),
                                           (appendString prev i curr l):acc))
      (1,show (xs !! 0),[]) $ drop 1 xs
    appendString p i x l = concat ["append(",p,",",newVarName x,",",(l:(show $ i+1)),")"]
    newVarName x = case x of
      ElVar _ -> show x
      ElSym _ -> "[" ++ show x ++ "]"

prismify :: FilePath -> FilePath -> IO ()
prismify inpath outpath = do
  slcfrs <- readSystem inpath
  writeFile outpath $ unlines $ header slcfrs ++ body slcfrs
