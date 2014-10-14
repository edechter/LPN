module GIJoe.SRS.Prismify (prismify) where

import Data.Function (on)
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

-- header :: RewriteSystem -> [String]
-- header slcfrs = ["%% Original SLCFRS"] ++ [""] commented (rsRules slcfrs) ++ [""]
--  where
--    commented = map (\x -> "%% " ++ show x)

body :: RewriteSystem -> [String]
body slcfrs =
    mainClause ++ [""] ++
    acyclicClause ++ [""] ++
    srsClause ++ [""] ++
    switchClauses ++ [""] ++
    probClauses ++ [""] ++ reductionClauses
  where
    groupedRules = groupBy sameHead $ sortBy (compare `on` (cTermPredicate . ruleHead . weightedRuleRule)) (rsRules slcfrs)
    sameHead =  (\(WRule ((ComplexTerm p1 _) :<-: _) _)
                  (WRule ((ComplexTerm p2 _) :<-: _) _)
                 -> p1 == p2)
    mainClause = [ "main(Gs,Foutp,Fouta) :- set_prism_flag(restart,1), set_prism_flag(learn_mode,vb), set_prism_flag(viterbi_mode,vb), set_prism_flag(default_sw_a,uniform), set_prism_flag(log_scale,on), learn(Gs), save_sw(Foutp), save_sw_a(Fouta).",
                  "", "getTrain(F,Gs) :- load_clauses(F,ALL,[]), findall(X,member(train(X),ALL),Gs).",
                  "", "getTest(F,Gs) :- load_clauses(F,ALL,[]), findall(X,member(test(X),ALL),Gs).", ""]
    srsClause =  [ "srs(P-IN) :- msw(P,V), reduce(P-IN,V)." ]
    acyclicClause = [ "acyclic([A,B],[C,D]) :- length(A,AL), length(B,BL), length(C,CL), length(D,DL), X is AL + BL, Y is CL + DL, X < Y." ]
    switchClauses = map makeSwitch groupedRules
    probClauses = map makeProbs groupedRules
    reductionClauses = concatMap makeReductions groupedRules

makeSwitch :: [WeightedRule] -> String
makeSwitch rs =
    "values(" ++ weightedRuleName (head rs) ++ "," ++ show [1..(length rs)] ++ ")."

makeProbs :: [WeightedRule] -> String
makeProbs rs =
    ":- set_sw(" ++ weightedRuleName (head rs) ++ "," ++ show normProbList ++ ")."
  where
    normProbList = map (/ (sum probList)) probList
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
    appendVars = collectTupleItems h
    appendSRSs = createSRSTerms h bs
    appendAcyclics = createAcyclicTerms h bs
    appendAppends = createAppendTerms h
    theAppends = if (not $ null appendVars)
                 then "(" ++ appendVars ++ " -> " ++ appends1 ++ "; " ++ appends2 ++ ")"
                 else appendSRSs
    appends1 = intercalate ", " $ filter (not . null) [appendSRSs, appendAppends, appendAcyclics]
    appends2 = intercalate ", " $ filter (not . null) [appendAppends, appendAcyclics, appendSRSs]
    anyAppends = if null (theAppends) then "" else " :- " ++ theAppends
    collectTupleItems (ComplexTerm _ xs) = intercalate ", " $ filter (not . null) $ labeledMap collectTupleItem xs
    collectTupleItem ((NTString (x:[])),l) = ""
    collectTupleItem ((NTString xs),l) = "var(" ++ l:(show $ length xs) ++ ")"

createAcyclicTerms (ComplexTerm _ xs) bs = if null termList then "" else intercalate ", " termList
  where
    termList = map sTermToAcyclicCheck bs
    sTermToAcyclicCheck h = "acyclic([" ++ intercalate "," (map unpack $ stermArgs h) ++ "],[" ++ intercalate "," (renameTupleItems xs) ++ "])"
    renameTupleItems xs = labeledMap renameTupleItem xs
    renameTupleItem ((NTString (x:[])),l) = case x of
      ElVar _ -> show x
      ElSym _ -> show [x]
    renameTupleItem ((NTString xs),l) = l:(show $ length xs)
    

createSRSTerms (ComplexTerm _ xs) bs = if null termList then "" else intercalate ", " termList
  where
    termList = map sTermToPair bs
    sTermToPair h = "srs(" ++ predTuplePair (sTermPredicate h) (map unpack (stermArgs h)) ++ ")"

refactorHead h@(ComplexTerm _ xs) = predTuplePair (cTermPredicate h) (renameTupleItems xs)
  where
    renameTupleItems xs = labeledMap renameTupleItem xs
    renameTupleItem ((NTString (x:[])),l) = case x of
      ElVar _ -> show x
      ElSym y -> if (unpack y) == "null" then "[]" else show [x]
    renameTupleItem ((NTString xs),l) = l:(show $ length xs)

createAppendTerms h@(ComplexTerm _ xs) =
    intercalate ", " $ filter (not . null) $ labeledMap createAppends xs
  where
--     createNotUnifies (NTString xs) = intercalate ", " $
--       foldl (\acc curr -> case curr of
--                 ElVar _ -> (show curr ++ "\\=[]"):acc
--                 ElSym _ -> acc) [] xs
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
  writeFile outpath $ unlines $ body slcfrs
