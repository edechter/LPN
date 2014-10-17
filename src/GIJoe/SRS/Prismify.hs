module GIJoe.SRS.Prismify (prismify, prismClauses) where

import Data.Function (on)
import Data.List
import Data.Text (unpack)

import GIJoe.SRS.Type
import GIJoe.SRS.Parse


-- type PrismScript = String
-- type PrismFlag = (String, String)
-- setPrismFlag :: PrismScript
--                -> PrismFlag
--                -> PrismScript
-- setPrismFlag script (a, b) = script ++ [":- set_prism_flag(" ++
--                                         show a ++ ", " ++ show b ++ ").\n"]



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



prismClauses :: RewriteSystem -> String
prismClauses rs = unlines $ 
  switchClauses ++ [""] ++
  probClauses ++ [""] ++
  reductionClauses
  where switchClauses = map makeSwitch groupedRules
        probClauses = map makeProbs groupedRules
        reductionClauses = concatMap makeReductions groupedRules
        groupedRules = groupBy sameHead $
                       sortBy (compare `on`
                               (cTermPredicate . ruleHead . weightedRuleRule))
                         (rsRules rs)
        sameHead =  (\(WRule ((ComplexTerm p1 _) :<-: _) _)
                  (WRule ((ComplexTerm p2 _) :<-: _) _)
                 -> p1 == p2)

body :: RewriteSystem -> [String]
body slcfrs =
    mainClause ++ [""] ++
    srsClause ++ [""] ++
    switchClauses ++ [""] ++
    probClauses ++ [""] ++ reductionClauses
  where
    groupedRules = groupBy sameHead $ sortBy (compare `on` (cTermPredicate . ruleHead . weightedRuleRule)) (rsRules slcfrs)
    sameHead =  (\(WRule ((ComplexTerm p1 _) :<-: _) _)
                  (WRule ((ComplexTerm p2 _) :<-: _) _)
                 -> p1 == p2)
    mainClause = [ "main(Gs,Foutp,Fouta) :- set_prism_flag(restart,1), set_prism_flag(learn_mode,vb), set_prism_flag(viterbi_mode,vb), set_prism_flag(default_sw_a,uniform), set_prism_flag(log_scale,on), learn(Gs), save_sw(Foutp), save_sw_a(Fouta).",
                  "", "show_num(X) :- set_prism_flag(rerank,20), n_viterbig(20,srs('Number_1'-[X])).", "", "show_next(X,Y) :- set_prism_flag(rerank,20), n_viterbig(20,srs('Next_2'-[X,Y]))."]
    srsClause =  [ "srs(P-IN) :- reduce(P-IN,V), msw(P,V)."]
    switchClauses = map makeSwitch groupedRules
    probClauses = map makeProbs groupedRules
    reductionClauses = concatMap makeReductions groupedRules

makeSwitch :: [WeightedRule] -> String
makeSwitch rs =
  let n = weightedRuleName (head rs)
      n_values = init n ++ "_values'"
  in unlines [":- dynamic " ++ n_values ++ "/1.",
              "values(" ++ n ++ ", Vs) :- " ++ n_values ++ "(Vs).",
              n_values++"(" ++ show [1..(length rs)] ++ ")."]

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
    "reduce(" ++ refactorHead h ++ "," ++ show i ++ ")" ++ body ++ "."
  where
    appendVars = collectTupleItems h
    srsTerms = createSRSTerms h bs
    appendAppends = createAppendTerms h
    
    rhsTerms | null appendVars = []
             | otherwise = groundConditionals ++ [srsTerms] ++ varConditionals
    arity = predArity . cTermPredicate $ h
    groundConditionals = take arity ["(var(A2) -> true; append(X, Y, A2))",
                                                   "(var(B2) -> true; append(U, V, B2) )"]
    varConditionals = take arity ["(var(A2) -> append(X, Y, A2) ; true)",
                                "(var(B2) -> append(U, V, B2) ; true)"]
                      
    body | not (rhsTerms == []) = ":- " ++ intercalate ",\n" rhsTerms
         | otherwise = ""

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
