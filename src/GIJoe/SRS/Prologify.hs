module GIJoe.SRS.Prologify where

import Data.List
import Data.Text (unpack)

import GIJoe.SRS.Type
import GIJoe.SRS.Parse

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

header :: RewriteSystem -> [String]
header slcfrs =
    ["%% Original SLCFRS",""] ++
    commented (rsRules slcfrs) ++
    ["",""]
  where commented = map (\x -> "%% " ++ show x)

body :: RewriteSystem -> [String]
body slcfrs = ["%% Translation", ""] ++
    switchClauses ++ [""] ++ probClauses ++ [""] ++ reductionClauses
  where
    groupedRules = groupBy sameHead (rsRules slcfrs)
    sameHead =  (\(WRule ((ComplexTerm p1 _) :<-: _) _)
                  (WRule ((ComplexTerm p2 _) :<-: _) _)
                 -> p1 == p2)
    switchClauses = map makeSwitch groupedRules
    probClauses = map makeProbs groupedRules
    reductionClauses = concatMap makeReductions groupedRules

footer :: [String]
footer = ["","","%% Constants"] ++ srsPred ++ projPred
  where
    srsPred =  [ ""
               , "srs(P-IN) :- msw(P,V), reduce(P-IN,V,OUT), recurse(OUT)." ]
    projPred = [ ""
               , "recurse([])."
               , "recurse([P-XS|XS2]) :- srs(P-XS),recurse(XS2)."
               , ""
               , ":- p_not_table recurse/2. % prettify results" ]

makeSwitch :: [WeightedRule] -> String
makeSwitch rs =
    "values(" ++ weightedRuleName (head rs) ++ show [1..(length rs)] ++ ")."

makeProbs :: [WeightedRule] -> String
makeProbs rs =
    ":- set_sw(" ++ weightedRuleName (head rs) ++ show probList ++ ")."
  where
    probList = map pullProb rs
    pullProb = (\r -> read (show $ weightedRuleWeight r) :: Double)

makeReductions :: [WeightedRule] -> [String]
makeReductions rs = map makeReduction indexedRules
  where
    indexedRules = zip [1..(length rs)] (map weightedRuleRule rs)

weightedRuleName :: WeightedRule -> String
weightedRuleName (WRule ((ComplexTerm p _) :<-: _) _) = predicateName p

predicateName :: Predicate -> String
predicateName (Predicate p i) = concat ["'",unpack p,"_",show i,"'"]

makeReduction :: (Int,Rule) -> String
makeReduction (i,(h :<-: bs)) =
    "reduce(" ++
    fst3 manip ++ "," ++
    show i ++ "," ++
    snd3 manip ++ ")" ++
    anyAppends ++ "."
  where
    manip = manipulation h bs
    anyAppends = if null (trd3 manip) then "" else " :- " ++ trd3 manip
    
manipulation :: ComplexTerm -> [SimpleTerm] -> (String,String,String)
manipulation h [] = (cTermToPair h,"[]","")
  where
    cTermToPair h = predicateName (cTermPredicate h) ++ "-" ++ cTermBody h
    cTermBody (ComplexTerm _ xs) = "[" ++ intercalate "," (map cTermEntry xs) ++ "]"
    cTermEntry (NTString xs) = "[" ++ intercalate "," (map show xs) ++ "]"
manipulation h bs = (renameTerms h, printBody bs, createAppendTerms h)
  where
    printBody bs = "[" ++ (intercalate "," $ map sTermToPair bs) ++ "]"
    sTermToPair h = (predicateName (sTermPredicate h)) ++ "-" ++ sTermBody h
    sTermBody (SimpleTerm _ xs) = "[" ++ intercalate "," (map unpack xs) ++ "]"
    renameTerms h@(ComplexTerm _ xs) = (predicateName (cTermPredicate h)) ++ "-[" ++ renameTuples xs ++ "]"
    renameTuples xs = intercalate "," $ map rename $ zip xs $ take (length xs) ['A'..'Z']
    rename ((NTString (x:[])),l) = show x
    rename ((NTString xs),l) = [l] ++ (show $ length xs)
    createAppendTerms h@(ComplexTerm _ xs) = intercalate ", " $ filter (not . null) $ map createAppends $ zip xs $ take (length xs) ['A'..'Z'] 
    createAppends :: (NonTerminalString,Char) -> String
    createAppends ((NTString xs),l) =  intercalate ", " $
      trd3 $ foldl (\(i,p,acc) x -> (i+1,
                                      l:(show $ i+1),
                                      ("append(" ++ p ++ "," ++ (case x of
                                                                    ElVar y -> show x
                                                                    ElSym y -> "[" ++ show x ++ "]")
                                       ++ "," ++ (l:(show $ i+1)) ++ ")"):acc))
      (1,show (xs !! 0),[]) $ drop 1 xs

srsToProlog :: FilePath -> FilePath -> IO ()
srsToProlog inpath outpath = do
  slcfrs <- readSystem inpath
  writeFile outpath $ unlines $ header slcfrs ++ body slcfrs ++ footer
