module GIJoe.SRS.Prismify where 

import Data.Function (on)
import Data.List
import Data.Text (unpack)

import GIJoe.SRS.Type
import GIJoe.SRS.Parse



-- showPred :: Predicate -> String
-- showPred (Predicate p i) = concat ["'",unpack p,"_",show i,"'"]

-- predTuplePair :: Predicate -> [String] -> String
-- predTuplePair p xs = showPred p ++ "-" ++ showCleanList xs

-- showCleanList xs = "[" ++ (intercalate "," xs) ++ "]"

-- labeledMap f xs =  map f $ zip xs $ take (length xs) ['A'..'Z']

-- prismClauses :: RewriteSystem -> String
-- prismClauses rs = unlines $ 
--   switchClauses ++ [""] ++
--   probClauses ++ [""] ++
--   reductionClauses ++ [""] ++ 
--   prismToSRSClauses ++ [""] ++
--     [":- include(vis)."]

--   where switchClauses = map makeSwitch groupedRules
--         probClauses = map makeProbs groupedRules
--         reductionClauses = concatMap makeReductions groupedRules
--         groupedRules = groupRulesByHeadPredicate rs
--         prismToSRSClauses = concatMap makePrismToSRS groupedRules

makeSwitch :: (Show r, IsRule r) => 
              Predicate
           -> [r]
           -> String
makeSwitch p rules = unlines [lexSwitchString, reduceSwitchString]
  where
    lexRules = filter isLexRule rules
    reduceRules = filter (not . isLexRule) rules
    lexSwitchString = makeSwitch' p lexRules True
    reduceSwitchString = makeSwitch' p reduceRules False

isLexRule :: IsRule r => r -> Bool
isLexRule = null . body

makeSwitch' :: (Show r, IsRule r) =>
              Predicate
              -> [r]
              -> Bool -- ^ is lexical rule
              -> String
makeSwitch' p rules isLex = 
  unlines $ [":- dynamic " ++ valuePred ++ "/1.",
           "values(" ++ switchName ++ ", Vs) :- " ++ valuePred ++ "(Vs).",
              valuePred ++"(" ++ show [1..k] ++ ")."] ++ [""] ++ toLPNs ++ probs
  where valuePred = quoted $ show p ++ "_values" ++ if isLex then "_lex" else "_reduce"
        k = length rules 
        toLPNs = ["toLPN(" ++ switchName ++ ", " ++ show i ++ ", " ++ showQuoted rule ++ ")."
                 | (i, rule) <- zip [1..] rules]
        probs = [":- set_sw(" ++ switchName ++ ", " ++ show vs ++")"]
                where vs = map (/ (sum ws)) ws
                      ws = map weight rules
        switchName | isLex = "lex(" ++ showQuoted p ++ ")"
                   | otherwise = "reduce(" ++ showQuoted p ++ ")"

showQuoted x = "'" ++ show x ++ "'"
quoted x = "'" ++ x ++ "'"

-- makeProbs :: [WeightedRule] -> String
-- makeProbs rs =
--     ":- set_sw(" ++ weightedRuleName (head rs) ++ "," ++ show normProbList ++ ")."
--   where
--     normProbList = map (/ (sum probList)) probList
--     probList = map pullProb rs
--     pullProb = (\r -> read (show $ weightedRuleWeight r) :: Double)

-- makeReductions :: [WeightedRule] -> [String]
-- makeReductions rs = map makeReduction indexedRules
--   where
--     indexedRules = zip [1..(length rs)] (map weightedRuleRule rs)

-- makeReduction :: (Int,Rule) -> String
-- makeReduction (i,(h :<-: bs)) =
--     "reduce(" ++ refactorHead h ++ "," ++ show i ++ ")" ++ body ++ "."
--   where
--     appendVars = collectTupleItems h
--     srsTerms = createSRSTerms h bs
--     appendAppends = createAppendTerms h
    
--     rhsTerms | null appendVars = []
--              | otherwise = groundConditionals ++ [srsTerms] ++ varConditionals
--     arity = predArity . cTermPredicate $ h
--     groundConditionals = take arity ["(var(A2) -> true; append(X, Y, A2))",
--                                                    "(var(B2) -> true; append(U, V, B2) )"]
--     varConditionals = take arity ["(var(A2) -> append(X, Y, A2) ; true)",
--                                 "(var(B2) -> append(U, V, B2) ; true)"]
                      
--     body | not (rhsTerms == []) = ":- " ++ intercalate ",\n" rhsTerms
--          | otherwise = ""

--     collectTupleItems (ComplexTerm _ xs) = intercalate ", " $ filter (not . null) $ labeledMap collectTupleItem xs
--     collectTupleItem ((NTString (x:[])),l) = ""
--     collectTupleItem ((NTString xs),l) = "var(" ++ l:(show $ length xs) ++ ")"

-- createSRSTerms (ComplexTerm _ xs) bs = if null termList then "" else intercalate ", " termList
--   where
--     termList = map sTermToPair bs
--     sTermToPair h = "srs(" ++ predTuplePair (sTermPredicate h) (map unpack (stermArgs h)) ++ ")"

-- refactorHead h@(ComplexTerm _ xs) = predTuplePair (cTermPredicate h) (renameTupleItems xs)
--   where
--     renameTupleItems xs = labeledMap renameTupleItem xs
--     renameTupleItem ((NTString (x:[])),l) = case x of
--       ElVar _ -> show x
--       ElSym y -> if (unpack y) == "null" then "[]" else show [x]
--     renameTupleItem ((NTString xs),l) = l:(show $ length xs)


-- createAppendTerms h@(ComplexTerm _ xs) =
--     intercalate ", " $ filter (not . null) $ labeledMap createAppends xs
--   where
--     createAppends :: (NonTerminalString,Char) -> String
--     createAppends ((NTString xs),l) =  intercalate ", " $
--       trd3 $ foldl (\(i,prev,acc) curr -> (i+1,
--                                            l:(show $ i+1),
--                                            (appendString prev i curr l):acc))
--         (1,show (xs !! 0),[]) $ drop 1 xs
--     appendString p i x l = concat ["append(",p,",",newVarName x,",",(l:(show $ i+1)),")"]
--     newVarName x = case x of
--       ElVar _ -> show x
--       ElSym _ -> "[" ++ show x ++ "]"

-- prismify :: FilePath -> FilePath -> IO ()
-- prismify inpath outpath = do
--   slcfrs <- readSystem inpath
--   writeFile outpath $ prismClauses slcfrs


-- -------------
-- -- Helpers
-- fst3 (a,_,_) = a
-- snd3 (_,b,_) = b
-- trd3 (_,_,c) = c
-- -------------
