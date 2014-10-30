module GIJoe.SRS.Prismify where 

import Prelude hiding (head)

import Data.Function (on)
import Data.List hiding (head)
import Data.Text (unpack)

import Data.Map (Map, (!))
import qualified Data.Map as Map

import GIJoe.SRS.Type
import GIJoe.SRS.Parse



-- predTuplePair :: Predicate -> [String] -> String
-- predTuplePair p xs = showPred p ++ "-" ++ showCleanList xs

type RuleMap = Map Predicate [Rule]

mkRuleMap rs = Map.map (map dropRuleWeight) $ groupRulesByHeadPredicate rs

mkValues pred ruleMap = [dynamic_dec, value_def, value_pred_def, lpn_value_pred_assert]
  where switchName = showQuoted pred
        value_pred = quoted $ show pred ++ "_values"
        dynamic_dec = ":- dynamic " ++ value_pred ++ "/1."
        value_def =  "values(" ++ switchName ++ ", Vs) :- " ++ value_pred ++ "(Vs)."
        value_pred_def = value_pred ++"(" ++ show [1..numRules] ++ ")."
        lpn_value_pred_assert = ":- assert_lpn_value_pred("
                                ++ switchName ++ ", " ++ value_pred ++ ")."
        rules = ruleMap ! pred
        numRules = length rules

mkAllValues ruleMap = unlines [unlines $ mkValues p ruleMap | p <- Map.keys ruleMap]

mkAllRules :: RuleMap -> String
mkAllRules ruleMap = unlines [unlines $ mkPredRule p ruleMap | p <- Map.keys ruleMap]

mkAllPrismToLPN :: RuleMap -> String
mkAllPrismToLPN ruleMap = unlines $ do
  p <- Map.keys ruleMap
  return $ unlines $do
    (i, r) <- zip [1..] $ ruleMap ! p
    return $ mkPrismToLPN r i


mkPredRule :: Predicate -> RuleMap -> [String]
mkPredRule pred ruleMap = do (i, r) <- zip [1..] rules
                             return $ mkRule r i
  where rules = ruleMap ! pred

mkRule :: Rule -> Int -> String
mkRule rule i =  _H ++ if null (_B1 ++ _C ++ _B2) then "."
                         else (" :- \n\t\t" ++ intercalate ",\n\t\t" (filter (not.null) [_B1,  _C, _B2]) ++  ".")
  where _A = quoted . show . headPredicate $ rule
        ys = do (Argument els, k) <- zip (termArgs $ head rule) [(1::Int)..]
                case els of
                  [ElSym "null"]  -> return $ ("[]", Nothing)
                  [ElSym e] -> return $ ("[" ++ quoted e ++ "]", Nothing)
                  [ElVar v] -> return $ (v, Nothing)                  
                  els ->
                    let v = "Z_" ++ show k 
                    in return $ (v, Just $ "append(" ++
                                    intercalate ", " (map show els ++ [v]) ++ ")")
                                    
                                                 

        ts = do Term p args <- body rule
                let as = do Argument els <- args
                            case els of
                              [ElSym "null"]  -> return $ "[]"
                              [ElSym e] -> return $ "[" ++ quoted e ++ "]"
                              [ElVar v] -> return $ v
                              _ -> error $ "multiple variables in lhs terms."
                return $ "prove(" ++ showQuoted p
                          ++ "-[" ++ (intercalate ", " as) ++ "])"
                
                                    
        _H = "rule(" ++ _A ++ "-[" ++ intercalate ", " (map fst ys) ++ "], "
             ++ show i ++ ")"
        _B1 =  intercalate ", " $ ["(var(" ++ v ++ ") -> true; " ++ fromJust y ++ ")"| (v, y) <- ys, isJust y]
        _B2 =  intercalate ", " $ ["(var(" ++ v ++ ") -> " ++ fromJust y ++ "; true)"| (v, y) <- ys, isJust y]
        isJust (Just _) = True
        isJust _ = False
        fromJust (Just x) = x

        _C = intercalate ", " $ ts
                
mkPrismToLPN :: Rule -> Int -> String
mkPrismToLPN rule i = "prismToLPN(" ++ intercalate ", " args ++ ")."
  where switch = showQuoted $ headPredicate rule
        ruleString = showQuoted rule
        args = [switch, show i, ruleString]


prismClauses :: RewriteSystem -> String
prismClauses sys = unlines [":- include('prove.psm').",
                            mkAllValues ruleMap, "",
                            mkAllRules ruleMap, "",
                            mkAllPrismToLPN ruleMap]
  where ruleMap = mkRuleMap sys        

prismify :: FilePath -> FilePath -> IO ()
prismify inpath outpath = do
  sys <- readSystem inpath
  writeFile outpath $ prismClauses sys


------
showQuoted x = "'" ++ show x ++ "'"
quoted x = "'" ++ x ++ "'"
