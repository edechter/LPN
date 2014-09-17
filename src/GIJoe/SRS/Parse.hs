{-# Language LambdaCase #-}

module GIJoe.SRS.Parse where

import GIJoe.SRS.Type

import Control.Monad
import Numeric.Log
import qualified Data.HashMap.Strict as HashMap

import Data.Char

import Data.Text (pack)

import Text.Parsec.Prim hiding (State)
import Text.Parsec hiding (parse, State)
import Text.ParserCombinators.Parsec.Number


readSystem :: FilePath -> IO RewriteSystem
readSystem fpath = do xs <- readFile fpath
                      case parseSystem fpath xs of
                        Right sys -> return sys
                        Left err -> error $ show err

-- writeSystem fpath info sys = writeFile fpath (unlines (comment:ls))
--   where ls = map go (grammarRules gr)
--         go (BinaryRule h l r w) =
--           "$" ++ show h ++ " --> " ++ "$" ++ show l ++ " " ++ "$" ++ show r ++ " :: " ++ show w
--         go (UnaryRule h c w) =
--           "$" ++ show h ++ " --> " ++ show c ++ " :: " ++ show w
--         comment = '#':info
                         
parseSystem :: String -- ^ source name
            -> String -- ^ system content
            -> Either ParseError RewriteSystem
parseSystem source xs = parse systemParser source xs

comment = do spaces
             char '#'
             _ <- manyTill anyToken eol
             return ()
             
emptyLine = inlineSpaces >> eol >> return ()

eol = char '\n'

inlineSpaces = let oneSpace = satisfy $ \c -> c == ' ' || c == '\t'
               in many oneSpace
                  
systemParser = do rs <- manyTill lineParser eof
                  let rs' = getJusts rs
                  return $! systemFromRules rs'
  where getJusts [] = []
        getJusts ((Just x):xs) = x:(getJusts xs)
        getJusts (Nothing:xs) = getJusts xs

lineParser = do {try (try comment <|> emptyLine); return Nothing }
             <|>
             do {r <- ruleParser; return $ Just r}

ruleParser   = do inlineSpaces
                  h <- cterm
                  inlineSpaces
                  ss <- optionMaybe rhs >>= \case
                    Just ss -> return ss
                    Nothing -> return []
                  w <- optionMaybe weightAnotation >>= \case
                    Just w -> return w
                    Nothing -> return 1
                  char '.'
                  inlineSpaces
                  optional eol
                  return $ WRule (h :<-: ss) w
                   
rhs = do string "<--"
         inlineSpaces
         ss <- sterms
         inlineSpaces
         return $ ss

weightAnotation = try $ do 
                           string "::"
                           inlineSpaces
                           d <- floating2 False
                           return d
                  

-- | parse complex terms
cterm = do p <- predicateSymbol
           char '('
           as <- carguments
           char ')'
           let pred = Predicate p (length as)
           return $ ComplexTerm  pred  as

-- | parse simple terms
sterm = do p <- predicateSymbol
           char '('
           as <- sarguments
           char ')'
           let pred = Predicate p (length as)           
           return $ SimpleTerm pred as

sterms = sepBy1 sterm (try $ inlineSpaces >> char ',' >> inlineSpaces)

predicateSymbol = do p <- many1 alphaNum
                     return $ pack p

variable = do x <- upper
              xs <- many alphaNum
              return $ pack (x:xs)

symbol = do x <- satisfy $ \c -> isAlphaNum c && (not . isUpper) c
            xs <- many alphaNum
            return $ pack (x : xs)

element = do {v <- variable; return $ ElVar v} <|> do {s <- symbol; return (ElSym s)}

ntstring = do x <- sepEndBy1 element inlineSpaces
              return $ NTString x
             
carguments = sepEndBy1 ntstring (inlineSpaces >> char ',' >> inlineSpaces)

sarguments = sepEndBy1 variable (inlineSpaces >> char ',' >> inlineSpaces)




