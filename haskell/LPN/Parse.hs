{-# Language LambdaCase #-}

module GIJoe.SRS.Parse where

import GIJoe.SRS.Type

import Control.Monad
import Numeric.Log
import qualified Data.HashMap.Strict as HashMap

import Data.Char


import Text.Parsec.Prim hiding (State)
import Text.Parsec hiding (parse, State)
import Text.ParserCombinators.Parsec.Number


readSystem :: FilePath -> IO RewriteSystem
readSystem fpath = do xs <- readFile fpath
                      case parseSystem fpath xs of
                        Right sys -> return sys
                        Left err -> error $ show err


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
                  return $! RewriteSystem rs'
  where getJusts [] = []
        getJusts ((Just x):xs) = x:(getJusts xs)
        getJusts (Nothing:xs) = getJusts xs

lineParser = do {try (try comment <|> emptyLine); return Nothing }
             <|>
             do {r <- ruleParser; return $ Just r}

ruleParser   = do inlineSpaces
                  h <- term
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
                  return $ h :<=: (ss, w)
                   
rhs = do string "<--"
         inlineSpaces
         ss <- terms
         inlineSpaces
         return $ ss

weightAnotation = try $ do 
                           string "::"
                           inlineSpaces
                           d <- floating2 False
                           return d
                  

-- | parse complex terms
term = do p <- predicateSymbol
          char '('
          as <- arguments
          char ')'
          let pred = Predicate p (length as)
          return $ Term pred as

terms = sepBy1 term (try $ inlineSpaces >> char ',' >> inlineSpaces)

predicateSymbol = do p <- many1 alphaNum
                     return $ p

variable = do x <- upper
              xs <- many alphaNum
              return $ x:xs

symbol = do x <- satisfy $ \c -> isAlphaNum c && (not . isUpper) c
            xs <- many alphaNum
            return $ x : xs

element = do {v <- variable; return $ ElVar v} <|> do {s <- symbol; return (ElSym s)}

argument = do x <- sepEndBy1 element inlineSpaces
              return $ Argument x
             
arguments = sepEndBy1 argument (inlineSpaces >> char ',' >> inlineSpaces)






