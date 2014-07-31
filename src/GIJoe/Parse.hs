

module GIJoe.Parse where

import GIJoe.Grammar


import Control.Monad
import Numeric.Log
import qualified Data.HashMap.Strict as HashMap

import Text.Parsec.Prim hiding (parse, State)
import Text.Parsec hiding (parse, State)
import Text.ParserCombinators.Parsec.Number


readGrammar :: FilePath -> IO Grammar
readGrammar fpath = do xs <- readFile fpath
                       case parseGrammar fpath xs of
                         Right gr -> return gr
                         Left err -> error $ show err

writeGrammar fpath info gr = writeFile fpath (unlines (comment:ls))
  where ls = map go (grammarRules gr)
        go (BinaryRule h l r w) =
          "$" ++ show h ++ " --> " ++ "$" ++ show l ++ " " ++ "$" ++ show r ++ " :: " ++ show w
        go (UnaryRule h c w) =
          "$" ++ show h ++ " --> " ++ show c ++ " :: " ++ show w
        comment = '#':info
                         
parseGrammar :: String -- ^ source name
            -> String -- ^ grammar content
            -> Either ParseError Grammar
parseGrammar source xs = runParser grammarParser (HashMap.empty, 0) source xs

comment = do spaces
             char '#'
             _ <- manyTill anyToken eol
             return ()
             
emptyLine = spaces >> eol >> return ()

eol = char '\n'

grammarParser = do rs <- manyTill lineParser eof
                   return $! grammarFromRules rs

lineParser = skipMany (comment <|> emptyLine) >> ruleParser

ruleParser    = do spaces
                   h <- nonterminal
                   spaces
                   string "-->"
                   spaces
                   ss <- symbols
                   spaces
                   string "::"
                   spaces
                   d <- floating2 False
                   many (char ' ')
                   optional eol
                   case ss of
                     (s:[]) -> return $! UnaryRule h s (Exp (log d))
                     (s1:s2:[]) -> return $! BinaryRule h s1 s2 (Exp (log d))
                     _ -> unexpected $ "Too many symbols on left hnd side of rule."

nonterminal = do char '$'
                 xs <- many1 alphaNum
                 mp <- liftM fst getState                 
                 i <- liftM snd getState
                 case HashMap.lookup xs mp of
                   Just j -> return $ N j (Just xs)
                   Nothing -> do
                     modifyState (\(m, i) -> (HashMap.insert xs i m, i+1))
                     return $ N i (Just xs)

terminal = do xs <- many1 alphaNum
              return $ T xs

symbols = sepEndBy1 (try terminal <|> nonterminal) spaces
