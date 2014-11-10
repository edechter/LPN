

module Main where




main = do
  ss <- readFile "../numbers.txt"
  corpus <- readFile "allSentences"
  let numbers = lines ss
  