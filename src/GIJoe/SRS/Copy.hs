module GIJoe.SRS.Copy where

-- copy language
copyGrammar :: [String]
copyGrammar = 
    [ "A(a,a)."
    , "A(b,b)."
    , "A(X a, Y a) <-- A(X,Y)."
    , "A(X b, Y b) <-- A(X,Y)."]
