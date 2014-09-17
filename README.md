GrammarInduction
================

GIJoe.SRS
===========
A library for writing, querying, and learning Stochastic Rewrite Systems (SRS).

Example: 
to launch ghci in project directory:
```
>> cabal repl
```
In gchi: 
```
:m + GIJoe.SRS.Parse GIJoe.SRS.Type GIJoe.SRS.Sample
rs <- readSystem "data/rewriteSystems/number.sys"
sampleTerm rs $ SimpleTerm (Predicate "NumDigit" 2) ["X", "Z"]
```

