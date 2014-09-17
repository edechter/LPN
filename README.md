GrammarInduction
================

GIJoe.SRS
===========
A library for writing, querying, and learning Stochastic Rewrite Systems (SRS).

An example SRS can be found in ```data/rewriteSystems/number.sys```

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

