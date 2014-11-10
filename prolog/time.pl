
nAs(1, [a]). 
nAs(N, [a|Xs]) :- 
    N1 is N-1,
    nAs(N1, Xs).

go(Times) :- 
   Times @= [T:N in 1..15, 
             [Start, X, End, T],
             (cputime(Start), 
              nAs(N, X), 
              viterbi(prove('S_1'-[X])), 
              cputime(End), 
              T is (End-Start)/1000)].
              
