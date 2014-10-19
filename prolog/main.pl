main(Gs,Foutp,Fouta) :-
    set_prism_flag(restart,1),
    set_prism_flag(learn_mode,vb),
    set_prism_flag(viterbi_mode,vb),
    set_prism_flag(default_sw_a,uniform),
    set_prism_flag(log_scale,on),
    learn(Gs),
    save_sw(Foutp),
    save_sw_a(Fouta).

getTrain(F,Gs) :-
    load_clauses(F,ALL,[]),
    findall(X,member(train(X),ALL),Gs).

getTest(F,Gs) :-
    load_clauses(F,ALL,[]),
    findall(X,member(test(X),ALL),Gs).

srs(P-IN) :-
    msw(P,V),
    reduce(P-IN,V).

