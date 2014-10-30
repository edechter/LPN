
ovbem(NIter, BatchSize, DataSet, Eps) :- 
    ovbem(NIter, BatchSize, DataSet, 1, Eps). 

ovbem(NIter, BatchSize, DataSet, Iter, Eps) :- Iter > NIter, !.
ovbem(NIter, BatchSize, DataSet, Iter, Eps) :- 
    Tau0 is 0,
    Kappa is 0.8,
    StepSize is (Tau0 + Iter) ** (-Kappa), 
    ovbem1(DataSet, BatchSize, StepSize),
    save_ovbem_alphas(Iter),
    get_delta_ovbem_alphas(Iter, Delta),
    write('Iter: '), write(Iter), nl,
    write('StepSize: '), write(StepSize), nl,
    write('Normed Delta Alpha: '), write(Delta), nl,
    ((Iter>1, Delta < Eps) -> true; 
     (Iter1 is Iter+1, 
      ovbem(NIter, BatchSize, DataSet, Iter1, Eps))).

ovbem1(DataSet, BatchSize, StepSize) :- 
    length(DataSet, N),
    % get current alphas
    findall(Info, 
            get_sw_a(Info), 
            ParamsInit), 
    % sample a data point from the dataset
    random_multiselect(DataSet, BatchSize, Batch),
    M is round(N/BatchSize),
    BatchOut @= [D1: D in Batch, [D1], multiply_datum_count(D, M, D1)],
    % run vbem
    learn(BatchOut), 
    % get output alphas
    findall(Info, 
            get_sw_a(Info), 
            ParamsFinal),
    % update parameters towards ParamsFinal
    foreach(switch(Sw, Status, Vals, As0) in ParamsInit, [As0, As1, AsOut],
            (member(switch(Sw, Status, Vals, As1), ParamsFinal), !,
             update_vb_params(StepSize, As0, As1, AsOut), 
             set_sw_a(Sw, AsOut))). 

update_vb_params(_, [], [], []).
update_vb_params(StepSize, [A0|As0], [A1|As1], [AOut|AsOut]) :-
    AOut is (1-StepSize)*A0 + StepSize*A1, 
    update_vb_params(StepSize, As0, As1, AsOut).

multiply_datum_count(count(X, C), N, count(X, C1)) :- !, C1 is C * N.
multiply_datum_count(X, N, count(X, N)).

:- dynamic ovbem_alphas/3.
initialize_ovbem_alphas :- retractall(ovbem_alphas).
save_ovbem_alphas(Iter, Sw, Alpha) :- assert(ovbem_alphas(Iter, Sw, Alpha)).
save_ovbem_alphas(Iter):- 
    get_reg_sw_list(Sws), 
    foreach(Sw in Sws, [Alpha, Sw, Vals],
            (get_sw_a(Sw, [unfixed, Vals, Alpha]), 
             save_ovbem_alphas(Iter, Sw, Alpha))).

get_delta_ovbem_alphas(Iter, Sw, Delta) :- get_delta_ovbem_alphas(Iter, Sw, Delta, yes).
get_delta_ovbem_alphas(Iter, Sw, Delta, IsNormed) :- 
    Iter0 is Iter-1, 
    ovbem_alphas(Iter, Sw, Alpha), 
    ovbem_alphas(Iter0, Sw, Alpha0), 
    l1_distance(Alpha, Alpha0, Delta1), 
    (IsNormed = yes -> (length(Alpha, L), 
                        Delta is Delta1/L); 
                       Delta is Delta1).

get_delta_ovbem_alphas(Iter, Delta) :- 
    get_reg_sw_list(Sws), 
    Deltas @= [D : Sw in Sws, [D],
               get_delta_ovbem_alphas(Iter, Sw, D)],
    Delta is sum(Deltas).

    

l1_distance([], [], 0) :- !.
l1_distance([X|Xs], [Y|Ys], D) :- 
    l1_distance(Xs, Ys, D1),
    D is (abs(X-Y) + D1).
l1_distance(_, _,_) :- throw(error('lists of different lengths', l1_distance)).

    
    
                                       
                                              
    
    
    
    
