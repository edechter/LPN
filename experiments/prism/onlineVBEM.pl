
ovbem(NIter, BatchSize, DataSet, Rho0) :- 
    ovbem(NIter, BatchSize, DataSet, Rho0, 1). 

ovbem(NIter, BatchSize, DataSet, Rho0, Iter) :- Iter > NIter, !.
ovbem(NIter, BatchSize, DataSet, Rho0, Iter) :- 
    StepSize is (Rho0), 
    ovbem1(DataSet, BatchSize, StepSize), 
    Iter1 is Iter+1, 
    ovbem(NIter, BatchSize, DataSet, Rho0, Iter1).
    
    

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
                                              
    
    
    
    
