:- expand_environment('$GIJOE_ROOT/prism/util.pl', P), 
   cl(P).


%% 
set_ovbem_rec(Flag, Val) :- 
    retractall(ovbem_rec(Flag, _)),
    assert(ovbem_rec(Flag, Val)).

get_ovbem_rec(Flag, Val) :- ovbem_rec(Flag, Val).

ovbem_rec_assoc(Assoc) :- 
    findall(K\V, ovbem_rec(K, V), Assoc).

:- dynamic ovbem_rec/2. 
ovbem_rec(niter, 100).
ovbem_rec(batch_size, 10).
ovbem_rec(data_set, []).
ovbem_rec(held_out, []).
ovbem_rec(iter_between, 10).
ovbem_rec(held_out_data_expected_log_like, []).
ovbem_rec(logfile, '.ovbem_log').
ovbem_rec(outfile, '.ovbem_out').

save_ovbem_rec :- 
    get_ovbem_rec(outfile, File),
    ovbem_rec_assoc(Assoc), 
    save_clauses(File, [Assoc], []). 

run_ovbem :- 
    ovbem_rec_assoc(Assoc), 
    get_assocs(Assoc, 
               [niter, batch_size, data_set, held_out, iter_between], 
               [NIter, BatchSize, DataSet, HeldOut, IterBetween]), 
    run_held_out(HeldOut),
    ovbem_held_out(NIter, 1, BatchSize, DataSet, HeldOut, IterBetween).

run_held_out(HeldOut) :- 
    free_energy(HeldOut, _, _, _, ExpectedLogLike),
    get_ovbem_rec(held_out_data_expected_log_like, LLs), 
    append(LLs, [ExpectedLogLike], LLs1),
    set_ovbem_rec(held_out_data_expected_log_like, LLs1),
    save_ovbem_rec.
    
ovbem_held_out(NIter, Iter, 
               BatchSize, DataSet, HeldOut, IterBetween) :- 
    (((NIter-Iter) > IterBetween) -> 
        IterBetween1 = IterBetween;
        IterBetween1 = NIter-Iter),
    ovbem(IterBetween1, BatchSize, DataSet, Iter), 
    Iter1 is Iter + IterBetween1,
    run_held_out(HeldOut),
    write('NIter: '), write(NIter), nl,
    write('Iter1: '), write(Iter1), nl,
    (Iter < NIter -> 
        ovbem_held_out(NIter, Iter1, BatchSize, DataSet, HeldOut, IterBetween);
        true).

ovbem(NIter, BatchSize, DataSet) :- 
    ovbem(NIter, BatchSize, DataSet, 1).

ovbem(NIter, BatchSize, DataSet, StartIter) :-
    % get number of latent parameters
    findall(Info, get_sw_a(Info), PInit),
    findall(X, member(switch(_,_,_,X),PInit), Gs),
    flatten(Gs,G),
    length(G,N),
    % initialize rate
    UNIF is 1/N,
    GBar @= [UNIF : _X in 1..N],
    HBar = GBar,
    % run the online VB-EM
    ovbem(NIter, BatchSize, DataSet, rate(2,GBar,HBar,0.01), StartIter, StartIter). 

ovbem(NIter, _BatchSize, _DataSet, _RateInit, StartIter, Iter) :- 
    (Iter-StartIter+1) > NIter, !.
ovbem(NIter, BatchSize, DataSet, RateInit, StartIter, Iter) :- 
    format("ovbem iter# ~w\n", [Iter]),
    ovbem1(DataSet, BatchSize, RateInit, RateFinal), 
    NextIter is Iter+1, 
    ovbem(NIter, BatchSize, DataSet, RateFinal, StartIter, NextIter).


ovbem1(DataSet, BatchSize, RateInit, RateFinal) :- 
    length(DataSet, N),
    % get current alphas
    findall(Info, get_sw_a(Info), ParamsInit),
    % sample a data point from the dataset
    random_multiselect(DataSet, BatchSize, Batch),
    M is round(N/BatchSize),
    BatchOut @= [D1: D in Batch, [D1], multiply_datum_count(D, M, D1)],
    % run vbem
    learn(BatchOut),
    % get output alphas
    findall(Info, get_sw_a(Info), ParamsFinal),
    % update learning rate
    update_learning_rate(ParamsInit,ParamsFinal,RateInit,RateFinal),
    RateFinal = rate(_,_,_,StepSize),
    print_rate(RateFinal),
    % update parameters towards ParamsFinal
    foreach(switch(Sw, Status, Vals, As0) in ParamsInit, [As0, As1, AsOut],
            (member(switch(Sw, Status, Vals, As1), ParamsFinal), !,
             update_vb_params(StepSize, As0, As1, AsOut),
             set_sw_a(Sw, AsOut))).

print_rate(rate(T,G,H,R)) :-
    sumlist(G,GSum),
    format("Tau:~2f GBar:~2f HBar:~2f Rho:~10f ~n",[T,GSum,H,R]).

%% We use an adaptive learning rate requiring no tuning. Essentially,
%% we are trying to minimize the expected error between the batch and
%% stochastic (online) update. We do so via the following update rule:
%%
%% \lambda_{t+1} = (1-\rho_t)\lambda_t + \rho_t \hat{lambda_t} where,
%%
%% lambda_t is the set of estimated hyperparameters from iteration t
%% \hat{lambda_t} is the intermediate estimate for iteration t+1
%%
%% \rho_t = \bar{g}^{\trans}_t \bar{g}_t / \bar{h}_t \bar{g}_t =
%% (1-1/\tau_t)\bar{g}_{t-1} + g_t/\tau_t \bar{h}_t =
%% (1-1/\tau_t)\bar{h}_{t-1} + g_t^{\trans}g_t/\tau_t
%% \tau_{t+1} = \tau_t(1-\rho_t)+1

update_learning_rate(PInit,PFinal,
                     rate(TauT,GBarTMinus1,HBarTMinus1,_),
                     rate(TauTPlus1,GBarT,HBarT,RhoT)) :-
    compute_g(PInit,PFinal,Gt),
    update_bar_g(TauT,GBarTMinus1,Gt,GBarT),
    update_bar_h(TauT,HBarTMinus1,Gt,HBarT),
    update_rho(GBarT,HBarT,RhoT),
    update_tau(TauT,RhoT,TauTPlus1).
    
compute_g(PInit,PFinal,G) :-
    findall(X, (member(switch(Sw,St,Vals,PI),PInit),
                 member(switch(Sw,St,Vals,PF),PFinal),
                 maplist_math(-,PF,PI,X)),Gs),
    flatten(Gs,G).

dot([],[],0).
dot([X|Xs],[Y|Ys],N) :- dot(Xs,Ys,Rest), N is X*Y + Rest.

update_bar_g(TauT,GBarTMinus1,Gt,GBarT) :-
    length(Gt,N),
    Co1 is 1-(1/TauT),
    Co2 is 1/TauT,
    Co1s @= [Co1 : X in 1..N],
    Co2s @= [Co2 : X in 1..N],
    maplist_math(*,Co1s,GBarTMinus1,Summand1),
    maplist_math(*,Co2s,Gt,Summand2),
    maplist_math(+,Summand1,Summand2,GBarT).

update_bar_h(TauT,HBarTMinus1,Gt,HBarT) :-
    dot(Gt,Gt,GDot),
    HBarT is (1-(1/TauT))*HBarTMinus1 + GDot/TauT.

update_rho(GBarT,HBarT,Rho) :-
    dot(GBarT,GBarT,GDot),
    Rho is GDot/HBarT.

update_tau(TauT,RhoT,TauTPlus1) :- 
    TauTPlus1 is TauT*(1-RhoT) + 1.

update_vb_params(_, [], [], []).
update_vb_params(StepSize, [A0|As0], [A1|As1], [AOut|AsOut]) :-
    AOut is (1-StepSize)*A0 + StepSize*A1,
    update_vb_params(StepSize, As0, As1, AsOut).

multiply_datum_count(count(X, C), N, count(X, C1)) :- !, C1 is C * N.
multiply_datum_count(X, N, count(X, N)).

