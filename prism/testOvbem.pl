
:- cl('train.pl').

%% load .sys.psm file.
:- gijoe_root(ROOT),
   formatAtom("~a/experiments/prism/number_net_10_5_5.sys.psm", 
              [ROOT], PATH), 
   prism(PATH).

%% load data set
%% :- ['../experiments/prism/verbs0.psm'].

:- set_training_flags.

%% ovbem config
:- set_ovbem_rec(niter, 30), 
   set_ovbem_rec(batch_size, 1), 
   set_ovbem_rec(iter_between, 5).
   
:- findall(prove('S_1'-[X]), example(X), DataSet), 
    set_ovbem_rec(data_set, DataSet).

:- findall(prove('S_1'-[X]), example(X), DataSet), 
   take(3, DataSet, HeldOut), 
   write(HeldOut), nl, 
    set_ovbem_rec(held_out, HeldOut). 

%% run training
main :- run_ovbem.


    
