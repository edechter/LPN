
:- expand_environment('$GIJOE_ROOT/prism/train.pl', P), 
   cl(P).

%% load .sys.psm file.
:- expand_environment('$GIJOE_ROOT/experiments/prism/number_net_10_5_5.sys.psm', P),
   prism([load], P),
   set_ovbem_rec(sys_psm_file, P).

%% load data set
%% this includes before and after sentences with 20 held out test sentences
:- expand_environment('$GIJOE_ROOT/experiments/prism/num_trainTest_2.pl', P),
   cl(P).

:- set_training_flags.

%% ovbem config
:- set_ovbem_rec(niter, 1000), 
   set_ovbem_rec(batch_size, 1), 
   set_ovbem_rec(iter_between, 5).

:- expand_environment('$GIJOE_ROOT/experiments/prism/train_number_net_0003.log', P),
   set_ovbem_rec(logfile, P).

:- expand_environment('$GIJOE_ROOT/experiments/prism/train_number_net_0003.data', P),
   set_ovbem_rec(outdir, P), 
   initialize_outdir.
   
:- findall(X, train(_, X), DataSet), 
   set_ovbem_rec(data_set, DataSet).

:- findall(X, test(_, X), HeldOutAll), 
   take(10, HeldOutAll, HeldOut), 
   write(HeldOut), nl, 
   set_ovbem_rec(held_out, HeldOut). 

%% run training
main :- 
    run_ovbem.


    
