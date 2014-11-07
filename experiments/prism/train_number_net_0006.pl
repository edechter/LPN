
:- expand_environment('$GIJOE_ROOT/prism/train.pl', P), 
   cl(P).

%% load .sys.psm file.
load_sys_psm :- expand_environment('$GIJOE_ROOT/experiments/prism/number_net_5_5_5.sys.psm', 
                                   P),
                prism( P),
                set_ovbem_rec(sys_psm_file, P).

%% load data set
%% this includes before and after sentences with 20 held out test sentences
:- expand_environment('$GIJOE_ROOT/experiments/prism/num_trainTest_2.pl', P),
   cl(P).

:- set_training_flags.

%% ovbem config
:- set_ovbem_rec(niter, 100), 
   set_ovbem_rec(batch_size, 10).

:- expand_environment('$GIJOE_ROOT/experiments/prism/train_number_net_0006.log', P),
   set_ovbem_rec(logfile, P).

:- expand_environment('$GIJOE_ROOT/experiments/prism/train_number_net_0006.data', P),
   set_ovbem_rec(outdir, P).
   
:- findall(count(X, 100), train(_, X), DataSet), 
   set_ovbem_rec(data_set, DataSet).

%% run training
main :- 
    initialize_outdir,
    load_sys_psm, 
    set_training_flags,
    run_ovbem.


    
          
