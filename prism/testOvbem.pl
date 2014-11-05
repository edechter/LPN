:- ['train.psm'].
:- prism([load], 'out.sys.psm').
:- ['onlineVBEM.pl'].
:- ['../experiments/prism/verbs0.psm'].

:- set_training_flags.

:- set_ovbem_rec(niter, 30), 
   set_ovbem_rec(batch_size, 1), 
   set_ovbem_rec(iter_between, 5).
   
:- findall(prove('S_1'-[X]), example(X), DataSet), 
    set_ovbem_rec(data_set, DataSet).

:- findall(prove('S_1'-[X]), example(X), DataSet), 
   take(3, DataSet, HeldOut), 
   write(HeldOut), nl, 
    set_ovbem_rec(held_out, HeldOut). 

main :- 
    viterbif(prove('S_1'-[[w,a,k]])),
    run_ovbem.

    
    
