%% Author: Eyal Dechter
%%
%% Calculate the posterior dirichlet variational lowerbound of a test dataset.

:- cl('util.pl').

%% by default, we want to save the explanation graphs of the test
%% dataset goals, so that calculating the free energy multiple times
%% is fast. 
:- set_prism_flag(clean_table, off).

calc_free_energy(Goals, FreeEnergy) :- 
    $pp_learn_check_goals(Goals),
    $pp_learn_message(MsgS,MsgE,MsgT,MsgM),
    $pc_set_em_message(MsgE),
    cputime(Start),
    $pp_clean_learn_info,
    $pp_learn_reset_hparams(vb),
    $pp_trans_goals(Goals,GoalCountPairs,AllGoals),!,
    global_set($pg_observed_facts,GoalCountPairs),
    cputime(StartExpl),
    global_set($pg_num_goals,0),
    $pp_find_explanations(AllGoals),!,
    $pp_print_num_goals(MsgS),
    cputime(EndExpl),
    statistics(table,[TableSpace,_]),
    flush_output,
    $pp_export_sw_info,
    $pp_format_if(MsgM,"done~n"),
    $pp_observed_facts(GoalCountPairs,GidCountPairs,
                       0,Len,0,NGoals,-1,FailRootIndex),
    $pc_prism_prepare(GidCountPairs,Len,NGoals,FailRootIndex),
    $pc_prism_vbem(_, FreeEnergy).







