
:- expand_environment('$GIJOE_ROOT/prism/onlineVBEM.pl', P), 
   cl(P). 
:- expand_environment('$GIJOE_ROOT/prism/util.pl', P), 
   cl(P).
:- expand_environment('$GIJOE_ROOT/prism/assoc_list.pl', P), 
   cl(P).

set_training_flags :-
    set_prism_flag(search_progress, 1), 
    set_prism_flag(em_progress, 1),
    set_prism_flag(max_iterate, 50),
    set_prism_flag(restart,1),
    set_prism_flag(learn_mode,vb),
    set_prism_flag(viterbi_mode,vb),
    set_prism_flag(default_sw_a,uniform),
    set_prism_flag(log_scale,on), 
    set_prism_flag(learn_message, em).


get_current_prism_flags(PrismFlags) :- findall(F\V, get_prism_flag(F, V), PrismFlags).

get_current_learn_statistics(Stats) :- findall(N\S, learn_statistics(N, S), Stats).

get_switchVBInfo(Info) :- findall(X, X=switchVBInfo(_, _, _), Info).

get_switches_post_pruning(Info) :- findall(I, get_sw_pa(I), Info).

execTrainTest(TrainSet, TestSet, SysFileName, RunInfo, FileName) :- 
    % load file
    write('Loading prism file: '), write(SysFileName), write('...'),
    prism(SysFileName), 
    write('Done'), nl, 
    set_srs_prism_flags,

    % get the training set
    length(TrainSet, NEx),

    write('*********'), nl, 
    write('Number of training examples: '), write('NEx'), nl,
    write('Training examples: '), nl, 
    foreach(T in TrainSet, (write(T), nl)),
    write('*********'), nl,

    % run learning
    learn(TrainSet), 
    recordExpectedCounts, 
    pruneAll, 
    write('Pruned ineffective rules...Done.'), nl,
    runTest(Results), 
    get_current_prism_flags(PrismFlags), 
    get_current_learn_statistics(LearnStatistics),
    get_switchVBInfo(SwitchVBInfo), 
    get_switches_post_pruning(SwitchesPruned),
    RunInfo=runInfo(
          numExamples(NEx), 
          sysFileName(SysFileName),
          savePath(FileName), 
          trainSet(TrainSet), 
          testResults(Results), 
          prism_flags(PrismFlags),
          learn_statistics(LearnStatistics), 
          switchVBInfo(SwitchVBInfo), 
          switches_post_pruning(SwitchesPruned)),

    write('Saving results to file: '), write(FileName), write('...'), 
    save_clauses(FileName, [RunInfo], []), 
    write('Done.'), nl.
    

execTrainTestLevels(TrainSetId, Level, NPred, RunInfo, FileName) :- 
    % get name of psm file
    sysFileName(nPred(NPred), SysFileName), 
    % load file
    prism(SysFileName), 
    set_srs_prism_flags,
    % get the training set
    load_clauses('training_sets/trainingSets.pl', [TrainingSets|_], []), 
    member(trainSet(TrainSetId, TrainSet), TrainingSets), !,
    member(level_examples(Level, Exs), TrainSet), !, 
    Gs @= [X:example(_, X) in Exs],
    % running learning
    BatchSize=10, 
    NIter=40, 
    Rho0=0.1, 
    ovbem(NIter, BatchSize, Gs, Rho0), 
    recordExpectedCounts, 
    pruneAll, 
    write('Pruned ineffective rules...Done.'), nl,
    runTest(Results), 
    get_current_prism_flags(PrismFlags), 
    get_current_learn_statistics(LearnStatistics),
    get_switchVBInfo(SwitchVBInfo), 
    get_switches_post_pruning(SwitchesPruned),
    RunInfo=runInfo(
          numExamples(NEx), 
          numPreds(NPred), 
          sysFileName(SysFileName),
          savePath(FileName), 
          trainSet(Gs), 
          testResults(Results), 
          prism_flags(PrismFlags),
          learn_statistics(LearnStatistics), 
          switchVBInfo(SwitchVBInfo), 
          switches_post_pruning(SwitchesPruned)),
    save_clauses(FileName, [RunInfo], []).


execFinal(Level, NPred, RunInfo, FileName) :- 
    % get name of psm file
    sysFileName(nPred(NPred), SysFileName), 
    % load file
    prism(SysFileName), 
    set_srs_prism_flags,
    % get the training set
    allAfter(As), 
    firstNBefore(Level, Bs), 
    append(As, Bs, Gs), 
    ExCounts=1000,
    TrainSet @= [count(X, ExCounts): X in Gs],
    length(TrainSet, NEx),
    % running learning
    learn(TrainSet), 
    recordExpectedCounts, 
    pruneAll, 
    write('Pruned ineffective rules...Done.'), nl,

    lastHalfBefore(TestSet), 
    runTest(TestSet, Results), 
    get_current_prism_flags(PrismFlags), 
    get_current_learn_statistics(LearnStatistics),
    get_switchVBInfo(SwitchVBInfo), 
    get_switches_post_pruning(SwitchesPruned),
    RunInfo=runInfo(
          numExamples(NEx), 
          numPreds(NPred), 
          sysFileName(SysFileName),
          savePath(FileName), 
          trainSet(TrainSet), 
          testResults(Results), 
          prism_flags(PrismFlags),
          learn_statistics(LearnStatistics), 
          switchVBInfo(SwitchVBInfo), 
          switches_post_pruning(SwitchesPruned)),
    save_clauses(FileName, [RunInfo], []).
    

runAccuracy(Acc) :- findall(X, train(X), _Gs), 
                    makeTestSetQuestions(_Gs, _QandAPairs), 
                    testAccuracy(_QandAPairs, Predictions, Acc).

show_num(X) :- set_prism_flag(rerank,20), n_viterbig(20,srs('Number_1'-[X])).

show_next(X,Y) :- set_prism_flag(rerank,20), n_viterbig(20,srs('Next_2'-[X,Y])).

srs(P-IN) :- reduce(P-IN,V), msw(P,V).       

%% change_values(Switch, Vs) :-
%%     lpn_value_pred
%%     atom_concat(Switch, '_values', P),
%%     RetractTerm =.. [P, K],
%%     AssertTerm =..[P, Vs],
%%     retract(RetractTerm), 
%%     assert(AssertTerm). 



%% prune all values for SwitchIn whose values 'a' values are really small 
prune(switch(Sw, _, Vs, As)) :- 
    atom(Sw), 
    length(Vs, NVals),
    AThresh is (2/NVals),
    write('AThresh: '), write(AThresh), nl,
    zip(Vs, As, VAs), 
    Filtered @= [V\A: V\A in VAs, A > AThresh],
    ValuesOut @= [V:V\_ in Filtered],
    AlphaOut @= [A:_\A in Filtered],
    write('Values Out: '), write(ValuesOut), nl, 
    write('Alpha Out: '), write(AlphaOut), nl, 
    ((AlphaOut = []) -> 
        (write(Sw), write('no values'), nl, 
         lpn_set_sw_va(Sw, [noValue], [1]));
        (lpn_set_sw_va(Sw, ValuesOut, AlphaOut),
         write(Sw), nl, 
         write('Set Values to '), write(ValuesOut), nl)).

pruneAll :- 
    findall(Info, get_sw_a(Info), Switches),  
    foreach(Sw in Switches, (prune(Sw))).

:- dynamic switchVBInfo/3.
recordExpectedCounts:- get_reg_sw_list(Switches), 
                       foreach(Sw in Switches, [C, _1,  _2, _3, Alpha], (
                                   get_sw_pa(Sw, _1, _2, _3, Alpha, C), 
                                   retractall(switchVBInfo(Sw, _, _)), 
                                                    assert(switchVBInfo(Sw, Alpha, C)))).
                                 
    
zip(_, [], []) :- !. 
zip([], _, []) :- !. 
zip([X|Xs], [Y|Ys], [X\Y|Rest]) :- zip(Xs, Ys, Rest).

zip3([], [], [], []) :- !. 
zip3(_, [], [], []) :- !.
zip3([], _, [], []) :- !.
zip3([], [], _, []) :- !.
zip3([X|Xs], [Y|Ys], [Z|Zs], [X\Y\Z| Rest]) :- zip3(Xs, Ys, Zs, Rest).

atom_number(N, Atom) :- number_codes(N, Codes), 
                     atom_codes(Atom, Codes).
    
%%%%%%%%%%%%
%% nExs([100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]).
%% nReps(3).
%% nPredicates([1, 2]).
%% saveDir('data/').

%% go(Results) :- 
%%     nExs(NExs), 
%%     nReps(NReps),
%%     Reps@=[I:I in 1..NReps],
%%     nPredicates(NPreds),
%%     RunSpecs@=[NEx\Rep\NPred: 
%%               NEx in NExs, 
%%               Rep in Reps, 
%%               NPred in NPreds],
%%     %% write(RunSpecs), nl,fail, 
%%     Results@= [RunInfo: NEx\Rep\NPred in RunSpecs, 
%%             [NEx_a, Rep_a, NPred_a, FileName, RunInfo], 
%%             (saveDir(SaveDir), 
%%              go(SaveDir, NEx, Rep, NPred, RunInfo))].

go(SaveDir, TrainSetId, Level, NPred, RunInfo) :-
            atom_number(TrainSetId, TrainSetId_a),
            atom_number(Level, Level_a), 
            atom_number(NPred, NPred_a), 
            atom_concats([SaveDir, '/', 'results',
                           '_trainSetId', TrainSetId_a, 
                           '_level_', Level_a, 
                           '_nPred_', NPred_a 
                           ], FileName), 
            write(FileName), nl,
            execTrainTestLevels(TrainSetId, Level, NPred, RunInfo, FileName),  nl, !.

%%%%%%%%%%%
prism_main([TrainSetId_a, Level_a, NPred_a, SaveDir]) :- 
    write('**********************'), nl,nl,
    write('Running srs.psm: prism_main with parameters: '), nl, 
    write('Training Set Id: '), write(TrainSetId_a), nl,
    write('Level: '), write(Level_a), nl,
    write('Num Predicates'), write(NPred_a), nl,
    write('Save In: '), write(SaveDir), nl,
    write('Now: '), system('date'), nl,
    write('**********************'), nl,nl,
    parse_atom(TrainSetId_a, TrainSetId), 
    parse_atom(Level_a, Level), 
    parse_atom(NPred_a, NPred), 
    go(SaveDir, TrainSetId, Level, NPred, Results),
    write('Now: '), system('date'), nl,    
    write('**************************'), nl.


    
    
            
            
    
%%%%%%%%%%%
numExamples(runInfo(
          numExamples(NEx), 
          numPreds(NPred), 
          sysFileName(SysFileName),
          savePath(FileName), 
          trainSet(Gs), 
          testResults(Results), 
          prism_flags(PrismFlags),
          learn_statistics(LearnStatistics), 
          switchVBInfo(SwitchVBInfo), 
          switches_post_pruning(SwitchesPruned)), NEx).

numPreds(runInfo(
          numExamples(NEx), 
          numPreds(NPred), 
          sysFileName(SysFileName),
          savePath(FileName), 
          trainSet(Gs), 
          testResults(Results), 
          prism_flags(PrismFlags),
          learn_statistics(LearnStatistics), 
          switchVBInfo(SwitchVBInfo), 
          switches_post_pruning(SwitchesPruned)), NPred).

testResults(runInfo(
          numExamples(NEx), 
          numPreds(NPred), 
          sysFileName(SysFileName),
          savePath(FileName), 
          trainSet(Gs), 
          testResults(Results), 
          prism_flags(PrismFlags),
          learn_statistics(LearnStatistics), 
          switchVBInfo(SwitchVBInfo), 
          switches_post_pruning(SwitchesPruned)), Results).

switches_post_pruning(runInfo(
          numExamples(NEx), 
          numPreds(NPred), 
          sysFileName(SysFileName),
          savePath(FileName), 
          trainSet(Gs), 
          testResults(Results), 
          prism_flags(PrismFlags),
          learn_statistics(LearnStatistics), 
          switchVBInfo(SwitchVBInfo), 
          switches_post_pruning(SwitchesPruned)), SwitchesPruned).

%%%%%%%%%%%%%%%
%% Analyze Run Accuracies
extractData(File, NEx, NPred, Acc, Sw_Post_Pruning) :- 
    load_clauses(File, [C|_], []), 
    testResults(C, results(_, accuracy(A))), 
    numPreds(C, NPred),
    numExamples(C, NEx), 
    switches_post_pruning(C, Sw_Post_Pruning).
    

printData:- 
    directory_files('.', L), 
    foreach(F in L, 
            [NEx, NPred, Acc, F],
            (
                file_property(F, type(V)),
                V=regular -> (
                     extractData(F, NEx, NPred, Acc, S), 
                     format("~10d , ~10d , ~5f~1n", [NEx, NPred, Acc]))
                ; true)).

%%%%%%%%%%%%%%
loadGrammarFromRunFile(File) :- 
    extractData(File, NEx, NPred, Acc, Sws), 
    foreach(switch(Sw, _, Vs, Ps, As) in Sws, 
            (write(Sw), nl, 
             change_values(Sw, Vs), 
             write(Vs), nl,
             set_sw_a(Sw, As))).

    
%%%%%%% 
dataset(Xs) :- findall(X, example(C, X), Xs).


%%%%%%%%%%%%
take(0, Xs, []). 
take(N, [Y|Ys], [Y|Zs]) :- N1 is N-1, take(N1, Ys, Zs).
take(N, [], []). 


