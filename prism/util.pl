%% util.py
%% Author: Eyal Dechter 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% load LPN at path. By default, load precompiled code found at
% <path>.out. 
load_lpn(Path) :- 
    load_lpn([load], Path).

% load LPN path with supplied options. Options are the same as the
% those to prism(options, path).
load_lpn(Options, Path) :-
    format("Loading LPN in file ~a...~1n", [Path]),
    format("Options: ~w~1n", [Options]),
    prism(Options, Path),
    format("Done").


:- dynamic lpn_value_pred/2. 
lpn_value_pred(Switch, _) :- throw(lpn_value_pred_not_found(Switch)).
assert_lpn_value_pred(Switch, Pred) :- 
    asserta(lpn_value_pred(Switch, Pred)).

%% lpn_set_sw_... 
%% change the values, probs, and alpha values of lpn switch
lpn_set_sw_v(Switch, Vs) :- 
    lpn_value_pred(Switch, P), 
    RetractTerm =.. [P, K], 
    AssertTerm =.. [P, Vs], 
    retract(RetractTerm), 
    assert(AssertTerm).

lpn_set_sw_a(Switch, As) :- 
    set_sw_a(Switch, As).

lpn_set_sw_vp(Switch, Vs, Ps) :- 
    lpn_set_sw_v(Switch, Vs), 
    set_sw(Switch, Ps).

lpn_set_sw_va(Switch, Vs, As) :- 
    lpn_set_sw_v(Switch, Vs), 
    set_sw_a(Switch, As).

lpn_set_sw_vpa(Switch, Vs, Ps, As) :- 
    lpn_set_sw_v(Switch, Vs), 
    set_sw_p(Switch, Ps),
    set_sw_a(Switch, As).

%%%%%%%%%%%%%%%%%%%%

:- dynamic lpn_data/1. 
set_lpn_data(Ds) :- 
    retractall(lpn_data), 
    assert(lpn_data(Ds)).

get_lpn_data(Ds) :- 
    lpn_data(Ds).
