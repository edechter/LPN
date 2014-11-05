%% util.py
%% Author: Eyal Dechter 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gijoe_root(DIR) :- 
    expand_environment('$GIJOE_ROOT', DIR).
gijoe_bin(DIR) :- 
    gijoe_root(B), 
    atom_concat(B, '/bin', DIR).
prismify(SysFile, PsmFile) :- 
    gijoe_bin(BINDIR), 
    write(BINDIR), nl,
    formatAtom("~a/prismify ~a ~a", [BINDIR, SysFile, PsmFile], A),
    format("Prismifying LPN ~a ... ~1n", [SysFile]),
    system(A),
    format("Done.", []).

% load LPN .sys file at path. By default, load precompiled code found at
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
formatAtom(Format, Ls, A) :- 
    open('/tmp/stream', write, SOut),
    format(SOut, "term('", []), 
    format(SOut, Format, Ls),
    format(SOut, "').", []),
    flush_output(SOut),
    close(SOut),
    open('/tmp/stream', read, SIn),
    read(SIn, In), 
    (In == end_of_file -> close(SIn), 
                          open('/tmp/stream', read, SIn1),
                          read(SIn1, term(A)),
                          close(SIn1);
     In=term(A), close(SIn)).


atom_concats([], '') :- !.
atom_concats([A|Rest], Out) :- atom_concats(Rest, B), 
                               atom_concat(A, B, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
take(_, [], []) :- !. 
take(0, _, []) :- !.
take(N, [X|Xs], [X|Ys]) :- N1 is N - 1, 
                           take(N1, Xs, Ys). 
