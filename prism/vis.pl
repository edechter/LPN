%% %% Print a Parse Tree %% %%

srsTree(X,[Olhs,Orule,Orhs]) :-
    select(msw(P,V),X,X2),
    select(srs(P-IN),X2,X3),
    delete(X3,reduce(_,_),X4),
    maplist(srsTree,X4,Orhs),
    prismToLPN(P,V,Orule),
    predicateToString(P-IN,Olhs).

%% hack! This currently only works up to 9-ary predicates
predicateToString(P-IN,S) :-
    sub_atom(P,0,_,2,P2),
    atom_lists_to_atoms(IN,A2),
    atom_concat(P2,'(',S1),
    atom_concat(A2,')',S2),
    atom_concat(S1,S2,S).

atom_lists_to_atoms([],'').
atom_lists_to_atoms([X],O) :-
    recursive_atom_concat_space(X,O).

atom_lists_to_atoms([X|XS],O) :-
    length(XS,XL), XL > 0,
    atom_lists_to_atoms(XS,OS),
    recursive_atom_concat_space(X,OS2),
    atom_concat(OS2,', ',O1),
    atom_concat(O1,OS,O).

recursive_atom_concat([],'').
recursive_atom_concat([X],X).
recursive_atom_concat([X|XS],O) :-
    length(XS,XL), XL > 0,
    recursive_atom_concat(XS,OS),
    atom_concat(X,OS,O).

recursive_atom_concat_space([],'').
recursive_atom_concat_space([X],X).
recursive_atom_concat_space([X|XS],O) :-
    length(XS,XL),
    XL > 0,
    recursive_atom_concat_space(XS,OS),
    atom_concat(X,' ',O1),
    atom_concat(O1,OS,O).

formatSRSTree(X) :- format("~n",[]), formatSRSTree(X,'').
formatSRSTree([LHS,RULE,RHS],SEP) :-
    format("~a~a~n~a~a~n",[SEP,LHS,SEP,RULE]),
    atom_concat(SEP,'    ',NEWSEP),
    findall(X,(member(X,RHS),formatSRSTree(X,NEWSEP)),_).

%% %% Print The Grammar %% %%

%% findall([AS,B,C,DS],(get_sw_a(A,_,B,C), D is sum(C), length(B,BL), listOfN(A,BL,AS), listOfN(D,BL,DS), XS2).

%% srs Grammar: give grammar as [predicate,value,count] triplets
srsQuadGrammar(OS) :-
    findall([AS,B,C,DS],(get_sw_a(A,_,B,C), 
                         D is sum(C), 
                         length(B,BL), 
                         listOfN(A,BL,AS), 
                         listOfN(D,BL,DS)),
            XS2),
    write(XS2), nl, 
    maplist(quadZip,XS2,XS3),
    concat(XS3,OS).

%% srsPairGrammar: give grammar as [rule,count] pairs
srsTripleGrammar(OS2) :-
    srsQuadGrammar(OS),
    maplist(ruleCountPair,OS,OS2).

ruleCountPair([P,V,C,S],C-[R,S]) :- prismToLPN(P,V,R).

formatGrammar :- formatGrammar(0).

%% formatGrammar: ignore the machine representation
formatGrammar(T) :- formatGrammar(_,T).

%% formatGrammar: OS is a machine readable representation of the printed grammar
formatGrammar(OS,T) :- current_output(S), formatGrammar(OS,T,S).

formatGrammar(OS,T,S) :-
    sortedDoubleGrammar(OS,T),
    findall(X,(member(X,OS),format(S,"~a:~7f:~7f~n",X)),_).

%% sortedDoubleGrammar: give a pair grammar sorted by expected counts
sortedDoubleGrammar(OS,T) :-
    srsTripleGrammar(OSG),
    keysort(OSG,OSSorted),
    thresholdGrammar(OSSorted,T,OSThreshed),
    reverse(OSThreshed,OSReversed),
    maplist(keyValueToValueKey,OSReversed,OSFlopped),
    maplist(escapePair,OSFlopped,OS).

escapePair([A,C]-B,[A,B,C]).

%% thresholdGrammar: given a pair grammar, 
thresholdGrammar(G,T,GOUT) :-
    reducelist(Y,X,Z,(pastThresh(X,T,XOUT),append(Y,XOUT,Z)),G,[],GOUT).

pastThresh(C-_,T,[]) :- C < T.
pastThresh(C-R,T,[C-R]):- C >= T.

%% %% Utilities %% %%
                               
keyValueToValueKey(A-B,B-A).

pairsToDoubles(A-B,[A,B]).

concat([],[]).
concat([X|XS],OS) :-
    concat(XS,REST),
    append(X,REST,OS).

%% http://stackoverflow.com/questions/6682987
maplist(_C, [], []).
maplist(C, [X|Xs], [Y|Ys]) :-
   call(C, X, Y),
   maplist(C, Xs, Ys).

%% quadZip: zip 4 lists together
quadZip([WS,XS,YS,ZS],OS) :-
    maplist(Y,Z,O1,O1 = [Y,Z],YS,ZS,O2),
    maplist(X,O3,O4,O4 = [X|O3],XS,O2,O5),
    maplist(W,O6,O7,O7 = [W|O6],WS,O5,OS).

%% tripleZip: zip three lists together
tripleZip([XS,YS,ZS],OS) :-
    maplist(Y,Z,O1,(O1 = [Y,Z]),YS,ZS,O2),
    maplist(X,O3,O4,(O4 = [X|O3]),XS,O2,OS).

%% listOfN: give a list of N copies of X
listOfN(_X,0,[]).
listOfN(X,N,[X|XS]) :-
    N > 0,
    Next is N-1,
    listOfN(X,Next,XS).

%%%%%
switch_va(Sw, V, A) :- 
    get_sw_a(Sw, [_, Vs, As]), 
    zip(Vs, As, VAs), 
    member(V\A, VAs).


    
show_lpn :- 
    findall(K, 
            (switch_va(S, V, A), 
             prismToLPN(S, V, R), 
             formatAtom("~w :: ~w\n", [R, A], K), 
             write(K)), _Ks).
