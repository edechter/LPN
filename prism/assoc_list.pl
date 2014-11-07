%% association list
%% an Association List is a list of key value pairs K\V. 

get_assoc([K\V|_], K, V) :- !.
get_assoc([_|Rest], K, V) :- get_assoc(Rest, K, V). 

get_assocs(Assoc, Ks, Vs) :- 
    Vs @= [V: K in Ks, [V], get_assoc(Assoc, K, V)].
           

set_assoc([], K, V, [K\V]).
set_assoc([K\_|AssocIn], K, V, [K\V|AssocIn]) :- !.
set_assoc([P|AssocIn], K, V, [P|AssocOut]) :- set_assoc(AssocIn, K, V, AssocOut).


assoc_keys([], []).
assoc_keys([K\_|Rest], [K|Ks]) :- assoc_keys(Rest, Ks).
