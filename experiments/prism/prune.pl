%% take a psm srs system and prune out rules

write_sw_vs(Sw, Vs, Clause) :- 
    Clause=values(Sw, Vs).

write_sw_ps(Sw, Ps, Clause) :- 
    Clause = (:- set_sw(Sw, Ps)).

write_sw_as(Sw, As, Clause) :- 
    Clause = (:- set_sw_a(Sw, As)). 

write_sw(Sw, [Vs, Ps, As], Clauses) :- 
    write_sw_vs(Sw, Vs, C1), 
    write_sw_ps(Sw, Ps, C2), 
    write_sw_as(Sw, As, C3), 
    Clauses = [C1, C2, C3].

delete_switches([], []). 
delete_switches([C|CsIn], CsOut) :- 
    C = values(_, _), !, delete_switches(CsIn, CsOut).
delete_switches([C|CsIn], [C|CsOut]) :- 
    C = values(_, _), !, delete_switches(CsIn, CsOut).

replace_switches(ClausesIn, Switches, ClausesOut) :- 
    delete_switches(ClausesIn, Clauses1), 
    NewSwitches @= [Clauses : [Sw, Vs, Ps, As] in Switches, 
                    [Clauses],
                    write_sw(Sw, [Vs, Ps, As], Clauses)], 
    append(NewSwitches, Clauses1, ClausesOut). 
    
    

 
    

