unwinder([], []).
unwinder([E|ES], APP) :-
    is_list(E), !,
    unwinder(E, K1),
    unwinder(ES, K2),
    append(K1, K2, APP). 
unwinder([E|ES], [E|K]) :-
    %\+ is_list(E),
    unwinder(ES, K).

no_member(E, ES) :-     \+ member(E, ES).