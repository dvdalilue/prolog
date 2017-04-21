mergesort([],[]).
mergesort([X],[X]).
mergesort(L,LS) :-
   div(L,A,B),
   mergesort(A,AS), mergesort(B,BS), !,
   merge(AS,BS,LS).

merge([],L,L).
merge(L,[],L):- L \= [].
merge([X|T1],[Y|T2],[X|T]) :- X =< Y, merge(T1,[Y|T2],T).
merge([X|T1],[Y|T2],[Y|T]) :- X > Y, merge([X|T1],T2,T).

split([],[]).
split([H|T],[[H]|Y]) :- split(T,Y).

div([],[],[]).
div(L,A,B) :-
    append(A, B, L),
    length(A, N1),
    length(B, N2),
    D is abs(N1 - N2),
    D < 2.