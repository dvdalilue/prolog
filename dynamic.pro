:- dynamic([fibonacci/2]).
:- dynamic([genNumeros/2]).
:- dynamic([product/3]).

mergesort([],[]) :- !.
mergesort([X],[X]) :- !.
mergesort(L,R) :-
    divide(L,A,B),
    mergesort(A,AS), mergesort(B,BS),
    merge(AS,BS,R), !.

divide(L,A,B) :-
    append(A,B,L),
    length(A,N1),
    length(B,N2),
    abs(N1 - N2) < 2.

merge([],L,L).
merge(L,[],L).
merge([H1|T1],[H2|T2],R) :-
    H1 =< H2, merge(T1,[H2|T2],R2), R = [H1|R2].
merge([H1|T1],[H2|T2],R) :-
    H2 =< H1, merge([H1|T1],T2,R2), R = [H2|R2].

fibonacci(1,1).
fibonacci(2,1).
fibonacci(Number,F):-
    Number > 2,
    N1 is Number - 1,
    fibonacci(N1,F1),
    N2 is Number - 2,
    fibonacci(N2,F2),
    F is F1 + F2, !,
    asserta((fibonacci(Number,F):-!)).

genNumeros(0,[]) :- !.
genNumeros(N,[H|T]) :-
        N > 0,
        NN is N - 1,
        H = N,
        genNumeros(NN,T),
        asserta((genNumeros(N,[H|T]) :- !)).

foo :- retract(genNumeros(X,Y)), genNumeros(5,X).        
make_table(RowNumbers,ColumnNumbers):-
    member(Row,RowNumbers),
    member(Column,ColumnNumbers),
    Product is Row*Column,
    assertz(product(Row,Column,Product)),
    fail.

map(_,[],[]).
map(F,[X|T],[call(A)|R]) :- A =.. [F|[X]], map(F,T,R).

filter(_,[],[]).
filter(P,[X|T],[X|R]) :- C =.. [P,X], call(C), !, filter(P,T,R).
filter(P,[_|T],L) :- filter(P,T,L).