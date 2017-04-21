padre(elias,rafael).
padre(rafael,david).
padre(elias,elba).
padre(elias,juan).

hermano(X,Y) :- padre(Z,X), padre(Z,Y), X \= Y.

abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

mcd(N,N,N).
mcd(A,B,M):- A > B, A1 is A - B, mcd(A1,B,M).
mcd(A,B,M):- B > A, B1 is B - A, mcd(A,B1,M).

fact(0,1).
fact(N,P) :- N > 0, N1 is N-1, fact(N1,P1), P is N * P1.
