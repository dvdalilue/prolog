:- dynamic([genNumbers/3]).
:- dynamic([genNumeros/2]).
/*
  Pregunta 2 - Verificar que una matriz es
  diabolica.
*/

diabolico(A) :- 
		var(A),
		generator(16,A).
diabolico(A) :-
		\+ var(A),
		filasV(A,16,3,0),
		diagonalP(A,16,0,34),
		diagonalS(A,16,3,0,34),
		diagonalm(A,16,2,0),
		diagonalu(A,16,3,0),
		diagonald(A,16,1,0),
		columnas(A,16),
		!.

stopwatch(Predicate) :-
    real_time(Start),
    call(Predicate),
    real_time(Finish),
    Elapsed is (Finish - Start) / 1000,
    format('~4f seg~N',[Elapsed]), !.

generator(T,G) :-
		genNumbers(T,0,A), !,
		gP(A,G).

gP(A0,[G1,G2,G3,G4,H1,H2,H3,H4,J1,J2,J3,J4,K1,K2,K3,K4]) :-
		select(G1,A0,A1),
		select(G2,A1,A2),
		select(G3,A2,A3),

		17 < G1 + G2 + G3,

		select(H1,A3,A4),
		select(J1,A4,A5),

		17 < G1 + H1 + J1,

		select(G4,A5,A6),

		34 is G1 + G2 + G3 + G4, %%Fila 1

		select(K1,A6,A7),

		34 is G1 + H1 + J1 + K1, %%Columna 1

		select(H2,A7,A8),

		17 < J1 + H2 + G3,
		
		select(H3,A8,A9),

		17 < H1 + H2 + H3,

		select(J2,A9,A10),

		34 is K1 + J2 + H3 + G4, %%Diagonal Pricipal

		select(K4,A10,A11),

		34 is J1 + H2 + G3 + K4, %%Triangulo, puntos: J1, G3, K4

		select(J3,A11,A12),

		34 is G1 + H2 + J3 + K4, %%Diagonal Secundaria

		select(J4,A12,A13),

		34 is K1 + G2 + H3 + J4, %%Triangulo, puntos: K1, G2, J4
		34 is J1 + J2 + J3 + J4, %%Fila 3

		select(K3,A13,A14),

		34 is H1 + J2 + K3 + G4, %%Triangulo, puntos: H1, K3, G4
		34 is G3 + H3 + J3 + K3, %%Columna 3
		34 is H1 + G2 + K3 + J4, %%Cuadrado, puntos: H1, G2, K3, J4

		select(K2,A14,A15),

		34 is K1 + K2 + K3 + K4, %%Fila 4
		34 is G2 + H2 + J2 + K2, %%Columna 2

		select(H4,A15,_),

		34 is J1 + K2 + G3 + H4, %%Cuadrado, puntos: J1, K2, G3, H4
		34 is H1 + H2 + H3 + H4, %%Fila 2
		34 is G4 + H4 + J4 + K4, %%Columna 4
		34 is G1 + K2 + J3 + H4. %%Triangulo, puntos: G1, K2, H4

filasV([H|T],NN,I,B) :-
		NN > 0,
		I > 0,
		P is B + H,
		N is NN - 1,
		M is I - 1,
		!,
		filasV(T,N,M,P).
filasV([H|T],NN,0,B) :-
		NN > 0,
		34 is B + H,
		N is NN - 1,
		!,
		filasV(T,N,3,0).
filasV(T,0,_,_) :-
		T = [], !.

diagonalP([H|T],NN,0,B) :-
		NN > 0,
		N is B - H,
		M is NN - 1,
		!,
		diagonalP(T,M,4,N).
diagonalP([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonalP(T,M,N,B).
diagonalP(T,0,_,B)      :- B is 0, T = [], !.

diagonalS([_|T],NN,I,B,X) :-
		NN > 0, /*Creo poder quitar esto, si pongo de primero el predicado base*/
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonalS(T,M,N,B,X).
diagonalS([_|T],1,_,B,X)  :- T = [], X is B, !.
diagonalS([H|T],NN,0,B,X) :-
		NN > 0,
		N is H + B,
		M is NN - 1,
		!,
		diagonalS(T,M,2,N,X).

diagonalm([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonalm(T,M,N,B).
diagonalm([H|[NH|T]],9,0,B) :-
		N is H + NH + B,
		!,
		diagonalm(T,7,4,N).
diagonalm([H|T],NN,0,B) :-
		NN > 0,
		N is H + B,
		M is NN - 1,
		!,
		diagonalm(T,M,4,N).
diagonalm([],_,_,B) :-
		B is 34, !.

diagonalu([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonalu(T,M,N,B).
diagonalu([H|[NH|T]],13,0,B) :-
		N is H + NH + B,
		!,
		diagonalu(T,11,4,N).
diagonalu([H|T],NN,0,B) :-
		NN > 0,
		N is H + B,
		M is NN - 1,
		!,
		diagonalu(T,M,4,N).
diagonalu([],_,_,B) :-
		B is 34, !.

diagonald([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is I - 1,
		M is NN - 1,
		!,
		diagonald(T,M,N,B).
diagonald([H|[NH|T]],5,0,B) :-
		N is H + NH + B,
		!,
		diagonald(T,3,4,N).
diagonald([H|T],NN,0,B) :-
		NN > 0,
		N is H + B,
		M is NN - 1,
		!,
		diagonald(T,M,4,N).
diagonald([],_,_,B) :-
		B is 34, !.

columnas(A,T) :- columna(A,T,0,0), columna(A,T,1,0), columna(A,T,2,0), columna(A,T,3,0), !.

columna([_|T],NN,I,B) :-
		NN > 0,
		I > 0,
		N is NN - 1,
		M is I - 1,
		!,
		columna(T,N,M,B).
columna([H|T],NN,0,B) :-
		NN > 0,
		M is B + H,
		N is NN - 1,
		!,
		columna(T,N,3,M).
columna(T,0,_,B) :-
		B is 34,
		T = [],
		!.

/*
  Generador de una lista de numeros, tantos
  como se le haya indicado.
*/
genNumbers(0,_,R)     :- R = [], !.
genNumbers(T,V,[A|R]) :-
		N is T - 1,
		M is V + 1,
		genNumbers(N,M,R),
		A is M,
		asserta((genNumbers(T,V,[A|R]) :- !)).

genNumeros(0,[]) :- !.
genNumeros(N,[H|T]) :-
		N > 0,
		NN is N - 1,
		H = N,
		genNumeros(NN,T),
		asserta((genNumeros(N,[H|T]) :- !)).
