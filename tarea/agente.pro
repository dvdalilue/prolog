% :- op(500,xfy,[':']).

% ':'(_,_) :- !.

ui :-
		write('\nPosee los siguiete comandos:\n\n þ Vuelos por fecha       - v\n þ Vuelos directos       - d\n þ Gira      - g\n þ Ida y Vuelta - iv\n þ quit         - q\n\nAgente~® '),
		read_atom(S),
		manager(S),
		!,
		ui.
ui :- !.

manager(S) :-
	S == v,!,
	write('\n\n þ Inserte el origen:\t'),
	read_atom(O),
	write('\n þ Inserte el destino:\t'),
	read_atom(D),
	member(X,[lun,mar,mier,jue,vie,sab,dom]),
	ruta(O,D,X,R),
	write('\n\n\tø Dia:\t') , write(X),write('->\t'),write(R), write('\n'),
	ui.

manager(S) :-
	S == d,!,
	write('\n\n þ Inserte el origen:\t'),
	read_atom(O),
	write('\n þ Inserte el destino:\t'),
	read_atom(D),
	member(X,[lun,mar,mier,jue,vie,sab,dom]),
	ruta(O,D,X,R),
	length(R,T),T==1,
	write('\n\n\tø Dia:\t') , write(X),write('->\t'),write(R), write('\n'),
	ui.

manager(S) :-
	S == g,!,
	write('\n\n þ Inserte las cuidades de la gira en este formato Ciudad1,Ciudad2,... :\t'),
	read_atom(O),
	read_atom(Di),
	gira(O,Di,R),
	write('\n\n\tø\t') ,write(R), write('\n'),
	ui.


manager(S) :-	S \= q, !.


todos(D) :- D == lun, !.
todos(D) :- D == mar, !.
todos(D) :- D == mie, !.
todos(D) :- D == jue, !.
todos(D) :- D == vie, !.
todos(D) :- D == sab, !.
todos(D) :- D == dom, !.

habiles(D) :- D == lun, !.
habiles(D) :- D == mar, !.
habiles(D) :- D == mie, !.
habiles(D) :- D == jue, !.
habiles(D) :- D == vie, !.

ruta(O,DE,DI,[R]) :-
		horario(O,DE,V),
		vuelos(V,DI,Rs),
		member(R,Rs).
ruta(O,DE,DI,[X,Y]) :-
 		horario(O,I,V),
 		vuelos(V,DI,Xs),
		member(X,Xs),
 		horario(I,DE,V2),
		vuelos(V2,DI,Ys),
		member(Y,Ys),
		intermedio(X,I,DI,V2,Y).

vuelos([],_,[]) :- !.
vuelos([X:Y/Z:W/V/D|T],DI,[RH|RT]) :-
		verificarD(DI,D),
		RH = X:Y/Z:W/V/D, !,
		vuelos(T,DI,RT).
vuelos([_|T],DI,R) :-
		!, vuelos(T,DI,R).

intermedio(_:_/H1:M1/_/_,I,_,E,R) :-
		member(Y,E),
		Y = H2:M2/H3:M3/V2/D2,
		transbordo(I,Trs),
		addTime(H1:M1,Trs,Hs:Ms),
		checkTime(Hs:Ms,H2:M2),
		R = H2:M2/H3:M3/V2/D2.

gira(Ciu,Ds,Rs):-
 	member(X,[lun,mar,mier,jue,vie,sab,dom]),
	giras(Ciu,Ds,X,Rs).

giras([_],_,_,[]).
giras([C0,C1|Cs],Ds,Dia,[R|Rs]):-
	ruta(C0,C1,Dia,R),
	addDay(Dia,Ds,Df),
	giras([C1|Cs],Ds,Df,Rs).
	
verificarD(D,L) :-
		\+ atom(L), !,
		member(D,L).
verificarD(D,F) :-
		D == F, !.
verificarD(D,F) :-
		F == todos,
		D == habiles, !.
verificarD(D,F) :-
		V =.. [F,D],
		V, !.

transbordo(new_york,90) :- !.
transbordo(chicago,90) :- !.
transbordo(los_angeles,90) :- !.
transbordo(san_francisco,60) :- !.
transbordo(dallas,60) :- !.
transbordo(miami,60) :- !.
transbordo(_,40) :- !.

checkTime(H1:M1,H1:M2) :- !,M1 =< M2.
checkTime(H1:_,H2:_) :- H1 < H2,!.

addTime(H:M,X,Hf:Mf):-
	M + X < 60,
	Hf is H,
	Mf is M + X,!.
addTime(Hm,X,Hf:Mf):-
	Xs is X - 60,
	!,
	addTime(Hm,Xs,Hs:Mf),
	Hf is Hs + 1.

% addDay(_,0,[]):-!.
% addDay(X,Y,Z):- Y > 7 ,!,addDay(X,7,Z).
% addDay(Dia,X,[Res|R]):-nextDay(Dia,Res),Xn is X-1,addDay(Res,Xn,R).
addDay(D,0,D):-!.
addDay(Dia,X,R):-nextDay(Dia,Res),Xn is X-1,addDay(Res,Xn,R).


nextDay(lun,mar).
nextDay(mar,mie).
nextDay(mie,jue).
nextDay(jue,vie).
nextDay(vie,sab).
nextDay(sab,dom).
nextDay(dom,lun).

/*
  Hechos!!
*/

horario( new_york, chicago,
           [  9:40 / 10:50 / nw4733 / todos,
             13:40 / 14:50 / nw4773 / habiles,
	     22:40 / 23:50 / nw1001 / [lun], 
             19:40 / 20:50 / nw4833 / [lun,mar,mie,jue,vie,dom]
	   ] ). 
horario( chicago, new_york,
           [  9:10 / 10:00 / nw458 / todos,
             12:20 / 13:10 / aa511 / todos ] ). 

horario( chicago, dallas,
           [ 9:40 / 10:50 / aa4732 / todos,
             12:20 / 12:50 / aa4752 / habiles,
	     00:00 / 1:50 / y0l0 / [lun], 
             18:40 / 19:50 / aa4822 / [lun,mar,mie,jue,vie]
	   ] ). 

horario( dallas, los_angeles,
           [ 13:50 / 16:20 / nw212 / [lun,mar,mie,vie,dom],
             16:30 / 19:30 / aa473 / [lun,mie,jue,sab] ] ). 

horario( new_york, washington,
           [  9:10 / 11:45 / united614 / todos,
             14:45 / 17:20 / united805 / todos ] ). 

horario( chicago, miami,
           [  8:30 / 11:20 / nw510 / todos,
             11:00 / 13:50 / aa459 / todos ] ). 


horario( los_angeles, san_francisco,
           [ 11:30 / 12:40 / sw322 / [mar,jue] ] ). 
horario( san_francisco, los_angeles,
           [  9:25 / 10:15 / aa621 / todos,
             12:45 / 13:35 / sw623 / todos ] ). 


horario( san_francisco, seattle,
           [ 11:10 / 12:20 / sw211 / [lun,mar,mie,vie,dom],
             20:30 / 21:30 / nw472 / [lun,mie,jue,sab] ] ). 
horario( seattle, san_francisco,
           [ 7:55 / 8:45 / aa620 / todos,
             11:25 / 12:15 / aa666 / habiles ] ).


horario( dallas, san_francisco,
           [ 13:30 / 14:40 / nw323 / [mar,jue] ] ). 


horario( boston, new_york,
           [ 9:00 / 9:40 / aa613 / [lun,mar,mie,jue,vie,sab],
            16:10 / 16:55 / united806 / [lun,mar,mie,jue,vie,dom] ] ).
