%% Trabajo Practico 2 - PLP, DC UBA
%% 'Los Militantes de Andi'

%% Auxiliares
	%prefijo(+L, ?P)
	prefijo(P, L) :- append(P, _, L).

	%sufijo(+L, ?S)
	sufijo(S, L) :- append(_, S, L).

	%sublista(+L, SL)
	sublista(SL, L) :- sufijo(S, L),  prefijo(SL, S).

	%insertar(?X, +L, -LconX)
	insertar(X, L, LconX) :- append(L1, L2, L), append(L1, [X|L2], LconX).

	%long(+L, N)
	long([], 0).
	long([ _ |L], R) :- long(L, M), R is M+1.

	%% borrar(X, L, LsinX)
	borrar(X, L, LsinX) :- append(L1, [X|L2], L), append(L1, L2, LsinX).

	%long(?N, +L, ?X)
	nesimo(0, [ X |_], X).
	nesimo(suc(N), [ _ |L], X) :- nesimo(N, L, X).

	borrarLista([], L2, L2).
	borrarLista([X|L1], [X|L2], L) :- borrarLista(L1, L2, L).
	borrarLista([X|L1], [Y|L2], L) :- X \= Y, borrarLista(L1, [Y|L2], L).

%% Herramientas basicas
	herramienta(rayo, 10).
	herramienta(volatilizador, 40).
	herramienta(encendedor, 5).

%% Funciones
	%% composicion(+Composicion, ?Potencial, ?Costo)
	composicion(X, P, 1) :- herramienta(X, P).
	composicion(binaria(X, Y), P, 5)  :- herramienta(X, PX), herramienta(Y, PY), P is 2*PX+PY.

%% Ejercicio 1
	composicion(jerarquica(X, Y), P, C) :- composicion(X, PX, CX), composicion(Y, PY, CY), P is PX*PY, C is 2*(CX+CY).

%% Ejercicio 2
	%% configuracion(+M, ?Conf, ?P, ?C)
	configuracion([X], X, P, 1) :- herramienta(X, P).
	configuracion([X, Y], binaria(X, Y), P, C) :- composicion(binaria(X, Y), P, C).
	%% configuracion(L, jerarquica(X, ConfL), P, C) :- nesimo(_, L, X), setof(Ls, borrar(X, L, Ls), Lss), member(LsinX, Lss), herramienta(X, PX), configuracion(LsinX, ConfL, PL, CL), P is PL*PX, C is 2*(CL+1).
	configuracion(L, jerarquica(X, ConfL), P, C) :- nesimo(_, L, X), setof(Ls, borrar(X, L, Ls), Lss), nesimo(0, Lss, LsinX), herramienta(X, PX), setof((PL, CL), configuracion(LsinX, ConfL, PL, CL), L2), member((PL, CL), L2), P is PL*PX, C is 2*(CL+1).

%% Ejercicio 3
	%% masPoderosa(+M1, +M2)
	masPoderosa(M1, M2) :- configuracion(M1,Conf,P1,C), configuracion(M2,Conf2,P2,C2), P1 > P2, not((configuracion(M2,Conf3,P3,C3), Conf3\=Conf2, P3 >= P1)).
	
%% Ejercicio 4
	%% mejor(+M1,+M2)

%% Ejercicio 5
	%% usar(+M1,+Ps,?Cs,?M2)
	usar(M1, [], [], M1).
	usar(M1, [P|Ps], [C|Cs], M2) :- setof(Ls, sublista(Ls, M1), Lss), member(Ls, Lss), Ls \= [], configuracion(Ls, C, P1, _), P1 >= P,
									borrarLista(Ls, M1, M3), usar(M3, Ps, Cs, M2).

%% Ejercicio 6
	alSumoCElem(0, []).
	alSumoCElem(C, [X|M]) :- C > 0, D is C - 1, alSumoCElem(D, M), herramienta(X,_).

	%% comprar(+P,+C,?M)
	comprar(P, C, M) :- alSumoCElem(C, M), configuracion(M, _, P1, _), P >= P1.
