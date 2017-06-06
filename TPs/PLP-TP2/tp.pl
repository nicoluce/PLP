%% Trabajo Practico 2 - PLP, DC UBA
%% 'Los Militantes de Andi'

%% Auxiliares
	%prefijo(+L, ?P)
	prefijo(P, L) :- append(P, _, L).

	%sufijo(+L, ?S)
	sufijo(S, L) :- append(_, S, L).

	%sublista(+L, SL)
	sublista(SL, L) :- sufijo(S, L),  prefijo(SL, S).

	%% borrar(X, L, LsinX)
	borrar(X, L, LsinX) :- append(L1, [X|L2], L), append(L1, L2, LsinX).

	%long(?N, +L, ?X)
	nesimo(0, [ X |_], X).
	nesimo(suc(N), [ _ |L], X) :- nesimo(N, L, X).

	%% borrarLista(+L1, ?L2)
	borrarLista([], L2, L2).
	borrarLista([X|L1], [X|L2], L) :- borrarLista(L1, L2, L).
	borrarLista([X|L1], [Y|L2], L) :- X \= Y, borrarLista(L1, [Y|L2], L).

	%% mochilaDeCElem(+C, ?M)
	mochilaDeCElem(0, []).
	mochilaDeCElem(C, [X|M]) :- C > 0, D is C - 1, mochilaDeCElem(D, M), herramienta(X,_).


%% Herramientas basicas
	herramienta(rayo, 10).
	herramienta(volatilizador, 40).
	herramienta(encendedor, 5).

%% Funciones
	%% composicion(+Composicion, ?Potencial, ?Costo)
	composicion(X, P, 1) :- herramienta(X, P).
	composicion(binaria(X, Y), P, 5) :- herramienta(X, PX), herramienta(Y, PY), P is 2*PX+PY.

%% Ejercicio 1
	composicion(jerarquica(X, Y), P, C) :- composicion(X, PX, CX), composicion(Y, PY, CY), P is PX*PY, C is 2*(CX+CY).

	%% Ejemplos:
		%% composicion(jerarquica(jerarquica(binaria(rayo, volatilizador), rayo), rayo), X, Y).
			%% X = 6000,
			%% Y = 26.

		%% composicion(jerarquica(rayo, jerarquica(rayo, binaria(rayo, volatilizador))), X, Y).
			%% X = 6000,
			%% Y = 26.


%% Ejercicio 2
	%% configuracion(+M, ?Conf, ?P, ?C)
	configuracion([X], X, P, 1) :- herramienta(X, P).
	configuracion([X, Y], binaria(X, Y), P, C) :- composicion(binaria(X, Y), P, C).
	configuracion(L, jerarquica(X, ConfL), P, C) :- sort(L, LsinRepetidos), nesimo(_, LsinRepetidos, X), 
														%% Elijo un elemnto de LsinRepetidos como X 
														%% (ya que para la mochila [rayo_1, rayo_2, rayo_3, volatilizador] es indistinto que rayo es X) 
													setof(Ls, borrar(X, L, Ls), Lss), nesimo(0, Lss, LsinX),
														%% y tomo la submochila resultante de borrar la 1ra aparicion de X 
														%% (como el orden de los elementos no importa las mochilas [rayo, volatilizador] y [volatilizador, rayo] son indistintas).

													herramienta(X, PX), setof((PL, CL), configuracion(LsinX, ConfL, PL, CL), L2),
														%% Como lo unico que necesito de ConfL es su potencia y costo, uso setof() para filtrar soluciones repetidas.
													member((PL, CL), L2), P is PL*PX, C is 2*(CL+1).
														%% Para cada configuracion con par (PL, CL) distinto, calculo P y C como especifica el enunciado.


	%% Ejemplos:
		%% configuracion([rayo,rayo],Conf,P,C).
			%% Conf = binaria(rayo, rayo),
			%% P = 30,
			%% C = 5 ;
			%% Conf = jerarquica(rayo, rayo),
			%% P = 100,
			%% C = 4 ;

		%% configuracion([rayo,volatilizador],Conf,P,C).
			%% Conf = binaria(rayo, volatilizador),
			%% P = 60,
			%% C = 5 ;
			%% Conf = jerarquica(rayo, volatilizador),
			%% P = 400,
			%% C = 4 ;
			%% Conf = jerarquica(volatilizador, rayo),
			%% P = 400,
			%% C = 4 ;

		%% configuracion([rayo,volatilizador,rayo],Conf,P,C).
			%% Conf = jerarquica(rayo, binaria(rayo, volatilizador)),
			%% P = 600,
			%% C = 12 ;
			%% Conf = jerarquica(rayo, jerarquica(rayo, volatilizador)),
			%% P = 4000,
			%% C = 10 ;
			%% Conf = jerarquica(rayo, jerarquica(volatilizador, rayo)),
			%% P = 4000,
			%% C = 10 ;
			%% Conf = jerarquica(volatilizador, binaria(rayo, rayo)),
			%% P = 1200,
			%% C = 12 ;
			%% Conf = jerarquica(volatilizador, jerarquica(rayo, rayo)),
			%% P = 4000,
			%% C = 10 ;

%% Ejercicio 3
	%% masPoderosa(+M1, +M2)
	masPoderosa(M1, M2) :- 	configuracion(M1, _, P1, _), configuracion(M2, Conf2, P2, _), P1 > P2, 
								%% Dada una configuracion de M1 con potencia P1, busco una configuracion de M2, tal que su potencia sea menor a P1,
							not((configuracion(M2, Conf3, P3, _), Conf3 \= Conf2, P3 >= P1)).
								%% y que no exista otra configuracion de M2, distinta a la ya encontrada, tal que su potencia sea mayor o igual a P1.
	%% Ejemplos:
		%% masPoderosa([rayo, volatilizador], [volatilizador, rayo]).
			%% false.
		%% masPoderosa([rayo, rayo], [rayo, volatilizador]).
			%% false.
		%% masPoderosa([rayo, volatilizador], [rayo, rayo]).
			%% true.
	
%% Ejercicio 4
	%% mejor(+M1,+M2)

%% Ejercicio 5
	%% usar(+M1,+Ps,?Cs,?M2)
	usar(M1, [], [], M1).
	usar(M1, [P|Ps], [C|Cs], M2) :- setof(Ls, sublista(Ls, M1), Lss), member(Ls, Lss), Ls \= [],
										%% Una submochilas distintas de [].
									configuracion(Ls, C, P1, _), P1 >= P,
										%% Busco una configuracion valida apartir de la submochila, tal que su potencia sea mayor o igual a P. 
									borrarLista(Ls, M1, M3), usar(M3, Ps, Cs, M2).
										%% Saco los elementos de la submochila de la mochila original y hago recursion sobre el resultado.

%% Ejercicio 6
	%% comprar(+P,+C,?M)
	comprar(P, C, M) :- between(1, C, N), mochilaDeCElem(N, M),
							%% Construyo todas las mochilas con a lo sumo C elementos.
						findall(M, (configuracion(M, _, P1, _), P1 >= P), Ls), 	
							%% Construyo una lista con el elemento M, 
							%% donde la cantidad de apariciones será dada 
							%% por la cantidad de configuraciones distintas que cumplan que P1 >= P. (Generate & Test en una consulta)

							%% Esto es para evitar soluciones repetidas, ya que para el caso comprar(100, 4, M)
							%% M = [rayo, rayo] resulta de dos configuraciones distintas, 
							%% jerarquica(rayo_1, rayo_2) y jerarquica(rayo_2, rayo_1).
						nesimo(0, Ls, M).

