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

	%% cantApariciones(+X, +L, ?N)
	cantApariciones(_, [], 0).
	cantApariciones(X, [X|L], M) :- cantApariciones(X, L, N), M is N +1.
	cantApariciones(X, [Y|L], M) :- X \= Y, cantApariciones(X, L, M).

	%% compAHerramientas(+Cs, ?Hs)
	compAHerramientas([], []).
	compAHerramientas([X|Cs], [X|Hs]) :- herramienta(X, _), compAHerramientas(Cs, Hs).
	compAHerramientas([binaria(X, Y)|Cs], Hs) :- compAHerramientas([X, Y|Cs], Hs).
	compAHerramientas([jerarquica(X,Y)|Cs], Hs) :- compAHerramientas([X, Y|Cs], Hs).


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
														%% Elegimos un elemnto de LsinRepetidos como X 
														%% (ya que para la mochila [rayo_1, rayo_2, rayo_3, 
														%% volatilizador] es indistinto que rayo es X) 

													setof(Ls, borrar(X, L, Ls), Lss), nesimo(0, Lss, LsinX),
														%% y tomamos la submochila resultante de borrar la 1ra aparicion de X 
														%% (como el orden de los elementos no importa las mochilas 
														%% [rayo, volatilizador] y [volatilizador, rayo] son indistintas).

													herramienta(X, PX), setof((PL, CL), configuracion(LsinX, ConfL, PL, CL), L2),
														%% Como lo unico que necesitamos de ConfL es su potencia y costo,
														%% usamos setof() para filtrar soluciones repetidas.

													member((PL, CL), L2), P is PL*PX, C is 2*(CL+1).
														%% Para cada configuracion con par (PL, CL) distinto, 
														%% calculamos P y C como especifica el enunciado.
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
	masPoderosa(M1, M2) :- 	configuracion(M1, _, P1, _), not((configuracion(M2, _, P2, _), P2 >= P1)).
								%% M1 es mas poderosa que M2 cuando dada una configuracion de M1 con potencia P1, 
								%% no exista una configuracion de M2, tal que su potencia sea mayor o igual a P1.
	%% Ejemplos:
		%% masPoderosa([rayo, volatilizador], [volatilizador, rayo]).
			%% false.
		%% masPoderosa([rayo, rayo], [rayo, volatilizador]).
			%% false.
		%% masPoderosa([rayo, volatilizador], [rayo, rayo]).
			%% true.
	
%% Ejercicio 4
	%% mejor(+M1,+M2)
	mejor(M1, M2) :- not((configuracion(M2, _, PM2, CM2), ((configuracion(M1, _, PM1, CM1), PM2 >= PM1, CM2 < CM1); 
					not(configuracion(M1, _, PM1, CM1))))).

%% Ejercicio 5
	%% usar(+M1,+Ps,?Cs,?M2)
	usar(M1, [], [], M1). 
		%% De esta forma nos aseguramos que no hayan elementos en M2 que nunca estuvieron en M1.

	usar(M1, [P|Ps], [C|Cs], M2) :- setof(Ls, sublista(Ls, M1), Lss), member(Ls, Lss), Ls \= [],
										%% Dada una submochilas distintas de [],

									configuracion(Ls, C, P1, _), P1 >= P,
										%% buscamos una configuracion valida apartir de la submochila, 
										%% tal que su potencia sea mayor o igual a P.
									
									borrarLista(Ls, M1, M3), usar(M3, Ps, Cs, M2),
										%% Sacamos los elementos de la submochila de la mochila original 
										%% y que exista un uso valido de la mochila resultante para las colas de las listas Ps y Cs.

									compAHerramientas([C|Cs], Usados), borrarLista(Usados, M1, SinUsar),
										%% Una vez que Cs fue instanciada correctamente, construimos la lista de herramientas sin usar (SinUsar) 
										%% en base a las herramientas usadas (Cs),
									
									not((nesimo(_, SinUsar, H), cantApariciones(H, SinUsar, N), cantApariciones(H, M2, M), N < M)).
										%% y confirmamos que SinUsar es igual a M2.
	%% Ejemplos:
		%% usar([rayo,rayo,rayo,rayo],[100],C,[rayo,rayo]).
			%% C = [jerarquica(rayo, rayo)] ;

		%% usar([rayo,rayo,rayo,rayo],[10,10],C,[rayo,rayo]).
			%% C = [rayo, rayo] ;

		%% usar([rayo,rayo,volatilizador,rayo,encendedor],[30,80],C,[encendedor]).
			%% [...]
			%% C = [binaria(rayo, rayo), jerarquica(rayo, volatilizador)] ;
			%% C = [binaria(rayo, rayo), binaria(volatilizador, rayo)] ;
			%% C = [jerarquica(rayo, rayo), jerarquica(rayo, volatilizador)] ;
			%% [...]

%% Ejercicio 6
	%% comprar(+P,+C,?M)
	comprar(P, C, M) :- between(1, C, N), mochilaDeCElem(N, M),
							%% Construimos todas las mochilas con a lo sumo C elementos.
						
						findall(M, (configuracion(M, _, P1, _), P1 >= P), Ls),
							%% Construimos una lista con el elemento M, 
							%% donde la cantidad de apariciones sera dada 
							%% por la cantidad de configuraciones distintas que cumplan que P1 >= P. 
							%% (Generate & Test en una consulta)

							%% Esto es para evitar soluciones repetidas, ya que para el caso comprar(100, 4, M)
							%% M = [rayo, volatilizador] resulta de dos configuraciones distintas, 
							%% jerarquica(rayo, volatilizador) y jerarquica(volatilizador, rayo).
						
						nesimo(0, Ls, M).