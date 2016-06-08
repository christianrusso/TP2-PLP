% ####################################
% Calentando motores
% ####################################

% Por compatibilidad, no se usan tildes, circunflejos arriba de las n, ni otras cosas raras como dieresis arriba de las vocales.

%%% Ejercicio 1

% listaNats(+LInf,+LSup,?Nats)
% Unifica la lista Nats con los naturales en el rango [LInf, LSup], o una lista vacia si LSup < LInf.
listaNats(I,S,[]):- S < I.
listaNats(I,S,[I | T]):- S >= I, IMas1 is I + 1, listaNats(IMas1, S, T).


%%% Ejercicio 2

% nPiezasDeCada(+Cant, +Tamaños, -Piezas)
% Instancia a Piezas con una lista que contiene una cantidad Cant de cada tamanio en la lista Tamanios.
nPiezasDeCada(_,[],[]).
nPiezasDeCada(C,[HT | T],[pieza(HT, C) | P]) :- nPiezasDeCada(C,T,P).

%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas)
% Permite instanciar Piezas con la lista de piezas incluidas en SecPiezas. 
resumenPiezas([], []).
resumenPiezas([T | TT], [pieza(T, CTm1) | TP]) :- apariciones(T, TT, CT),
                                                  CTm1 is CT + 1, 
                                                  remover(T, TT, TTSinT),
                                                  resumenPiezas(TTSinT, TP).

% apariciones(+Elemento, +Lista, -Cantidad)
% Cantidad es la cantidad de apariciones de Elemento en Lista.
apariciones(_, [], 0).
apariciones(E, [E | T], N) :- apariciones(E, T, NMen1), N is NMen1 + 1. 
apariciones(E, [X | T], N) :- X \= E, apariciones(E, T, N). 

% remover(+Elemento, +Lista, -ListaSinElemento)
% ListaSinElemento es Lista sin los elementos iguales a Elemento.
remover(_,[],[]).
remover(N, [N | T], L) :- remover(N, T, L). 
remover(N, [X | T], [X | TL]) :- N \= X, remover(N, T, TL). 

% ####################################
% Enfoque naive
% ####################################

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solucion)
% Solucion representa una lista de tamanios de piezas cuyos valores suman Total. Aqui no se pide
% controlar que la cantidad de cada pieza este acorde con la disponibilidad.
generar(0, _, []).
generar(Total, [pieza(Tam, Cant) | P], [Tam | TSol]):- Total >= Tam,
                                                       Tam > 0,
                                                       TotalNuevo is Total - Tam,
                                                       generar(TotalNuevo, [pieza(Tam, Cant) | P], TSol).
generar(Total, [_ | P], Sol):- Total > 0, generar(Total, P, Sol).

% sum(+Lista, ?Suma).
% Funcion extrania que nunca nadie ha entendido que hace.
sum([], 0).
sum([H | T], S) :- sum(T, ST), S is H + ST.

%%% Ejercicio 5 

% cumpleLimite(+Piezas,+Solucion)
% Sera verdadero cuando la cantidad de piezas utilizadas en Solucion no exceda 
% las cantidades disponibles indicadas en Piezas.

cumpleLimite(_,[]).
cumpleLimite([pieza(Tam, Cant) | TP], Sol) :- apariciones(Tam, Sol, NTam),
                                              NTam =< Cant,
                                              remover(Tam, Sol, SolSinTam),
                                              cumpleLimite(TP, SolSinTam).


%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solucion)
% Solucion representa una lista de piezas cuyos valores suman Total y, ademas, las cantidades utilizadas 
% de cada pieza no exceden los declarados en Piezas.

construir1(Total, Piezas, Sol) :- setof(SolAux, construir1Aux(Total, Piezas, SolAux), Set), dameUno(Set, Sol).

construir1Aux(Total, Piezas, Sol) :- generar(Total, Piezas, Sol1), cumpleLimite(Piezas, Sol1), permutacion(Sol1, Sol).

dameUno([X | _], X).
dameUno([_ | Xs], Y) :- dameUno(Xs, Y).

% removerUna(+Elemento, +Lista, -SinUna) o removerUna(+Elemento, -Lista, +SinUna)
% SinUna es el resultado de sacarle a Lista la primera aparicion de Elemento.
removerUna(X, [X | T], T).
removerUna(X, [H | T], [H | TT]) :- X \= H, removerUna(X, T, TT). 

% permutacion(+Lista, -Permutacion)
% Permutacion es una permutacion de Lista. Genera todas las permutaciones.
permutacion([], []).
permutacion([H | T], L) :- permutacion(T, LT), removerUna(H, L, LT).

% ####################################
% Enfoque dinamico
% ####################################

%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución)
% Su comportamiento es identico a construir1/3 pero utiliza definiciones dinamicas para persistir los calculos 
% auxiliares realizados y evitar repetirlos. No se espera que las soluciones aparezcan en el mismo orden entre 
% construir1/3 y construir2/3, pero si, sean las mismas.

:- dynamic dp/3.
cleanup_dp_now_please :- retractall(dp(_, _, _)).

construir2(Total, Piezas, Sol) :- cleanup_dp_now_please, construir2Aux(Total, Piezas, SolAux), permutacion(SolAux, Sol).

construir2Aux(T, PS, SS) :- dp(T, PS, SS), !.
construir2Aux(0, _, []).
construir2Aux(Total, [pieza(Tam, Cant) | TP], [Tam | TSol]) :- Total >= Tam,
                                                    Tam > 0,
                                                    Cant > 0,
                                                    NuevaCant is Cant - 1,
                                                    NuevoTotal is Total - Tam,
                                                    construir2Aux(NuevoTotal, [pieza(Tam, NuevaCant) | TP], TSol),
                                                    asserta(dp(Total, [pieza(Tam, Cant) | TP], [Tam | TSol])).
construir2Aux(Total, [_ | TP], Sol) :- Total > 0, construir2Aux(Total, TP, Sol).

% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N)
% Soluciones representa una lista con todas las soluciones de longitud Total obtenidas con construir1/3, 
% y N indica la cantidad de soluciones totales.

todosConstruir1(Tot, Piezas, Sol, N) :- setof(M, construir1(Tot, Piezas, M), Sol), length(Sol, N).

%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N)
% Soluciones representa una lista con todas las soluciones de longitud Total obtenidas con construir2/3,
% y N indica la cantidad de soluciones totales.

todosConstruir2(Tot, Piezas, Sol, N) :- setof(M, construir2(Tot, Piezas, M), Sol), length(Sol, N).


% ####################################
% Patrones
% ####################################

%%% Ejercicio 10

% construirConPatron(+Total, +Piezas, ?Patrón, -Solución) será verdadero cuando Solución sea una solución factible 
%  en los términos definidos anteriormente y, además, sus piezas respeten el patrón indicado en Patrón. 
%  Se sugiere definir un predicado tienePatrón(+Lista, ?Patrón) que decida si Lista presenta el Patrón especificado.

tienePatron(Patron, Solucion) :- length(Patron, N1), 
                                    length(Solucion, N2),
                                    N1 =< N2,
                                    0 =:= mod(N2, N1),
                                    Times is div(N2, N1),
                                    repetir(Patron, Times, PatronMismaLong),
                                    PatronMismaLong = Solucion.

repetir(_, 0, []).
repetir(Part, Times, L) :- Times > 0,
                       TimesMenos1 is Times - 1,
                       append(Part, L1, L),
                       repetir(Part, TimesMenos1, L1).



construirConPatron(Total, Piezas, Patron, Solucion):- construir2(Total, Piezas, Solucion), tienePatron(Patron, Solucion).

% ####################################
% Tests
% ####################################

% Aclaracion: para probar los tests, corremos el predicado y chequeamos a mano si da lo que nosotros esperabamos.

% Utilizamos constructorEjemplo para construir ejemplos de forma prolija.

constructorEjemplo(a,[pieza(1,5), pieza(2,4), pieza(3,2), pieza(4,1)]).
constructorEjemplo(b,[pieza(1,1), pieza(2,1), pieza(3,1), pieza(4,1)]).
constructorEjemplo(c,[pieza(1,100), pieza(2,1), pieza(3,1), pieza(4,1)]).
constructorEjemplo(d,[pieza(10,100), pieza(20,100), pieza(30,100), pieza(40,100)]).
constructorEjemplo(e, []).
constructorEjemplo(f, [pieza(1,5)]).
constructorEjemplo(g, Piezas) :- listaNats(1, 4, S), nPiezasDeCada(10, S, Piezas).
constructorEjemplo(h, Piezas) :- listaNats( 4, 9, S), nPiezasDeCada(7, S, Piezas).
constructorEjemplo(i, Piezas) :- constructorEjemplo(d, Piezas1), constructorEjemplo(e, Piezas2), append(Piezas1,Piezas2,Piezas).
constructorEjemplo(k, [pieza(1, 100000), pieza(5, 100000), pieza(25, 100000)]).

% Ejericicio 1

%listaNats( 1, 3, S).     -> S = [1, 2, 3]
%listaNats( 0, 3, S).     -> S = [0, 1, 2, 3]
%listaNats( 0, 0, S).     -> S = [0] 
%listaNats( 1, 30, S).    -> S = [1, 2, 3, 4, 5, 6, 7, 8, 9|...] 
%listaNats( 1, 300, S).   -> S = [1, 2, 3, 4, 5, 6, 7, 8, 9|...]
%listaNats( 1, 3000, S).  -> S = [1, 2, 3, 4, 5, 6, 7, 8, 9|...]


% Ejericicio 2

% listaNats( 1, 3, S), nPiezasDeCada(10, S, Sol).     -> S = [1, 2, 3], Sol = [pieza(1, 10), pieza(2, 10), pieza(3, 10)] 
% listaNats( 1, 3, S), nPiezasDeCada(10, S, Sol).     -> S = [1, 2, 3], Sol = [pieza(1, 10), pieza(2, 10), pieza(3, 10)] 
% listaNats( 0, 3, S), nPiezasDeCada(10, S, Sol).     -> S = [0, 1, 2, 3], Sol = [pieza(0, 10), pieza(1, 10), pieza(2, 10), pieza(3, 10)] 
% listaNats( 0, 0, S), nPiezasDeCada(10, S, Sol).     -> S = [0], Sol = [pieza(0, 10)] 
% listaNats( 1, 30, S), nPiezasDeCada(10, S, Sol).    -> S = [1, 2, 3, 4, 5, 6, 7, 8, 9|...], Sol = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10), pieza(5, 10), pieza(6, 10), pieza(7, 10), pieza(8, 10), pieza(..., ...)|...] 
% listaNats( 1, 300, S), nPiezasDeCada(10, S, Sol).   -> S = [1, 2, 3, 4, 5, 6, 7, 8, 9|...], Sol = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10), pieza(5, 10), pieza(6, 10), pieza(7, 10), pieza(8, 10), pieza(..., ...)|...] 
% listaNats( 1, 3000, S), nPiezasDeCada(10, S, Sol).  -> S = [1, 2, 3, 4, 5, 6, 7, 8, 9|...], Sol = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10), pieza(5, 10), pieza(6, 10), pieza(7, 10), pieza(8, 10), pieza(..., ...)|...] 

% Ejericicio 3
% Notar que lo que imprime en la consola es igual en los ultimos 3 casos pero no se espera que sean iguales.

%listaNats( 1, 3, S), resumenPiezas(S,Sol).     -> S = [1, 2, 3], Sol = [pieza(1, 1), pieza(2, 1), pieza(3, 1)] ;
%listaNats( 0, 3, S), resumenPiezas(S,Sol).     -> S = [0, 1, 2, 3], Sol = [pieza(0, 1), pieza(1, 1), pieza(2, 1), pieza(3, 1)] ;
%listaNats( 0, 0, S), resumenPiezas(S,Sol).     -> S = [0], Sol = [pieza(0, 1)] .
%listaNats( 1, 30, S), resumenPiezas(S,Sol).    -> S = [1, 2, 3, 4, 5, 6, 7, 8, 9|...], Sol = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1), pieza(5, 1), pieza(6, 1), pieza(7, 1), pieza(8, 1), pieza(..., ...)|...] ;
%listaNats( 1, 300, S), resumenPiezas(S,Sol).   -> S = [1, 2, 3, 4, 5, 6, 7, 8, 9|...], Sol = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1), pieza(5, 1), pieza(6, 1), pieza(7, 1), pieza(8, 1), pieza(..., ...)|...] ;
%listaNats( 1, 3000, S), resumenPiezas(S,Sol).  -> S = [1, 2, 3, 4, 5, 6, 7, 8, 9|...], Sol = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1), pieza(5, 1), pieza(6, 1), pieza(7, 1), pieza(8, 1), pieza(..., ...)|...] ;

% Ejericicio 4
% Nota: Utlizamos para testear diversos tamaños, pero a la hora de documentar el resultado solo ponemos los test de tamaño chico, dado que la solucion es demasiado grande.

% constructorEjemplo(a,Piezas), generar(2,Piezas,Sol).  -> Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)], Sol = [1, 1] ;
%                                                          Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)], Sol = [2] ;
% constructorEjemplo(b,Piezas), generar(2,Piezas,Sol).  -> Piezas = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [1, 1] ;
%                                                          Piezas = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [2] ;
% constructorEjemplo(c,Piezas), generar(2,Piezas,Sol).  -> Piezas = [pieza(1, 100), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [1, 1] ;
%                                                          Piezas = [pieza(1, 100), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [2] ;
%
% constructorEjemplo(d,Piezas), generar(20,Piezas,Sol). -> Piezas = [pieza(10, 100), pieza(20, 100), pieza(30, 100), pieza(40, 100)], Sol = [10, 10] ;
%                                                          Piezas = [pieza(10, 100), pieza(20, 100), pieza(30, 100), pieza(40, 100)], Sol = [20] ;
% constructorEjemplo(d,Piezas), generar(2,Piezas,Sol).  -> false
% constructorEjemplo(f,Piezas), generar(2,Piezas,Sol).  -> Piezas = [pieza(1, 5)], Sol = [1, 1] ;
% constructorEjemplo(h,Piezas), generar(5,Piezas,Sol).  -> Piezas = [pieza(4, 7), pieza(5, 7), pieza(6, 7), pieza(7, 7), pieza(8, 7), pieza(9, 7)], Sol = [5] ;
% constructorEjemplo(h,Piezas), generar(10,Piezas,Sol). -> Piezas = [pieza(4, 7), pieza(5, 7), pieza(6, 7), pieza(7, 7), pieza(8, 7), pieza(9, 7)], Sol = [4, 6] ;
%                                                          Piezas = [pieza(4, 7), pieza(5, 7), pieza(6, 7), pieza(7, 7), pieza(8, 7), pieza(9, 7)], Sol = [5, 5] ;
% constructorEjemplo(i,Piezas), generar(20,Piezas,Sol). -> false
% constructorEjemplo(i,Piezas), generar(20,Piezas,Sol). -> Piezas = [pieza(10, 100), pieza(20, 100), pieza(30, 100), pieza(40, 100)], Sol = [10, 10] ;
%                                                          Piezas = [pieza(10, 100), pieza(20, 100), pieza(30, 100), pieza(40, 100)], Sol = [20] ;


% Ejericicio 5
% constructorEjemplo(a,Piezas), cumpleLimite(Piezas,[1,1,1,1,2,3,1,1]).       -> NO
% constructorEjemplo(a,Piezas), cumpleLimite(Piezas,[1,1,1,1,2,3,1]).         -> SI
% constructorEjemplo(a,Piezas), cumpleLimite(Piezas,[1,1,1,1,2,3,1,2,2,2,2]). -> NO
% constructorEjemplo(a,Piezas), cumpleLimite(Piezas,[1,1,1,1,2,3,1,2,2,2]).   -> SI
% constructorEjemplo(e, Piezas), cumpleLimite(Piezas, []).                    -> SI
% constructorEjemplo(f, Piezas), cumpleLimite(Piezas, [1, 2]).                -> NO
% constructorEjemplo(f, Piezas), cumpleLimite(Piezas, [1]).                   -> SI

% Ejericicio 6
% Nota: Probamos con los mismos casos que el ejercicio 4, para poder ver la difernecia.
% constructorEjemplo(a,Piezas), construir1(2,Piezas,Sol). -> Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)], Sol = [1, 1] ;
%                                                            Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)], Sol = [2] ;
% constructorEjemplo(b,Piezas), construir1(2,Piezas,Sol). -> Piezas = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [2] ;
% constructorEjemplo(c,Piezas), construir1(2,Piezas,Sol). -> Piezas = [pieza(1, 100), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [1, 1] ;
%                                                            Piezas = [pieza(1, 100), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [2] ;
% constructorEjemplo(d,Piezas), construir1(2,Piezas,Sol). -> false
% constructorEjemplo(e,Piezas), construir1(2,Piezas,Sol). -> false
% constructorEjemplo(f,Piezas), construir1(2,Piezas,Sol). -> Piezas = [pieza(1, 5)], Sol = [1, 1] ;
% constructorEjemplo(g,Piezas), construir1(2,Piezas,Sol). -> Piezas = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10)], Sol = [1, 1] ;
%                                                            Piezas = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10)], Sol = [2] ;
% constructorEjemplo(h,Piezas), construir1(2,Piezas,Sol). -> false
% constructorEjemplo(i,Piezas), construir1(2,Piezas,Sol). -> false

% Ejericicio 7

% constructorEjemplo(a,Piezas), construir2(2,Piezas,Sol). -> Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)], Sol = [1, 1] ;
%                                                        Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)], Sol = [2] ;
% constructorEjemplo(b,Piezas), construir2(2,Piezas,Sol). -> Piezas = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [2] ;
% constructorEjemplo(c,Piezas), construir2(2,Piezas,Sol). -> Piezas = [pieza(1, 100), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [1, 1] ;
%                                                        Piezas = [pieza(1, 100), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [2] ;
% constructorEjemplo(d,Piezas), construir2(2,Piezas,Sol). -> false
% constructorEjemplo(e,Piezas), construir2(2,Piezas,Sol). -> false
% constructorEjemplo(f,Piezas), construir2(2,Piezas,Sol). -> Piezas = [pieza(1, 5)], Sol = [1, 1] ;
% constructorEjemplo(g,Piezas), construir2(2,Piezas,Sol). -> Piezas = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10)], Sol = [1, 1] ;
%                                                            Piezas = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10)], Sol = [2] ;
% constructorEjemplo(h,Piezas), construir2(2,Piezas,Sol). -> false
% constructorEjemplo(i,Piezas), construir2(2,Piezas,Sol). -> false


% Ejericicio 8
% constructorEjemplo(a,Piezas), todosConstruir1(2,Piezas,Sol,N).    -> Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)] Sol = [[1, 1], [2]] N = 2.
% constructorEjemplo(b,Piezas), todosConstruir1(2,Piezas,Sol,N).    -> Piezas = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [[2]], N = 1.
% constructorEjemplo(c,Piezas), todosConstruir1(2,Piezas,Sol,N).    -> Piezas = [pieza(1, 100), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [[1, 1], [2]], N = 2.
% constructorEjemplo(d,Piezas), todosConstruir1(5,Piezas,Sol,N).    -> false
% constructorEjemplo(e,Piezas), todosConstruir1(2000,Piezas,Sol,N). -> false
% constructorEjemplo(f,Piezas), todosConstruir1(5,Piezas,Sol,N).    -> Piezas = [pieza(1, 5)], Sol = [[1, 1, 1, 1, 1]] N = 1.
% constructorEjemplo(g,Piezas), todosConstruir1(2,Piezas,Sol,N).    -> Piezas = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10)], Sol = [[1, 1], [2]], N = 2 ;
% constructorEjemplo(h,Piezas), todosConstruir1(2,Piezas,Sol,N).    -> false
% constructorEjemplo(i,Piezas), todosConstruir1(2,Piezas,Sol,N).    -> false
% constructorEjemplo(i,Piezas), todosConstruir1(20,Piezas,Sol,N).   -> Piezas = [pieza(10, 100), pieza(20, 100), pieza(30, 100), pieza(40, 100)], Sol = [[10, 10], [20]], N = 2.

% Ejericicio 9
% constructorEjemplo(a,Piezas), todosConstruir2(2,Piezas,Sol,N).    -> Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)] Sol = [[1, 1], [2]] N = 2.
% constructorEjemplo(b,Piezas), todosConstruir2(2,Piezas,Sol,N).    -> Piezas = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [[2]], N = 1.
% constructorEjemplo(c,Piezas), todosConstruir2(2,Piezas,Sol,N).    -> Piezas = [pieza(1, 100), pieza(2, 1), pieza(3, 1), pieza(4, 1)], Sol = [[1, 1], [2]], N = 2.
% constructorEjemplo(d,Piezas), todosConstruir2(5,Piezas,Sol,N).    -> false
% constructorEjemplo(e,Piezas), todosConstruir2(2000,Piezas,Sol,N). -> false
% constructorEjemplo(f,Piezas), todosConstruir2(5,Piezas,Sol,N).    -> Piezas = [pieza(1, 5)], Sol = [[1, 1, 1, 1, 1]] N = 1.
% constructorEjemplo(g,Piezas), todosConstruir2(2,Piezas,Sol,N).    -> Piezas = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10)], Sol = [[1, 1], [2]], N = 2 ;
% constructorEjemplo(h,Piezas), todosConstruir2(2,Piezas,Sol,N).    -> false
% constructorEjemplo(i,Piezas), todosConstruir2(2,Piezas,Sol,N).    -> false
% constructorEjemplo(i,Piezas), todosConstruir2(20,Piezas,Sol,N).   -> Piezas = [pieza(10, 100), pieza(20, 100), pieza(30, 100), pieza(40, 100)], Sol = [[10, 10], [20]], N = 2.

% Ejericicio 10

% tienePatron([HPat, T], [5,5,5,5]).        SI
% tienePatron([HPat, T], [5,5,5,5,2,3,4]).  NO
% tienePatron([HPat, T], []).               SI
% tienePatron([HPat, T], [5,5,5,1]).        NO
% tienePatron([HPat, T], [5]).              NO
% tienePatron([HPat, T], [5,5,1]).          NO
% tienePatron([HPat, T], [2,1,2,1]).        SI
% tienePatron([HPat, T], [2,1,4,5]).        NO
% tienePatron([HPat, 5], [6,5,6,5]).        SI
% tienePatron([HPat, 5], [2,5,6,5]).        NO
% tienePatron([HPat, 5], [2,4,6,5]).        NO
% tienePatron([2, 5], [2,5,2,4]).           NO
% tienePatron([2, 5], [2,5,2,5]).           SI
% tienePatron([2, 5, HPat, T, W], [2,5,2,4,3,2,5,2,4,3]).   SI
% tienePatron([2, 5, HPat, T, W], [2,5,2,4,3,2,2,5,2,4,3]). NO

% constructorEjemplo(a,Piezas), construirConPatron(10, Piezas, [HPat, T], S).   -> Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)], HPat = 2, T = 3, S = [2, 3, 2, 3] ;
%                                                                                  Piezas = [pieza(1, 5), pieza(2, 4), pieza(3, 2), pieza(4, 1)], HPat = 3, T = 2, S = [3, 2, 3, 2] ;
% constructorEjemplo(b,Piezas), construirConPatron(1, Piezas, [1, 1], S).       -> false
% constructorEjemplo(b,Piezas), construirConPatron(1, Piezas, [1], S).          -> Piezas = [pieza(1, 1), pieza(2, 1), pieza(3, 1), pieza(4, 1)], S = [1] ;
% constructorEjemplo(d,Piezas), construirConPatron(30, Piezas, [HPat, T], S).   -> Piezas = [pieza(10, 100), pieza(20, 100), pieza(30, 100), pieza(40, 100)], HPat = 10, T = 20, S = [10, 20] ;
%                                                                                  Piezas = [pieza(10, 100), pieza(20, 100), pieza(30, 100), pieza(40, 100)], HPat = 20, T = 10, S = [20, 10] ;
% constructorEjemplo(d,Piezas), construirConPatron(30, Piezas, [10,10,10], S).  -> Piezas = [pieza(10, 100), pieza(20, 100), pieza(30, 100), pieza(40, 100)], S = [10, 10, 10] ;
% constructorEjemplo(d,Piezas), construirConPatron(30, Piezas, [10,10,1], S).   -> false
% constructorEjemplo(h,Piezas), construirConPatron(10, Piezas, [HPat, T], S).   -> Piezas = [pieza(4, 7), pieza(5, 7), pieza(6, 7), pieza(7, 7), pieza(8, 7), pieza(9, 7)], HPat = 4, T = 6, S = [4, 6] ;
%                                                                                  Piezas = [pieza(4, 7), pieza(5, 7), pieza(6, 7), pieza(7, 7), pieza(8, 7), pieza(9, 7)], HPat = T, T = 5, S = [5, 5] ;
%                                                                                  Piezas = [pieza(4, 7), pieza(5, 7), pieza(6, 7), pieza(7, 7), pieza(8, 7), pieza(9, 7)], HPat = 6, T = 4, S = [6, 4] ;
% constructorEjemplo(h,Piezas), construirConPatron(10, Piezas, [HPat, T,T], S). -> false
% constructorEjemplo(g,Piezas), construirConPatron(5,Piezas,[1,1,1,1,1],S).     -> Piezas = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10)], S = [1, 1, 1, 1, 1] 
% constructorEjemplo(g,Piezas), construirConPatron(15,Piezas,[2,3],S).          -> Piezas = [pieza(1, 10), pieza(2, 10), pieza(3, 10), pieza(4, 10)], S = [2, 3, 2, 3, 2, 3] ;


% ####################################
% Tiempos
% ####################################

% test1
% time((constructorEjemplo(a,Piezas), todosConstruir1(20,Piezas,Sol,N))). => 200,259 inferences, 0.126 CPU in 0.139 seconds (91% CPU, 1587946 Lips)
% time((constructorEjemplo(a,Piezas), todosConstruir2(20,Piezas,Sol,N))). => 129,285 inferences, 0.079 CPU in 0.089 seconds (88% CPU, 1643342 Lips)

% test1
% time((constructorEjemplo(b,Piezas), todosConstruir1(20,Piezas,Sol,N))). => 9,998 inferences, 0.002 CPU in 0.012 seconds (18% CPU, 4700517 Lips)
% time((constructorEjemplo(b,Piezas), todosConstruir2(20,Piezas,Sol,N))). => 245 inferences, 0.000 CPU in 0.000 seconds (91% CPU, 1929134 Lips) 

% test2
% time((constructorEjemplo(c,Piezas), todosConstruir1(20,Piezas,Sol,N))). => 53,951 inferences, 0.016 CPU in 0.025 seconds (63% CPU, 3406213 Lips)
% time((constructorEjemplo(c,Piezas), todosConstruir2(20,Piezas,Sol,N))). => 30,690 inferences, 0.010 CPU in 0.019 seconds (54% CPU, 2939655 Lips)

% test3
% time((constructorEjemplo(d,Piezas), todosConstruir1(50,Piezas,Sol,N))). => 7,584 inferences, 0.003 CPU in 0.012 seconds (23% CPU, 2673246 Lips) 
% time((constructorEjemplo(d,Piezas), todosConstruir2(50,Piezas,Sol,N))). => 3,308 inferences, 0.002 CPU in 0.011 seconds (19% CPU, 1565547 Lips)

% test4
% time((constructorEjemplo(e,Piezas), todosConstruir1(2000,Piezas,Sol,N))). => 30 inferences, 0.000 CPU in 0.000 seconds (77% CPU, 750000 Lips)
% time((constructorEjemplo(e,Piezas), todosConstruir2(2000,Piezas,Sol,N))). => 20 inferences, 0.000 CPU in 0.000 seconds (95% CPU, 104167 Lips)

% test5
% time((constructorEjemplo(f,Piezas), todosConstruir1(500,Piezas,Sol,N))). => 4,535 inferences, 0.001 CPU in 0.010 seconds (14% CPU, 3064189 Lips)
% time((constructorEjemplo(f,Piezas), todosConstruir2(500,Piezas,Sol,N))). => 71 inferences, 0.000 CPU in 0.000 seconds (81% CPU, 1420000 Lips)

% test6
% time((constructorEjemplo(g,Piezas), todosConstruir1(20,Piezas,Sol,N))). => 2,480,565 inferences, 1.650 CPU in 1.702 seconds (97% CPU, 1503026 Lips)
% time((constructorEjemplo(g,Piezas), todosConstruir2(20,Piezas,Sol,N))). => 1,318,149 inferences, 1.283 CPU in 1.297 seconds (99% CPU, 1027465 Lips)

% test7
% time((constructorEjemplo(h,Piezas), todosConstruir1(20,Piezas,Sol,N))). => 2,720 inferences, 0.001 CPU in 0.001 seconds (98% CPU, 3726027 Lips)
% time((constructorEjemplo(h,Piezas), todosConstruir2(20,Piezas,Sol,N))). => 1,550 inferences, 0.002 CPU in 0.010 seconds (17% CPU, 934861 Lips)

% test8
% time((constructorEjemplo(i,Piezas), todosConstruir1(100,Piezas,Sol,N))). => 2,776,910 inferences, 2.259 CPU in 2.322 seconds (97% CPU, 1229532 Lips)
% time((constructorEjemplo(i,Piezas), todosConstruir2(100,Piezas,Sol,N))). => 1,457,493 inferences, 1.310 CPU in 1.347 seconds (97% CPU, 1112359 Lips)

% test9
% time((constructorEjemplo(i,Piezas), todosConstruir1(200,Piezas,Sol,N))). => 2,776,911 inferences, 2.144 CPU in 2.195 seconds (98% CPU, 1295362 Lips)
% time((constructorEjemplo(i,Piezas), todosConstruir2(200,Piezas,Sol,N))). => 1,457,494 inferences, 1.445 CPU in 1.481 seconds (98% CPU, 1008896 Lips)


