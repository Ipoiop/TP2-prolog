% entre(+X,+Y,-Z)

entre(X,_,X).     % aca pido que, cuando unifique, es verdadero si pasas o si unifica que X que le pases sea igual al Z
entre(X,Y,Z) :- X < Y, L1 is X+1, entre(L1,Y,Z).
% podes verlo como que "primer caso, si vale x es 2 y z = x, z es 2, siguiente caso, z es nada u otra cosa y asi"
% ojo, mas alla de que eleves el Z, mejor achica la diferencia del X y asi llegas al caso base. lo que no esta unificado, deberas devolver algo que lo haga verdadero recuerda, y si no esta unificado podes unificarlo

% es reversible

% long (+XS,-L)

long([],L) :- L = 0.
long([_|T],L) :- long(T,L1), L is L1 + 1.
% es reversible


% sacar(+X,+XS,-YS)

sacar(_,[],L):- L = []. % como L no es nada con constructor, no le costara unificar L con la lista vacia 
sacar(X,[Y|T],L) :- X = Y, sacar(X,T,L1), L = L1. 
sacar(X,[Y|T],L) :-  X \= Y, sacar(X,T,L1), L = [Y|L1].

% scr(+XS,-YS)

scr([],[]). % era mas facil asi decir que el res era el caso vacio si x era [], intetara unificar [] con lo que le pasas si es verdadero y asi sigue.
scr([X],[X]).
scr([X,X|T],L) :- scr([X|T],L). 
scr([X1,X2|T],L) :- X1 \= X2, scr([X2|T],L1), L = [X1|L1]. % X1 PUEDE SER X2, POR ESO HAGO LO PRIMERO, QUE SEAN DISTINTOS.

 % mas que aportar algoa  la recursion, el [] [] es por si alguien pone ese caso para que no se cuelgue,

 % parteqsuma(+L,+S,-P) me huele a que los resultados los generara el backtrack pues los de delante, si no hay mas res, iran atras buscando los demas casos.

pqs(0,_,[]).  
% LOS CASOS BASE SON MAS DE PATTMATCH Y NO DE "ACOMODAR PARA QUE SEA ESE EL RESULTADO DE ALGO COMO EL DE ENTRE"
pqs(X,[Y|T],L) :- X \= 0,X >= Y, Z is X - Y, pqs(Z,T,L1), L = [Y|L1].
pqs(X,[Y|T],L) :- X \= 0, X < Y , pqs(X,T,L).  % CASO DONDE SE PASO, MEJOR BUSCAR OTRO. 
pqs(X,[_|T],L) :- X \= 0, pqs(X,T,L).
% EL CASO BASE ACA ES QUE ES VERDADERO SI (NO ES "SI NO FUE 9, TERMINA" , NO DEBES DAR CASOS DONDE DECIS QUE ES FALSO, ESO TE LO DA SOLO)

% ESPERA, UNA COSA ES QUE "TERMINE" POR EL CASO BASE Y OTRA ES QUE "DE UN RESULTADO ERRONEO" Y OTRA ES QUE ME ASEGURE DE QUE SI AL FINAL LO QUE ACUMULO NO ES 9, RETROCEDA. 
% ADEMAS, EL CASO EN EL QUE NO SEA 0, SEGUIRA INFINITAMENTE? NO, LO PARARA EL O? NO PUES NO MATCHEARA, Y TAMPOCO CON LOS DEMAS, ENTONCES 
% TENGO GRATIS EL "DETENERME" IMPORTANTE. 
% se detiene pero arroja 2 veces el mismo resultado. 

% Caso base
pqsx(0, _, []).

% Buscar una combinación de X con los elementos de la lista
pqsx(X, [Y|T], L) :- 
    X \= 0, X >= Y, 
    Z is X - Y, 
    pqsx(Z, T, L1), 
    L = [Y|L1].

% Si el número es mayor que el actual, pasamos al siguiente número
pqsx(X, [Y|T], L) :- 
    X \= 0, X < Y, 
    pqsx(X, T, L).

% Saltear el número actual y continuar con el resto de la lista
pqsx(X, [_|T], L) :- 
    X \= 0, 
    pqsx(X, T, L).

% Caso base
pqss(0, _, []).

% Si X es mayor o igual que Y, toma Y y busca la combinación para X - Y
pqss(X, [Y|T], [Y|L]) :- 
    X >= Y,                % X debe ser mayor o igual que Y
    Z is X - Y,            % Restamos Y de X
    pqss(Z, T, L).          % Llama recursivamente con el resto de la lista

% Si X es menor que Y, no tomes Y, solo explora el resto de la lista
pqss(X, [Y|T], L) :- 
    X < Y,                 % Si X es menor que Y, pasamos al siguiente
    pqss(X, T, L).

% Si no tomas el elemento actual, pasa al siguiente
pqss(X, [_|T], L) :- 
    X \= 0,                % Si X no es 0, pasa al siguiente número
    pqss(X, T, L).

% otra opcion era generar sublistas y asi evaluar, osea, usar un generador y eso. el tema esta en como da el siguiente resultado. 

% Caso base: Si el total buscado es 0, la lista resultante es vacía.
pqsxx(0, _, []).

% Si el primer elemento de la lista es menor o igual al total deseado, intentamos incluirlo en la solución.
pqsxx(X, [Y|T], [Y|L1]) :-
    X >= Y,
    Z is X - Y,
    pqsxx(Z, T, L1).

% Si el primer elemento es mayor que el total, lo descartamos y probamos con el resto de la lista.
pqsxx(X, [_|T], L) :-
    X \= 0,
    pqsxx(X, T, L).

% esto me da verdadero si el elemento x pertenece a la lista, y si es libre, este me dara todos los elementos que pertenescan.
member(X,[X|_]).
member(X,[_|L]) :- member(X,L).

% te concatena el l1 con el l2, recorre todo el l2, luego, busca el L3 donde L al final sera el L2 y ya. 

append([],L,L).
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).

% iesimo(+I,+L,-X) REVERSIBLE ES PARA -X NO PARA ? PUES POR DEFINICION DEBERIAS "HACER UN TRABAJO DE MAS"

iesimo(0,[X|_],X).
iesimo(X,[_|T],L) :- Z is X-1 ,iesimo(Z,T,L).

% ahora lo hacemos reversible pero creo que si ya lo es pues no toco o defino el L.

% Ejercicio
% El predicado desde.
desde(X, X). 
desde(X, Y) :- N is X+1, desde(N, Y).

% Y debe ser menor a X EN CASO DE QUE QUIERAS REVERSIBILIDAD? NO, SI VES BIEN, Y SI ES MENOR A X, ESTE NO TERMINARA NUNCA LLEGARA AL CASO BASE.
% lo de arriba va a condicion pues no hay caso de evitar que se ponga Y menor? si, aca abajo.
% desdeReversible(+X,?Y)

desdeReversible(X,X).
desdeReversible(X,Y) :- nonvar(Y), Y >= X.
desdeReversible(X,Y) :- var(Y), N is X+1, desdeReversible(N, Y).

% pmq(+X,-Y)
% aca tenemos entre como generador, espar si cumple.
espar(X) :- 0 is X mod 2.

pmq(X,Y) :- entre(0,X,Y), espar(Y). 

% comprimos(-X,-Y)
% lo que quiere aca es que me de todos los pares comprimos pero alternados si o si. 
coprimos(X,Y):- desde(1,L) , entre(1,L,X), Y is L-X, 1 is gcd(X,Y).

% admite el 1,0, pero no deberia, si pongo L-1 EN EL ENTRE Y EN EL DESDE DESDE 2, OMIIRA ESE CASO.

% el setof, evalua P(X) y ejecuta o da el valor en "X" para asi, ponerlo en L y si esta no lo pone, asi con cada X. 

% ejemplo
% esto extrae las primeras componentes de las tuplas en la lista de tuplas. 
primeraComponente([(X,_)|_],X).
primeraComponente([_|XS],X) :- primeraComponente(XS,X).

% el setof podes usarlo donde quieras, de hecho, el ?- setof(X,primeraComponente([(2,2),(1,3),(1,4)],X),L). 
% ves bien lo que hace, llama a primera componente, pone el L, y sigue extrayendo cosas del generador. 
% ahi tenes un caso de "pasas un predicado como argumento"

% Cortemasparejo(+L,-L1,-L2)
% basicamente, pide que de una lista que al cortarla, la diferencia entre ambas sea la mas menor. 
% para ello podemos realizar cortes al azar y pedir que la diferencia de las demas no sean mas pequeñas

prefijo(L,P) :- append(P,_,L).

% solo si XS E YS NO ESTAN DEFINIDOS.
% podes usar variables y calcularlas luego, por la unificacion, valdra. POR LA UNIFICACION, VALDRA, IMPORTATEEEEEEEEEEEEEEEEEEEEEEE
% pensaste que not corte mas parejo de lo mismo, pero no era asi, ese era tu error, era el append el que iba al not, siempre pesas otras opciones diferentes para no perder opciones. y no es recursion del "mas parejo entre estos"
% pues si fuera asi, generaria mucchas opciones en cada uno.

sum([], 0).  % La suma de una lista vacía es 0.
sum([X|XS], Sum) :- sum(XS, Rest), Sum is X + Rest.

difsuma(L1,L2,P) :- sum(L1,X),sum(L2,Y), P is abs(X-Y).  
cortemasparejo(L,XS,YS) :- append(XS,YS,L), difsuma(XS,YS,Z), not((append(XS2,YS2,L), difsuma(XS2,YS2,D), D < Z)).  

% basicamente pasa algo y no puede pasar que lo que da el append, y luego haces la difsuma, este sea D menor a Z. si lo que da
% es el mismo, D < Z ACA ES FALSO ENTONCES LO UNICO QUE PUEDE CONTRADECIR ES D =< Z y si el append no da mas, si D<Z es falso, 
% intentara usar otro append, y asi ves todos, si hay alguno tal que sea menor cagaste, pero si es igual que pasa? not algo es verdadero, pero asi no revisaria todos las opciones, 
% entonces deberia ser falso el not hasta buscar con todos, y no tener en cuenta si es igual 

% el not y lo demas es simplemente evaluar un predicado donde te decia todo lo que debias ver, pero como no juntos todos los predicados en uno solo, debnes ponerlo asi.
corteMasParejo2(L, L1, L2) :-
    append(L1, L2, L),               % Genera todas las particiones L1 y L2
    L1 \= [],                         % Asegura que L1 no sea vacío
    L2 \= [],                         % Asegura que L2 no sea vacío
    sumlist(L1, Sum1),                % Calcula la suma de L1
    sumlist(L2, Sum2),                % Calcula la suma de L2
    Dif is abs(Sum1 - Sum2),          % Calcula la diferencia entre las sumas
    not((append(L1P, L2P, L),          % Recorre las demás particiones
         L1P \= [],                    % Asegura que L1P no sea vacío
         L2P \= [],                    % Asegura que L2P no sea vacío
         sumlist(L1P, Sum1P),          % Calcula la suma de L1P
         sumlist(L2P, Sum2P),          % Calcula la suma de L2P
         DifP is abs(Sum1P - Sum2P),  % Calcula la diferencia entre las sumas de la partición
         DifP < Dif                   % Si existe una partición con menor diferencia, falla
    )).



% Filtra particiones con diferencia menor
% el TP contiene como "funcion" como fue definido, algo asi como funciones como tipos. 
% una implementacion es mas declarativa, y la que hice tambien esta bien.

intercalar(XS,YS,ZS):- append(L1,R1,Z1), append(L2,R2,Z2), ZS = append(Z1,Z2,) 
