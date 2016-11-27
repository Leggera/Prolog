edge1(a, b, 1).
/*edge1(d, f, 7).*/
edge1(a, c, 3).
edge1(a, f, 2).
edge1(f, m, 2).
edge1(c, m, 1).
edge1(m, d, 3).
/*edge1(d, a, 4).*/
edge1(f, c, 9).
/*edge1(l, p, 8).*/

edge(X, Y, N):-edge1(X, Y, N);edge1(Y, X, N).

min_path(X, Y, N1, P1):-findall([N, P], path(X, Y, N, P),  ALL_P), shortest(N1, P1, ALL_P), !.
shortest(X1, Y1, [[X1|Y1]]).
shortest(N, P, [[X1|Y1], [X2|_]| PS]):-X1<X2,!, shortest(N, P, [[X1|Y1]| PS]).
shortest(N, P, [[X1|Y1], [X2|Y2]| PS]):-X1=X2,!, shortest(N, P, [[X1|[Y1, Y2]]| PS]).
shortest(N, P, [[_|_], [X2|Y2]| PS]):- shortest(N, P, [[X2|Y2]| PS]).

path(X, Y, N, P):-path5(X, Y, [], N, P). /* works with oooi, ooio, ooii, iooi,oioi, iiio, iioi, iiii, oooo, iooo, oioo, iioo, ioio, oiio, ioii, oiii*/
path5(X, X, [], 0, []).
path5(X, Y, _, N, [X, Y]):-edge(X, Y, N).
path5(X, Y, L, N, [X|P]):-edge(X, X1, N1), not(member(X1, L)), dif(Y, X1), path5(X1, Y,[X|L], N2, P), not(member(X, P)), N is N1+N2.

edges2([], []):-!.
edges2([[_]], []):-!.
edges2([[_,X|L]|P], S):-!,edges2(X, V1), edges2([[_|L]], V2), edges2(P, V3), append(V1,V2,T),append(T, V3, V),list_to_set(V, S).
edges2(X, [[X|V]]):-findall(X1, edge(X, X1, _), V1), list_to_set(V1, V),!.
short_path([] ,_, []).
short_path(X, Y, [[]]):-edge(X, Y, _),  !.
short_path(X, Y, P):-edges2(X, V), /*print(X), print(V)*/ ((members(Y, V, Ex), Ex \= [], find(Ex, X, S), append(S, Ex, P), !) ;(short_path(V, Y, [B|L]),((find(B,X,S), P = [S,B|L],!);print(L),split(L, P), print("K"),print(P)))).
split2([], []):-print("A"),!.
split2([[X|T]|L], V):-print("B"), !, split2([X|L], V1),print(V1), ((T \= [],  split2([T|L], V2), print("D"), print(V2), append(V1, V2, V), !) ; (V = V1)).
split2([Y|L], V):-print("C"), split2(L, V1), print(V1), ((V1 \= [], app(Y, V1, V), print("E"), print(V),!); (V = [Y])).
/*split2([], []):-!.
split2([[X|T]|L], V):-!, print("A"), print([X|L]), split2([X|L], V1),((T \= [], split2([T|L], V2), !);V2 = []), append(V1, V2, V) /*, print("A"), print(X), print(T), print(L)*/ .
split2([X, [Y|T]|L], V):-!, print("B"), print([X, Y|L]),split2([X, Y|L], V1), ((T \= [], split2([X, T|L], V2), !) ; V2 = []), append([V1], V2, V).
split2([Y|L], V):-!, print("C"), print(Y), ((L \= [], split2(L, T), print("app"), print(Y), print(T), app(Y, T, V), print("KKK"), !) ; (V = Y)).*/
/*app(X, [H|T], [[X, H]|V]):-app(X, T, V).*/
/*app(_, [[]|_], []):-!.*/
app(_, [], []):-!.
app(X, [[H|T1]|T], R):-!, print("F"), ((T \= [], app(X, T, V), R = [[X,H|T1]|V],!);(R = [X,H|T1])).
app(X, [H|T], R):-print("D"), ((T \= [], app(X, T, V), R=[X,H|V], !);(R=[X, H])).
split([], []):-!.
split([[X|T]|L], V):-!, print("A"), split([X|L], V1), ((T \=[], print("B"),split([T|L], V2), print(V1), print(V2),append([V1], V2, V), !); (V = [V1])).
split([X|L], V):-print("C"), ((L \= [], split(L, V1), app(X, V1, V));(V = [X])).
/*app(X, [[H1|T1]|T], V):-!, print("F"), print(X), app(H1, T, V1), ((T1 \= [], app(X, [T1|T], V2), append([X|V1], V2, V), !);(V = V1)), print(V).*/
/*app(X, [H|T], R):-!, print("G"), /*((T \= [], app(H, T, V), !);(V = [H])),*/ app(H, T, R). /*, print([X|V]).*/
?*/

/*split2([], []):-print("K").
split2([[X|T]|L], [[X|V1]|V2]):-!, split2(L, V1),print("X"), print(X), print("V1"),print(V1), ((T \= [], split2([T|L], V2), !) ; V2=[]), print("[X|V1]"),print([X|V1]), print("V2"), print(V2).
split2([Y,[X|T]|L], [[Y, X|V1]|V2]):- !, print("Y"), split2(L, V1), split2([Y, [T]|L], V2).
split2([[]|L], V):- !, split2(L, V).
split2([Y|L], [Y|V1]):- print("Z"), split2(L, V1).*/

/*split2([[]|L], R, V):- split2(L, R, V), !.
split2([[X|T]|L], [X|R], [X|V]):- !, print("A"),split2([X|L], R1, V1), ((T \= [], print("B"), split2([T|L], R2, V2), print(V1), print([T|L]),print(V2),append([V1], V2, V), !) ; (V = V1)).
split2([],[],[]):- !.
split2([X|T], [X|R], [X|V]):- print("T"), split2(T, R, V).
split(L,[P|R]):-split2(L, P, S), print("here"), break, split(S, R), !.
split(_,[]).*/

find([],_,[]).
find([H|T], V, [X|L]):-find(H, V, X), find(T, V, L),!.
find(R,[[X|T]|L], [X|S]):-member(R,T), find(R, L, S),!.
find(R,[[_|_]|L], S):-find(R,L,S), !.
find(_, [], []).
members(_, [],[]).
members(Y, [[X|V1]|V], [X|Ex]):- member(Y, V1), members(Y, V, Ex), !.
members(Y, [[_|_]|V], Ex):-members(Y, V, Ex).



cyclic:-cyclic(_).
cyclic(X):-edge(X, X1,_), path1(X, X1,_),!.
path1(X, Y, P):-path4(X, Y, X, Y, [], P).
path4(X, X, _, _, [], []).
path4(X, Y, B, E,_, [X, Y]):-edge(X, Y, _), (dif(X, B);dif(Y, E)).
path4(X, Y, B, E, L, [X|P]):-edge(X, X1, _), not(member(X1, L)), path4(X1, Y, B, E,[X|L], P), not(member(X, P)).

vertices(S):-findall([X, Y], edge1(X, Y, _), V),flatten(V, V1), list_to_set(V1, S),!.

is_connected:- vertices(V), is_connected(V).
is_connected([X, Y|T]):-path(X, Y, _, _), is_connected([X|T]), !.
is_connected([_]).

/*finds([H|T], V, [X|L]):-finds(H, V, X), finds(T, V, L),!.*/
finds(R,[[X|T]|_], X):-member(R,T).
finds(R,[[_|_]|L], S):-finds(R,L,S).
finds(_, [], []).
membs(Y, [[X|V1]|_], X):- member(Y, V1).
membs(Y, [[_|_]|V], Ex):-membs(Y, V, Ex).

shpath([] ,_, [], 0).
shpath(X, Y, [X, Y], 1):-edge(X, Y, _), !.
shpath(X, Y, [Ex, Y], 2):- edges2(X, V), membs(Y, V, Ex), !.
shpath(X, Y, [Ex, B|P], N):- edges2(X, V), shpath(V, Y, [B|P], N1), membs(B, V, Ex), N is N1+1 .

/*shpath1([] ,_, [], 0).
shpath1(X, Y, [[]], 1):-edge(X, Y, _), !.
shpath1(X, Y, [Ex], 2):- edges2(X, V), membs(Y, V, Ex), !.
shpath1(X, Y, R, N):- edges2(X, V), shpath1(V, Y, [B|P], N1), membs(B,
V, Ex), ((X \= Ex, R = [Ex, B|P]);R=[B|P]), N is N1+1 .*/



























