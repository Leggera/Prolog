edge1(a, b, 1).
/*edge1(a, f, 8).*/
edge1(d, f, 7).
edge1(a, c, 3).
edge1(c, d, 3).
/*edge1(d, a, 4).*/
edge1(f, c, 9).

edge(X, Y, N):-edge1(X, Y, N);edge1(Y, X, N).
edge2(X, Y, N):-edge1(Y, X, N).

cycle(X, Y, P):-cycle5(X, Y, [], P);cycle5(Y, X, [], P).
cycle5(X, X, [], []).
cycle5(X, Y, _,  [X, Y]):-edge1(X, Y, _).
cycle5(X, Y, L, [X|P]):-edge1(X, X1, _),(X1=Y,print(X1), print(L), break), cycle5(X1, Y,[X|L], P)/*, print(X1), print(X), print(P)*/,member(X, P).

min_path(X, Y, N1, P1):-findall([N, P], path(X, Y, N, P),  ALL_P), shortest(N1, P1, ALL_P), !.
shortest(X1, Y1, [[X1|Y1]]).
shortest(N, P, [[X1|Y1], [X2|_]| PS]):-X1<X2,!, shortest(N, P, [[X1|Y1]| PS]).
shortest(N, P, [[X1|Y1], [X2|Y2]| PS]):-X1=X2,!, shortest(N, P, [[X1|[Y1, Y2]]| PS]).
shortest(N, P, [[_|_], [X2|Y2]| PS]):- shortest(N, P, [[X2|Y2]| PS]).

path(X, Y, N, P):-path5(X, Y, [], N, P).
path5(X, X, [], 0, []).
path5(X, Y, _, N, [X, Y]):-edge(X, Y, N).
path5(X, Y, L, N, [X|P]):-edge(X, X1, N1), not(member(X1, L)), dif(Y, X1), path5(X1, Y,[X|L], N2, P), not(member(X, P)), N is N1+N2.

short_path(X, Y, [X, Y]):-edge(X, Y, _), !.

short_path([X|V], Y, LV):-findall(X1, edge(X, X1, _), P), member(Y, P); short_path(V, Y, LV), !.
short_path([X|_], Y, LV):-findall(X1, edge(X, X1, _), P), short_path(P, Y, LV).
/*short_path(X, Y, [X1|L]):-findall(X1, edge(X, X1, _), V), short_path(V, Y, L).*/
short_path(X, Y, _):-findall(X1, edge(X, X1, _), V), (not(member(Y, V))),!/*?*/ , short_path(V, Y, _).
/*short_path([X|V], Y, P):-(findall(X1, edge(X, X1, _), V1), short_path(V1, Y, P1);short_path(V, Y, P2)), append(P1, P2, P).*/

edges2([], []):-!.
edges2([[_]], []):-!.
edges2([[_,X|L]|P], V):-!,edges2(X, V1), edges2([[_|L]], V2), edges2(P, V3), append(V1,V2,T),append(T, V3, V).
edges2(X, [[X|V]]):-findall(X1, edge(X, X1, _), V),!.
sh_path([] ,_, []).
sh_path(X, Y, [X, Y]):-edge(X, Y, _), !.
sh_path(X, Y, P):-edges2(X, V),((members(Y, V, Ex), Ex \= [],find(Ex, X, S), P = [S, Ex], !) ;(sh_path(V, Y, [B|L]),((find(B,X,S),P = [S,B|L],!);split(L, P)))).

split2([[X|T]|L], [X|R], S):- split2(L, R, V), append([T], V, S).
split2([],[],[]).
split(L,[P|R]):-split2(L, P, S), split(S, R), !.
split(_,[]).
find([],_,[]):-!.
find([H|T], V, [X|L]):-!,find(H, V, X), find(T, V, L).
find(R,[[X|T]|_], X):-member(R,T),!.
find(R,[[_|_]|L], S):-find(R,L,S),!.
members(_, [],[]).
members(Y, [[X|V1]|V], [X|Ex]):-member(Y, V1), members(Y, V, Ex), !.
members(Y, [[_|_]|V], Ex):-members(Y, V, Ex).




cyclic(X):-edge(X, X1,_), path1(X, X1,_),!.
path1(X, Y, P):-path4(X, Y, X, Y, [], P).
path4(X, X, _, _, [], []).
path4(X, Y, B, E,_, [X, Y]):-edge(X, Y, _), (dif(X, B);dif(Y, E)).
path4(X, Y, B, E, L, [X|P]):-edge(X, X1, _), not(member(X1, L)), path4(X1, Y, B, E,[X|L], P), not(member(X, P)).


vertices(S):-findall([X, Y], edge(X, Y, _), V1), flatten(V1, V), setof(V, S).

is_connected:-vertices(V), member(X, V), member(Y, V), dif(X, Y), edge(X, Y), fail.
is_connected.



