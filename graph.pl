edge1(a, b, 1).
edge1(a, f, 8).
edge1(f, d, 7).
edge1(a, c, 3).
edge1(c, d, 3).
edge1(d, m, 4).

edge(X, Y, N):-edge1(X, Y, N);edge1(Y, X, N).

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
sh_path(X, Y, P):-edges2(X, V),print(X),print(V),((members(Y, V, Ex), Ex \= [],find(Ex, X, S), P = [S, Ex, Y]) ;(sh_path(V, Y, [B|L]),((find(B,X,S),P = [S,B|L],!);P=[B|L]/*split([B|L], P)*/))).

/*find(R, [[X|T]|L],S):-print("R"),print(R),print(X),print(T),print(L),(print(R),print(X),print(T),member(R, T), S is X,print(S),!);find(R, [L], S).*/
split([[X, Y], Z|T], [[X|L1],[Y|L2]]):-print("Here"),print(X), print(Y),!,split([Z|T],[L1, L2]),print("Iver"), print(L1), print(L2),!.

split(X, [X, X]):-print("Over"), print(X).
find([],_,[]):-!.
find([H|T], V, [X|L]):-!,find(H, V, X), find(T, V, L).
find(R,[[X|T]|_], X):-member(R,T),!.
find(R,[[_|_]|L], S):-find(R,L,S),!.
members(_, [],[]).
members(Y, [[X|V1]|_], X):-member(Y, V1)/*, members(Y, V, Ex), !*/.
members(Y, [[_|_]|V], Ex):-members(Y, V, Ex).
/*members(Y, [X|V1], [X]):-member(Y, V1).*/

edges3([],[]):-!.
edges3([X|T], P):-!,findall(X1, edge(X, X1, _),V), edges3(T, L), append([[X|[V]]], L,P).
edges3(X, V):-findall(X1, edge(X, X1, _), V).













