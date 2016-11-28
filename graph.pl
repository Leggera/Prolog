/**edge1(a, b, 1).**/
/*edge1(d, f, 7).*/
/**edge1(a, c, 3).
edge1(a, f, 2).
edge1(f, m, 2).
edge1(c, m, 1).
edge1(m, d, 3).**/
/*edge1(d, a, 4).*/
/**edge1(f, c, 9).**/
/*edge1(l, p, 8).*/

edge1(a,b,4).
edge1(a,d,4).
edge1(a,e,1).
edge1(b,e,3).
edge1(d,e,3).
edge1(c,e,2).
edge1(b,c,1).
edge1(d,c,1).
edge1(b,f,15).
edge1(d,f,7).

edge(X, Y, N):-edge1(X, Y, N);edge1(Y, X, N).

min_path(X, Y, N1, P1):-setof([N, P], path(X, Y, N, P),  ALL_P), shortest(N1, P1, ALL_P).
/*shortest(X1, Y1, [[X1|Y1]]).
shortest(N, P, [[X1|Y1], [X2|_]| PS]):-X1<X2,!, shortest(N, P, [[X1|Y1]| PS]).
shortest(N, P, [[X1|Y1], [X2|Y2]| PS]):-X1=X2,!, shortest(N, P, [[X1|[Y1, Y2]]| PS]).
shortest(N, P, [[_|_], [X2|Y2]| PS]):- shortest(N, P, [[X2|Y2]| PS]).*/


shortest(N, P, [[X1|Y1], [X2|_]| PS]):-X1<X2, !, shortest(N, P, [[X1|Y1]| PS]).
shortest(N, P, [[X1|Y1], [X2|_]| PS]):-X1=X2, shortest(N, P, [[X1|Y1]| PS]).
shortest(N, P, [[_|_], [X2|Y2]| PS]):- shortest(N, P, [[X2|Y2]| PS]), !.
shortest(X1, Y1, [[X1,Y1|_]]).

path(X, Y, N, P):-path5(X, Y, [], N, P). /* works with oooi, ooio, ooii, iooi,oioi, iiio, iioi, iiii, oooo, iooo, oioo, iioo, ioio, oiio, ioii, oiii*/
path5(X, X, [], 0, []).
path5(X, Y, _, N, [X, Y]):-edge(X, Y, N).
path5(X, Y, L, N, [X|P]):-edge(X, X1, N1), not(member(X1, L)), dif(Y, X1), path5(X1, Y,[X|L], N2, P), not(member(X, P)), N is N1+N2.

edges2([], []):-!.
edges2([[_]], []):-!.
edges2([[_,X|L]|P], S):-!,edges2(X, V1), edges2([[_|L]], V2), edges2(P, V3), append(V1,V2,T),append(T, V3, V),list_to_set(V, S).
edges2(X, [[X|V]]):-findall(X1, edge(X, X1, _), V1), list_to_set(V1, V),!.

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

members(Y, [[X|V1]|_], X):- member(Y, V1).
members(Y, [[_|_]|V], Ex):-members(Y, V, Ex).

short_path([] ,_, [], 0).
short_path(X, Y, [X, Y], 1):-edge(X, Y, _), !.
short_path(X, Y, [Ex, Y], 2):- edges2(X, V), members(Y, V, Ex), !.
short_path(X, Y, [Ex, B|P], N):- edges2(X, V), short_path(V, Y, [B|P], N1), members(B, V, Ex), N is N1+1 .

























