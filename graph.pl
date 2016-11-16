edge1(a, b, 1).
edge1(a, c, 1).
edge1(b, c, 1).

edge(X, Y, N):-edge1(X, Y, N);edge1(Y, X, N).

d_path(X, X, 0, []).
d_path(X, Y, N, []):-edge(X, Y, N).
d_path(X, Y, N, [X1|L2]):-edge(X, X1, N1), dif(X1, Y), path(X1, Y, N2, L2), not(member(X1, L2)), N is N1+N2.

min_path(X, Y, N1, P1):-findall([N, P], path(X, Y, N, P),  ALL_P), shortest(N1, P1, ALL_P).
shortest(X1, Y1, [[X1|Y1]]).
shortest(N, P, [[X1|Y1], [X2|_]| PS]):-X1<X2,!, shortest(N, P, [[X1|Y1]| PS]).
shortest(N, P, [[_|_], [X2|Y2]| PS]):- shortest(N, P, [[X2|Y2]| PS]).

path(X, Y, N, P):-path5(X, Y, [], N, P).
path5(X, X, [], 0, []).
path5(X, Y, _, N, [X, Y]):-edge(X, Y, N).
path5(X, Y, L, N, [X|P]):-edge(X, X1, N1), not(member(X1, L)), dif(Y, X1), path5(X1, Y,[X|L], N2, P), not(member(X, P)), N is N1+N2.
