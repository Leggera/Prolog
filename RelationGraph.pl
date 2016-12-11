parent(ann, jack).
parent(lucy, jack).
parent(sue, jack).
parent(harry, andrew).
parent(gorge, andrew).
parent(harry, lucy).
parent(gorge, lucy).
parent(kate, sue).
parent(frank, sue).
parent(bobby, sue).
parent(amy, gorge).
parent(mark, harry).
parent(mark, jane).
male(jack).
male(andrew).
male(harry).
male(gorge).
male(frank).
male(bobby).
male(mark).
female(ann).
female(lucy).
female(sue).
female(kate).
female(amy).
female(jane).
spouse(andrew, lucy).
spouse(harry, jane).

edge(X, Y, 1):-parent(X, Y);child(X, Y);spouse(X, Y); brother(X, Y); sister(X, Y); cousin(X, Y); grandparent(X, Y); grandchild(X, Y); uncle(X, Y); aunt(X, Y);daughter_in_law(X, Y);brother_in_law(X, Y);great_grandmother(X, Y); great_grandfather(X, Y).

child(Y, X) :- parent(X, Y).
spouse_(X, Y):-spouse(X, Y);spouse(Y, X).
mother(X, Y):-parent(X, Y), female(Y).
father(X, Y):-parent(X, Y), male(Y).
daughter(Y, X):- child(Y, X), female(X).
son(Y, X):-child(Y, X), male(X).
grandparent(X, Y):-parent(X, Z), parent(Z, Y).
grandchild(Y, X):-grandparent(X, Y).
grandmother(X, Y):-grandparent(X, Y), female(Y).
grandfather(X, Y):-grandparent(X, Y), male(Y).
granddaughter(Y, X):-grandchild(Y, X), female(X).
grandson(Y, X):-grandchild(Y, X), male(X).
brother(X, Y):-parent(X, Z), parent(Y, Z), male(Y), dif(X, Y). /* при наличии обоих родителей выдвет два одинаковых ответа*/
sister(X, Y):-parent(X, Z), parent(Y, Z), female(Y), dif(X, Y). /* при наличии обоих родителей выдвет два одинаковых ответа*/


/* мужья дядь и теть не считаются тетями и дядями */
uncle(X, Y):-parent(X, Z), brother(Z, Y).
aunt(X, Y):-parent(X, Z), sister(Z, Y).
/* в обратную сторону будет просто родитель супруга */
daughter_in_law(X, Y):-son(X, Z), spouse_(Z, Y). /*невестка*/
son_in_law(X, Y):-daughter(X, Z), spouse_(Z, Y). /*зять*/
/* в обратную сторону будет просто брат/сестра супруга */
brother_in_law(X, Y):-sister(X, Z), spouse_(Z, Y).
sister_in_law(X, Y):-brother(X, Z), spouse_(Z, Y).
great_grandmother(X, Y):- grandmother(X, Z), mother(Z, Y).
great_grandfather(X, Y):- grandfather(X, Z), father(Z, Y).
cousin(X, Y):- (uncle(X, Z);aunt(X, Z)), child(Z, Y).



edges2([], []):-!.
edges2([[_]], []):-!.
edges2([[_,X|L]|P], S):-!,edges2(X, V1), edges2([[_|L]], V2), edges2(P, V3), append(V1,V2,T),append(T, V3, V),list_to_set(V, S).
edges2(X, [[X|V]]):-findall(X1, edge(X, X1, _), V1), list_to_set(V1,
V),!.
members(Y, [[X|V1]|_], X):- member(Y, V1).
members(Y, [[_|_]|V], Ex):- members(Y, V, Ex).
short_p([] ,_, [], 0).
short_p(X, Y, [X, Y], 1):-edge(X, Y, _), !.
short_p(X, Y, [Ex, Y], 2):- edges2(X, V), members(Y, V, Ex).
short_p(X, Y, [Ex, B|P], N):- edges2(X, V), short_p(V, Y, [B|P], N1), members(B, V, Ex), N is N1+1.

short_path(X, Y, P, N):-short_p(X, Y, _, N1), !, short_p(X, Y, P, N), (((N > N1), !, fail);(N = N1)).

one_word_relation(X, Y, Name):-(cousin(X, Y), Name = "cousin");(great_grandmother(X, Y), Name = "great grandmother");(great_grandfather(X, Y), Name = "great grandfather");(grandparent(X, Y), Name = "grandparent");(grandchild(X, Y), Name = "grandchild");(daughter_in_law(X, Y), Name = "dauhgter-in-law");(son_in_law(X, Y), Name = "son-in-law");(brother_in_law(X, Y), Name = "brother-in-law");(sister_in_law(X, Y), Name = "sister-in-law");(uncle(X, Y), Name = "uncle");(aunt(X, Y), Name = "aunt");(brother(X, Y), Name = "brother");(sister(X, Y), Name = "sister");(parent(X, Y), Name = "parent");(spouse_(X, Y), Name = "spouse");(child(X, Y), Name = "child").

relationship(X, Y):-short_path(X, Y, P, _), rel(P), !.
rel([_]).
rel([X, Y|T]):- one_word_relation(X, Y, Name), rel([Y|T]), (((T \= []), write(" of a "), !);(T = [])), write(Name).

