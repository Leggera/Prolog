:- dynamic parent/2.
:- dynamic male/1.
:- dynamic female/1.
:- dynamic spouse/2.

parent(clara, helen).
parent(clara, ron).
parent(andrew, helen).
parent(andrew, ron).
parent(lucy, stefany).
parent(lucy, jack).
parent(rory, stefany).
parent(rory, jack).
parent(sue, stefany).
parent(sue, jack).
parent(gorge, andrew).
parent(gorge, lucy).
parent(harry, andrew).
parent(harry, lucy).
parent(frank, sue).
parent(frank, percy).
parent(kate, sue).
parent(kate, percy).
parent(bob, sue).
parent(bob, percy).
parent(jane, peter).
parent(amy, mia).
parent(amy, gorge).
parent(mark, harry).
parent(mark, jane).

male(ron).
male(jack).
male(andrew).
male(rory).
male(percy).
male(peter).
male(gorge).
male(harry).
male(frank).
male(bob).
male(mark).
female(helen).
female(stefany).
female(clara).
female(lucy).
female(sue).
female(mia).
female(jane).
female(kate).
female(amy).

spouse(helen, ron).
spouse(stefany, jack).
spouse(andrew, lucy).
spouse(sue, percy).
spouse(mia, gorge).
spouse(harry, jane).

edge(X, Y, 1):-
	father(X, Y);
	mother(X, Y);
	daughter(X, Y);
	son(X, Y);
	spouse_(X, Y);
	brother(X, Y);
	sister(X, Y);
	cousin(X, Y);
	grandmother(X, Y);
	grandfather(X, Y);
	granddaughter(X, Y);
	grandson(X, Y);
	uncle(X, Y);
	aunt(X, Y);
	shurin(X, Y);
	zolovka(X, Y);
	dever(X, Y);
	svoyachenitsa(X, Y);
	svecrov(X, Y);
	svecor(X, Y);
	test(X, Y);
	tesha(X, Y);
	nevestka(X, Y);
	snokha(X, Y);
	zyat(X, Y);
	svaha(X, Y);
	svat(X, Y);
	great_grandmother(X, Y);
	great_grandfather(X, Y);
	great_grandson(X, Y);
	great_granddaughter(X, Y).

child(Y, X) :- parent(X, Y).
spouse_(X, Y):-(spouse(X, Y), !;spouse(Y, X)).
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
brother(X, Y):-parent(X, Z), parent(Y, Z), male(Y), dif(X, Y). /* при наличии обоих родителей выдает два одинаковых ответа*/
sister(X, Y):-parent(X, Z), parent(Y, Z), female(Y), dif(X, Y). /* при наличии обоих родителей выдает два одинаковых ответа*/

uncle(X, Y):-parent(X, Z), (brother(Z, Y); sister(Z, W), spouse_(W, Y)).
aunt(X, Y):-parent(X, Z), (sister(Z, Y);brother(Z, W), spouse_(W, Y)).

shurin(X, Y):-male(Y), sister(Y, Z), spouse_(Z, X).
zolovka(X, Y):-female(Y), brother(Y, Z), spouse_(Z, X).
dever(X, Y):-male(Y), brother(Y, Z), spouse_(Z, X).
svoyachenitsa(X, Y):-female(Y), sister(Y, Z), spouse_(Z, X).
svecrov(X, Y):-female(X), spouse_(X, Z), mother(Z, Y).
svecor(X, Y):-female(X), spouse_(X, Z), father(Z, Y).
test(X, Y):-male(X), spouse_(X, Z), father(Z, Y).
tesha(X, Y):-male(X), spouse_(X, Z), mother(Z, Y).
nevestka(X, Y):-zolovka(Y, X); dever(Y, X); svecrov(Y, X).
snokha(X, Y):-male(Y), son(Y, Z), spouse_(Z, X).
zyat(X, Y):-shurin(Y, X); svoyachenitsa(Y, X); test(Y, X); tesha(Y, X).
svaha(X, Y):-mother(Z, X), mother(W, Y), spouse_(Z, W).
svat(X, Y):-father(Z, X), father(W, Y), spouse_(Z, W).



great_grandmother(X, Y):- grandparent(X, Z), mother(Z, Y).
great_grandfather(X, Y):- grandparent(X, Z), father(Z, Y).
great_grandparent(X, Y):-grandparent(X, Z), parent(Z, Y).
great_grandchild(X, Y):-great_grandparent(Y, X).
great_grandson(X, Y):-great_grandchild(X, Y), male(Y).
great_granddaughter(X, Y):-great_grandchild(X, Y), female(Y).
cousin(X, Y):- (uncle(X, Z), child(Z, Y), !;aunt(X, Z), child(Z, Y)).



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

one_word_relation(X, Y, Name):-
	(Name = cousin, cousin(X, Y));
	(Name = great_grandmother, great_grandmother(X, Y));
	(Name = great_grandfather, great_grandfather(X, Y));
	(Name = great_granddaughter, great_granddaughter(X, Y));
	(Name = great_grandson, great_grandson(X, Y));
	(Name = grandmother, grandmother(X, Y));
	(Name = granfather, grandfather(X, Y));
	(Name = granddaughter, granddaughter(X, Y));
	(Name = grandson, grandson(X, Y));
	(Name = shurin, shurin(X, Y));
	(Name = zolovka, zolovka(X, Y));
	(Name = dever, dever(X, Y));
	(Name = svoyachenitsa, svoyachenitsa(X, Y));
	(Name = svecrov, svecrov(X, Y));
	(Name = svecor, svecor(X, Y));
	(Name = test, test(X, Y));
	(Name = tesha, tesha(X, Y));
	(Name = nevestka, nevestka(X, Y));
	(Name = snokha, snokha(X, Y));
	(Name = zyat, zyat(X, Y));
	(Name = svaha, svaha(X, Y));
	(Name = svat, svat(X, Y));
	(Name = uncle, uncle(X, Y));
	(Name = aunt, aunt(X, Y));
	(Name = brother, brother(X, Y));
	(Name = sister, sister(X, Y));
	(Name = father, father(X, Y));
	(Name = mother, mother(X, Y));
	(Name = spouse, spouse_(X, Y));
	(Name = daughter, daughter(X, Y));
	(Name = son, son(X, Y)).


interface:-write("Hello!"), nl,
	see(user),
	repeat,
	write("Here is the list of what you can do"), nl,
	write("Press 1. to ask about family relations"), nl,
	write("Press 2. to insert new information"), nl,
	write("Press 3. to exit"), nl,
	read(X),
	(X = end_of_file, seen, !;
	X = 1, relations;
	X = 2, assertions;
	X = 3, !, fail), fail.

relations:-repeat,
	   write("Press 1. to ask about existing relationship."), nl,
	   write("Press 2. to check in what way two people relate."), nl,
	   write("Press 3. go back to the main menu."), nl,
	   read(X),
	   (X = end_of_file, seen, !
	   ;X = 3, !
	   ;X = 2, relationship
	   ;X = 1, find_relative), fail.

relationship:-
	repeat,
	write("If you want to exit press 3"), nl,
	write("Write name of the first relative"), nl,
	read(X),
	(X = 3, !, fail;
	write("Write name of the second relative"), nl,
	read(Y),
	(Y = 3, !, fail;
	nl,
	(one_word_relation(X, _, _), one_word_relation(Y, _, _),
	write(Y),
	write(" is a "),
	short_path(X, Y, P, _),
	rel(P),!, /*rel prints relationships*/
	write(" of "),
	write(X), nl, nl; write("One or both names don't exist."), nl, nl))), fail.
rel([_]).
rel([X, Y|T]):- one_word_relation(X, Y, Name), rel([Y|T]), ((T \= [], write(" of a ")); T = []), write(Name).

find_relative:-
	print_relations_list,
	repeat,
	write("Enter relation_name from the list above."), nl,
	write("To exit from this level type 3."), nl,
	read(Relation),
	(Relation = 3, !, fail;
	write("Enter existing name"),nl,
	read(Name),
	(Name = 3, !, fail ;
	one_word_relation(Name, Y, Relation), write(Y), nl, nl)), fail.

assertions:-
	repeat,
	write('write "child" to insert new child.'), nl,
	write('write "spouse" to insert new spouse.'), nl,
	write('write 3. to escape this level.'), nl,
	read(X),
	(X = end_of_file, seen, !;
	X = 3, !, fail;
	(X = child, process_child;X = spouse, process_spouse), fail).

process_child:-
	write("Enter the name of the child."), nl,
	read(Name), nl,
	(not(one_word_relation(Name, _, _)),
	 write("Enter the sex of the child (m or f)."), !
	;write("Name already is in the data base"), nl, !, fail), nl,
	read(Sex),(Sex = f, assert(female(Name));Sex = m, assert(male(Name))), nl,
	write("Enter the parent of the child."), nl,
	read(Parent),(one_word_relation(Parent, _, _), !
	; write("Enter sex of "), write(Parent), write(" (m or f)."), nl,
	read(Parent_sex), (Parent_sex = f, assert(female(Parent)); Parent_sex = m,
	assert(male(Parent)))), assert(parent(Name, Parent)),
	write("OK."), nl, nl,
	(spouse_(Parent, Sp), assert(parent(Name, Sp));(not(spouse_(Parent, _)))).

process_spouse:-
	write("Enter the name of a new spouse"), nl,
	read(Name1), nl,
	(not(one_word_relation(Name1, _, _)),
	 write("Enter the name of an existing spouse."), !
	; write("Name already is in the data base."), nl, !, fail), nl,
	read(Name2), nl,
	((spouse_(Name2, Someone), write("Error: "),
	write(Name2), write(" is already married to "), write(Someone), nl,
	!, fail);
	((female(Name2), assert(male(Name1)), !
	; male(Name2), assert(female(Name1)), !
	; write("Error: The sex of the second spouse is unknown."), nl, fail),
	write("OK"), nl, nl, assert(spouse(Name1, Name2)))).


print_relations_list:-
	(write(cousin),nl),
	(write(great_grandmother), nl),
	(write(great_grandfather), nl),
	(write(great_granddaughter), nl),
	(write(great_grandson), nl),
	(write(grandmother), nl),
	(write(granfather), nl),
	(write(granddaughter), nl),
	(write(grandson), nl),
	(write(shurin), nl),
	(write(zolovka), nl),
	(write(dever), nl),
	(write(svoyachenitsa), nl),
	(write(svecrov), nl),
	(write(svecor), nl),
	(write(test), nl),
	(write(tesha), nl),
	(write(nevestka), nl),
	(write(snokha), nl),
	(write(zyat), nl),
	(write(svaha), nl),
	(write(svat), nl),
	(write(uncle), nl),
	(write(aunt), nl),
	(write(brother), nl),
	(write(sister), nl),
	(write(father), nl),
	(write(mother), nl),
	(write(spouse), nl),
	(write(daughter), nl),
	(write(son), nl).

































