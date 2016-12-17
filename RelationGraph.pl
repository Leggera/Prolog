:- dynamic parent/2.
:- dynamic male/1.
:- dynamic female/1.
:- dynamic spouse/2.

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
spouse_(X, Y):-(spouse(X, Y), !;spouse(Y, X)), male(X), female(Y).
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

int:-  see(user), repeat, /*print_options*/ read(X), (X = end_of_file, seen, !;process(X), fail).

complex([X|T]):-complex(X), complex(T), !.
complex(X):-(predicate_property(X, dynamic), assert(X), !; print(X), clause(X, (Cl1, Cl2)), complex(Cl1), complex(Cl2)).

interface:-write("Hello!"), nl, repeat, write("Here is the list of what you can do"), nl, write("Press 1. to ask about family relations"), nl, write("Press 2. to insert new information"), nl, write("Press 3. to exit"), nl, see(user), read(X), (X = end_of_file, seen, !; (print(X), (X = 1, relations; X = 2, assertions; X = 3, !, fail), !, fail)).

relations:-write("You can ask about existing relationship in format relation_name(person_name, Y) or relation_name(X, person_name) to learn about people who relate to this person by this relaiton"), nl, write("OR"), nl, write("You can check in what way two people relate in format relation(person_name1, person_name2)"), nl, write("or you can go back to the main menu by pressing 3."), nl, repeat, read(X), (X = end_of_file, seen, !;X=3, !, fail; X = 2; X = 1), fail.
assertions:-see(user), repeat, write('write "child" to insert new child'), nl, write('write "spouse" to insert new spouse'), nl, read(X), (X = end_of_file, seen, !;X = 3, !, fail;(X = child, process_child; X = spouse, process_spouse), fail).

conflict(X):-((X = male(Y), female(Y), !);(X = female(Y), male(Y), !)).
process((X, T)):-!, process(X), process(T), !.
process_child:-
	write("Enter the name of the child"), nl,
	read(Name), nl,
	(not(one_word_relation(Name, _, _)),
	 write("Enter the sex of the child (m or f)"), !
	; write("Name already is in the data base"), nl, !, fail), nl,
	read(Sex),(Sex = f, assert(female(Name));Sex = m, assert(male(Name))), nl,
	write("Enter the parent of the child"), nl,
	read(Parent),(one_word_relation(Parent, _, _), !
	; write("Enter sex of "), write(Parent), write(" (m or f)"), nl,
	read(Parent_sex), (Parent_sex = f, assert(female(Parent)); Parent_sex = m,        assert(male(Parent)))), assert(parent(Name, Parent)).
process_spouse:-
	write("Enter the name of a new spouse"), nl,
	read(Name1), nl,
	(not(one_word_relation(Name1, _, _)),
	 write("Enter the name of a existing spouse"), !
	; write("Name already is in the data base"), nl, !, fail), nl,
	read(Name2), nl,
	(female(Name2), assert(male(Name1)), !
	; male(Name2), assert(female(Name1)), !
	; write("Error: The sex of the second spouse is unknown"), nl, fail),
	assert(spouse(Name1, Name2)).





/*
	(ground(X), !;(!, write("Error: Intermidiate relatives have not yet been declared"),nl)),
	((predicate_property(X, dynamic),(not(conflict(X)), (X, !;assert(X), write(X), nl, !); write("Error: Sex conflict"), nl, !), !);
	not(compound(X));
	skipped(X);
	(X = (Cl1;Cl2), (process(Cl1), !;process(Cl2), !), !);
	(clause(X, Clauses), ((X \= Clauses, process(Clauses)), !;
	(X = Clauses, write("Error: Couldn't expand clause to the basic ones"), nl, !)))).
*/
skipped(X):- X = dif(_, _).



