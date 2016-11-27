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
male(jack).
male(andrew).
male(harry).
male(gorge).
male(frank).
male(bobby).
female(ann).
female(lucy).
female(sue).
female(kate).
female(amy).
spouse(andrew, lucy).


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
brother(X, Y):-parent(X, Z), parent(Y, Z), male(Y), dif(X, Y). /* при наличии обоих родителей выдвет два одинвоых ответа*/
sister(X, Y):-parent(X, Z), parent(Y, Z), female(Y), dif(X, Y). /* при наличии обоих родителей выдвет два одинвоых ответа*/



uncle(X, Y):-parent(X, Z), brother(Z, Y).
aunt(X, Y):-parent(X, Z), sister(Z, Y).
daughter_in_law(X, Y):-son(X, Z), spouse_(Z, Y). /*невестка*/
son_in_law(X, Y):-daughter(X, Z), spouse_(Z, Y). /*зять*/
brother_in_law(X, Y):-sister(X, Z), spouse_(Z, Y).
sister_in_law(X, Y):-brother(X, Z), spouse_(Z, Y).
great_grandmother(X, Y):- grandmother(X, Z), mother(Z, Y).
great_grandfather(X, Y):- grandfather(X, Z), father(Z, Y).
cousin(X, Y):-(uncle(X, Z);aunt(X, Z)), child(Z, Y).


one_word_relation(X, Y):-(parent(X, Y), print("parent"), !);(spouse_(X, Y), print("spouse"), !);(child(X, Y), print("child"), !);(grandparent(X, Y), print("grandparent"), !);(grandchild(X, Y), print("grandchild"), !);(brother(X, Y), print("brother"), !);(sister(X, Y), print("sister"), !);(uncle(X, Y), uncle("brother"), !);(aunt(X, Y), print("aunt"), !);(daughter_in_law(X, Y), print("dauhgter-in-law"), !);(son_in_law(X, Y), print("son-in-law"), !);(brother_in_law(X, Y), print("brother-in-law"), !);(sister_in_law(X, Y), print("sister-in-law"), !);(great_grandmother(X, Y), print("great grandmother"), !);(great_grandfather(X, Y), print("great grandfather"), !);(cousin(X, Y), print("cousin"), !).

relation(X, Y):-(one_word_relation(X, Y), !);(one_word_relation(X, Z), relation(Z, Y), !).







