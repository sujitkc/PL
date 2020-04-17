[neg].

mother(sita,luv).
mother(sita,kush).
mother(kaushalya,ram).
mother(sumitra,lakshman).
mother(kaikeyi,bharat).
mother(sumitra,shatrughna).

father(dasharatha,ram).
father(dasharatha,lakshman).
father(dasharatha,bharat).
father(dasharatha,shatrughna).
father(ram,luv).
father(ram,kush).
father(luv, parikshit).

/* parent */

parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).

/* Wife */

wife(X,Y) :- mother(X,Z),father(Y,Z).

/* Sibling */


sibling(X,Y) :-  X \= Y, parent(Z,X), parent(Z,Y).


/* Descendent */

descendent(X,Y) :- parent(Y, X).
descendent(X,Y) :-  descendent(Z,Y), parent(Z, X), !.

