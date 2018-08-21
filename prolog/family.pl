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

parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).

wife(X,Y) :- mother(X,Z),father(Y,Z).
sibling(X,Y) :-  parent(Z,X), parent(Z,Y),not(X = Y).

descendent(X,Y) :- parent(Y,X).
descendent(X,Y) :- parent(Z,X), descendent(Z,Y).
