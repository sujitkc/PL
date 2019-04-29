proud(X) :- parent(X, Y), newborn(Y).
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).
father(deepak, amogh).
mother(purnima, lakshmi).
mother(purnima, amogh).
newborn(amogh).
