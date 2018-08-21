edge(1, 2).
edge(2, 3).

connected(X, Y) :- edge(X, Z), connected(Z, Y) ; edge(X, Y).
