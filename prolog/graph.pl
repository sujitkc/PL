edge(a,b).
edge(a,f).
edge(f,g).
edge(b,c).
edge(g,c).
edge(f,c).
edge(f,e).
edge(c,e).
edge(c,d).
edge(e,d).



























twoedge(X,Y) :- edge(X,Z), edge(Z,Y).
threeedge(X,Y) :- twoedge(X,Z), edge(Z,Y).

































path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z),path(Z,Y).
