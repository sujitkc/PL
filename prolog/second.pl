child(pavan, vamshi).
child(vamshi, sujit).
child(sujit, rabin).
child(rabin, suren).
descend(X, Y):- child(X, Y).
descend(X, Y):- child(X, Z), descend(Z, Y).
