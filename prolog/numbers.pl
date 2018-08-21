fact(1, 1).
fact(N, F) :- fact(N1, F1), N is N1 + 1, F is F1 * N.

/**
?- fact(4,N).
N = 24 .

?- trace.
true.

[trace]  ?- fact(3,N).
   Call: (7) fact(3, _G1894) ? creep
   Call: (8) fact(_G1969, _G1970) ? creep
   Exit: (8) fact(1, 1) ? creep
   Call: (8) 3 is 1+1 ? creep
   Fail: (8) 3 is 1+1 ? creep
   Redo: (8) fact(_G1969, _G1970) ? creep
   Call: (9) fact(_G1969, _G1970) ? creep
   Exit: (9) fact(1, 1) ? creep
   Call: (9) _G1972 is 1+1 ? creep
   Exit: (9) 2 is 1+1 ? creep
   Call: (9) _G1975 is 1*2 ? creep
   Exit: (9) 2 is 1*2 ? creep
   Exit: (8) fact(2, 2) ? creep
   Call: (8) 3 is 2+1 ? creep
   Exit: (8) 3 is 2+1 ? creep
   Call: (8) _G1894 is 2*3 ? creep
   Exit: (8) 6 is 2*3 ? creep
   Exit: (7) fact(3, 6) ? creep
N = 6 ;
   Redo: (9) fact(_G1969, _G1970) ? creep
   Call: (10) fact(_G1969, _G1970) ? creep
   Exit: (10) fact(1, 1) ? creep
   Call: (10) _G1972 is 1+1 ? creep
   Exit: (10) 2 is 1+1 ? creep
   Call: (10) _G1975 is 1*2 ? creep
   Exit: (10) 2 is 1*2 ? creep
   Exit: (9) fact(2, 2) ? creep
   Call: (9) _G1978 is 2+1 ? creep
   Exit: (9) 3 is 2+1 ? creep
   Call: (9) _G1981 is 2*3 ? creep
   Exit: (9) 6 is 2*3 ? creep
   Exit: (8) fact(3, 6) ? creep
   Call: (8) 3 is 3+1 ? creep
   Fail: (8) 3 is 3+1 ? creep
   Redo: (10) fact(_G1969, _G1970) ? creep
   Call: (11) fact(_G1969, _G1970) ? creep
   Exit: (11) fact(1, 1) ? creep
   Call: (11) _G1972 is 1+1 ? creep
   Exit: (11) 2 is 1+1 ? creep
   Call: (11) _G1975 is 1*2 ? creep
   Exit: (11) 2 is 1*2 ? creep
   Exit: (10) fact(2, 2) ? creep
   Call: (10) _G1978 is 2+1 ? creep
   Exit: (10) 3 is 2+1 ? creep
   Call: (10) _G1981 is 2*3 ? creep
   Exit: (10) 6 is 2*3 ? creep
   Exit: (9) fact(3, 6) ? creep
   Call: (9) _G1984 is 3+1 ? creep
   Exit: (9) 4 is 3+1 ? creep
   Call: (9) _G1987 is 6*4 ? creep
   Exit: (9) 24 is 6*4 ? creep
   Exit: (8) fact(4, 24) ? creep
   Call: (8) 3 is 4+1 ? creep
   Fail: (8) 3 is 4+1 ? creep
   Redo: (11) fact(_G1969, _G1970) ? creep
   Call: (12) fact(_G1969, _G1970) ? creep
   Exit: (12) fact(1, 1) ? creep
   Call: (12) _G1972 is 1+1 ? creep
   Exit: (12) 2 is 1+1 ? creep
   Call: (12) _G1975 is 1*2 ? creep
   Exit: (12) 2 is 1*2 ? creep
   Exit: (11) fact(2, 2) ? creep
   Call: (11) _G1978 is 2+1 ? creep
   Exit: (11) 3 is 2+1 ? creep
   Call: (11) _G1981 is 2*3 ? creep
   Exit: (11) 6 is 2*3 ? creep
   Exit: (10) fact(3, 6) ? creep
   Call: (10) _G1984 is 3+1 ? creep
   Exit: (10) 4 is 3+1 ? creep
   Call: (10) _G1987 is 6*4 ? creep
   Exit: (10) 24 is 6*4 ? creep
   Exit: (9) fact(4, 24) ? creep
   Call: (9) _G1990 is 4+1 ? creep
   Exit: (9) 5 is 4+1 ? creep
   Call: (9) _G1993 is 24*5 ? creep
   Exit: (9) 120 is 24*5 ? creep
   Exit: (8) fact(5, 120) ? creep
   Call: (8) 3 is 5+1 ? creep
   Fail: (8) 3 is 5+1 ? 
*/


fact1(1, 1).
fact1(N, F) :- N > 1, N1 is N - 1, fact1(N1, F1), F is F1 * N.































