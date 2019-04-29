% len - length of a list.

len([], 0).
len([_ | T], N) :- len(T, X), N is X + 1.

% mem - if X is a member of a List.
 
not(mem(_,[])).
mem(X,[H|_]) :- X == H.
mem(X,[_|List]) :-  mem(X,List).





% append - To append two lists.

append([], List, List).
append([H|T],Y,[H|NT]) :- append(T, Y, NT).
% palindrome - A symmetric list.






































palindrome([]).
palindrome([_]).
palindrome([H|T]) :-  append(List,[H], T), palindrome(List).
% even_len - If the length of a list a even.






































even_len([], 1).
even_len([_ | L], 1) :- even_len(L, 0).
even_len([_ | L], 0) :- even_len(L, 1).
odd_len(L, 1) :- even_len(L, 0).
odd_len(L, 0) :- even_len(L, 1).


% rem_dup - Remove duplicate elements from a list.




































rem_dup([],[]).
rem_dup([H | T], X) :- mem(H, T), rem_dup(T, X).
rem_dup([H | T], [H | T1]) :- rem_dup(T, T1).
