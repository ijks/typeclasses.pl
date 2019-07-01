twice([], []).
twice([A | Xs], [A, A | Ys]) :- twice(Xs, Ys).

member(X, [Y | _]) :- X = Y.
member(X, [_ | Ys]) :- member(X, Ys).

subset([], _).
subset([X | Xs], Ys) :- member(X, Ys), subset(Xs, Ys).

setEqual(X, Y) :- subset(X, Y), subset(Y, X).

reverseAcc([], Acc, Acc).
reverseAcc([H | T], Acc, Res) :-
    reverseAcc(T, [H | Acc], Res).

reverse(X, Res) :- reverseAcc(X, [], Res).

palindrome(L) :- reverse(L, L).

last([X], X).
last([_ | Xs], Y) :- last(Xs, Y).

lastRev(L, X) :- reverse(L, [X | _]).