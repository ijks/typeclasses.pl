scalarMult(_, [], []).
scalarMult(N, [X | Xs], [Xm | Xsm]) :-
    Xm is N * X,
    scalarMult(N, Xs, Xsm).

dot(X, Y, R) :- pairwiseMult(X, Y, M), sum(M, R).

sum(X, S) :- sumAcc(X, 0, S).

sumAcc([], Acc, Acc).
sumAcc([ X | Xs ], Acc, S) :-
    NewAcc is Acc + X,
    sumAcc(Xs, NewAcc, S).

pairwiseMult(X, [], X).
pairwiseMult([X | Xs], [Y | Ys], [ M | Ms ]) :-
    M is X * Y,
    pairwiseMult(Xs, Ys, Ms).