trainLink(saarbruecken, dudweiler).
trainLink(forbach, saarbruecken).
trainLink(freyming, forbach).
trainLink(stAvold, freyming).
trainLink(fahlquemont, stAvold).
trainLink(metz, fahlquemont).
trainLink(nancy, metz). 

directTrain(A, B) :- trainLink(A, B) ; trainLink(B, A).

route_(A, A, [A], _).
route_(A, B, [A | Rest], Visited) :-
    directTrain(A, X),
    \+ member(X, Visited),
    route_(X, B, Rest, [X | Visited]).

route(A, B, Path) :- route_(A, B, Path, [A]).