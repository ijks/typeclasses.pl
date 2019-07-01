num(0).
num(succ(X)) :- num(X).

add(0, Y, Y).
add(succ(X), Y, succ(Z)) :- add(X, Y, Z).

greaterThan(succ(_), 0).
greaterThan(succ(X), succ(Y)) :- greaterThan(X, Y).