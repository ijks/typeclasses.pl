directlyIn(natasha, irina).
directlyIn(olga, natasha).
directlyIn(katarina, olga).

in(X, Y) :- directlyIn(X, Y).
in(X, Z) :- directlyIn(X, Y), in(Y, Z).