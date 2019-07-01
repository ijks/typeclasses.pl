:- use_module(library(clpfd)).

word(astante, a, s, t, a, n, t, e).
word(astoria, a, s, t, o, r, i, a).
word(baratto, b, a, r, a, t, t, o).
word(cobalto, c, o, b, a, l, t, o).
word(pistola, p, i, s, t, o, l, a).
word(statale, s, t, a, t, a, l, e). 

crossword(V1, V2, V3, H1, H2, H3) :-
    word(V1, _, X, _, P, _, S, _),
    word(V2, _, Y, _, Q, _, T, _),
    word(V3, _, Z, _, R, _, U, _),

    word(H1, _, X, _, Y, _, Z, _),
    word(H2, _, P, _, Q, _, R, _),
    word(H3, _, S, _, T, _, U, _).
