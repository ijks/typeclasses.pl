hanoi([Disc], A, _, B, [move(Disc, A, B)]).
hanoi([Bottom | Rest], A, Swap, B, Moves) :-
    hanoi(Rest, A, B, Swap, M1),
    hanoi([Bottom], A, _, B, M2),
    hanoi(Rest, Swap, A, B, M3),
    append([M1, M2, M3], Moves).