:- use_module(library(lists)).

sublist([], _).
sublist([A|XS], [A|YS]) :- sublist(XS, YS).
sublist(L, [_ | XS]) :- sublist(L, XS).

left(X, Y, Street) :- sublist([X, Y], Street).

right(X, Y, Street) :- sublist([Y, X], Street).

zebra(X, Street) :-
    length(Street, 3),

    % member(house(red, _, _), Street),
    % member(house(blue, _, _), Street),
    % member(house(green, _, _), Street),

    % member(house(_, english, _), Street),
    % member(house(_, spanish, _), Street),
    % member(house(_, japanese, _), Street),

    % member(house(_, _, jaguar), Street),
    % member(house(_, _, snail), Street),
    % member(house(_, _, zebra), Street),
    
    member(house(red, english, _), Street),
    member(house(_, spanish, jaguar), Street),

    right(house(_, japanese, _), house(_, _, snail), Street),
    left(house(_, _, snail), house(blue, _, _), Street),

    member(house(_, X, zebra), Street).
