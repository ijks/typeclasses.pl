:- module(syntax,
    [ type/1
    , op(100, yfx, $)
    , op(100, xfx, @)
    ]).

type(A) :-
    atom(A).
type(tyvar(A)) :-
    atom(A).
type(Ctor $ Arg) :-
    type(Ctor),
    type(Arg).

constraint(Name @ Type) :-
    atom(Name),
    type(Type).

instance(instance(Constraints, Head, _Methods)) :-
    maplist(constraint, Constraints),
    constraint(Head).

class(class(Constraints, Head, _Methods)) :-
    maplist(constraint, Constraints),
    constraint(Head).

% Helper to convert a list of types to a left-associative application of those types.
% E.g. `[a, b, c]` becomes `(a $ b) $ c`.
list_to_application([T], T).
list_to_application([T | Ts], Ap) :-
    foldl([TX, TY, R] >> (R = TY $ TX), Ts, T, Ap).
