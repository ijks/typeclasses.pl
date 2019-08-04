:- module(syntax,
    [ type/1
    , constraint/1
    , instance/1
    , class/1

    , tyvars/2
    , list_to_application/2
    , application_to_list/2

    , op(100, yfx, $)
    , op(100, xfx, @)
    ]).

type(ty(A)) :-
    atom(A).
type(tyvar(A)) :-
    atom(A).
type(Ctor $ Arg) :-
    type(Ctor),
    type(Arg).

constraint(Name @ Type) :-
    atom(Name),
    type(Type).

% TODO: when the class has superclasses, backreferences to the prerequisite instances
instance(instance(Constraints, Head, _Methods)) :-
    maplist(constraint, Constraints),
    constraint(Head).

class(class(Constraints, Head, _Methods)) :-
    maplist(constraint, Constraints),
    constraint(Head).

%! tyvars(Term, Vars).
%
% True if `Vars` is the set of type variables ocurring in `Term`.
tyvars(ty(_), []).
tyvars(tyvar(V), [V]).
tyvars(_ @ Arg, Vars) :-
    tyvars(Arg, Vars).
tyvars(Ctor $ Arg, Vars) :-
    tyvars(Ctor, CVars),
    tyvars(Arg, AVars),
    union(CVars, AVars, Vars).
tyvars(instance(Constraints, Head, _), Vars) :-
    tyvars(Constraints, CVars),
    tyvars(Head, HVars),
    union(CVars, HVars, Vars).
tyvars(class(Constraints, Head, _), Vars) :-
    tyvars(Constraints, CVars),
    tyvars(Head, HVars),
    union(CVars, HVars, Vars).
tyvars(List, Vars) :-
    maplist(tyvars, List, Subvars),
    foldl(union, Subvars, [], Vars).

%! list_to_application(List, Type)
%
% Convert between a list of types to a left-associative application of those types.
% E.g. `[a, b, c]` becomes `(a $ b) $ c`.
list_to_application([T | Ts], Ap) :-
    foldl([TX, TY, TY $ TX] >> true, Ts, T, Ap).

%! application_to_list(Type, List)
%
% The inverse of `list_to_application`, since that predicate isn't perfectly reversible.
application_to_list(ty(X), [ty(X)]).
application_to_list(tyvar(X), [tyvar(X)]).
application_to_list(A $ B, L) :-
    application_to_list(A, Left),
    append(Left, [B], L).
