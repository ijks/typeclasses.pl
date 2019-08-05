:- module(typeclasses,
    [ has_instance/2
    ]).

:- use_module(syntax).

%! assign(Term, Mappings, Assigned)
%
% True if `Assigned` is a Haskell term such that, for each assignment `a - X`
% in `Mappings`, % every occurrence of `tyvar(a)` in `Term` is replaced with `X`.
assign(tyvar(V), Mappings, R) :-
    ( member(V - X, Mappings) ->
        R = X
    ;
        R = tyvar(V)
    ).
assign(ty(X), _, ty(X)).
assign(Ctor $ Type, Mappings, ACtor $ AType) :-
    assign(Ctor, Mappings, ACtor),
    assign(Type, Mappings, AType).
assign(Class @ Type, Mappings, Class @ AType) :-
    assign(Type, Mappings, AType).
assign(instance(Ctx, Head, Methods), Mappings, instance(ACtx, AHead, Methods)) :-
    assign(Ctx, Mappings, ACtx),
    assign(Head, Mappings, AHead).
assign(List, Mappings, AList) :-
    maplist({Mappings}/[X, Y] >> assign(X, Mappings, Y), List, AList).

%! make_nonground(Term, NonGround).
%
% Replace each type variable in a (Haskell) term, or list of terms, with a fresh Prolog variable.
make_nonground(Term, NonGround) :-
    tyvars(Term, Vars),
    maplist([V, M] >> (M = V - _), Vars, Mappings),
    assign(Term, Mappings, NonGround).

%! has_instance(Instances, Candidate, Instance)
%
% True if, given a list of instance clauses `Instances`, the constraint `Candidate`
% holds.
%
% TODO: provide some kind of witness/ID of the satisfying instance - something like a dictionary, likely.
has_instance(Instances, Candidate) :-
    maplist(make_nonground, Instances, NGInstances),
    make_nonground(Candidate, NGCandidate),
    has_instance_ng(NGInstances, NGCandidate).

%! has_instance_ng(Instances, Candidate, Instance)
%
% Helper predicate for `has_instance`, where type variables are assumed to
% be replaced with Prolog variables.
has_instance_ng(Instances, Candidate) :-
    Instance = instance(Constraints, Head, _),
    member(Instance, Instances),
    Head = Candidate,
    maplist([C] >> (has_instance_ng(Instances, C)), Constraints).
