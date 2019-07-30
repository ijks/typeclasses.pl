:- use_module(syntax).

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

%! no_unbound_tyvars(Goals, Head).
%
% True if all variables in `Goals` are bound in `Head`, i.e. all occur in `Head`.
no_unbound_tyvars(Goals, Head) :-
    tyvars(Goals, GVars),
    tyvars(Head, HVars),
    subset(GVars, HVars).

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
% holds, with `Instance` as a witness.
%
% TODO: better way of indicating the witness instance - something like a dictionary, likely.
has_instance(Instances, Candidate, Instance) :-
    maplist(make_nonground, Instances, NGInstances),
    make_nonground(Candidate, NGCandidate),
    has_instance_ng(NGInstances, NGCandidate, Instance).

%! has_instance_ng(Instances, Candidate, Instance)
%
% Helper predicate for `has_instance_ng`, where type variables are assumed to
% be replaced with Prolog variables.
has_instance_ng(Instances, Candidate, Instance) :-
    Instance = instance(Constraints, Head, _),
    member(Instance, Instances),
    Head = Candidate,
    maplist([C] >> (has_instance_ng(Instances, C, _)), Constraints).
