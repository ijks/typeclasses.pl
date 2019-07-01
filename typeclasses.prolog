
% class Functor a where ...

% instance Functor Identity where ..
% instance([], 'Functor'('Identity')).
% ==>
% 'Functor'('Identity').

% instance (Functor f, Functor g) => Functor (Compose f g) where ...
% instance(['Functor'(tyvar(f)), 'Functor'(tyvar(g))], 'Functor'('Compose'(tyvar(f), tyvar(g)))).
% ==>
% 'Functor'('Compose'(F, G)) :- 'Functor'(F), 'Functor'(G).

% class Functor f => Applicative f where ...
% ==>
% 'Functor'(F) :- 'Applicative'(F).
% ^ how do we prevent Applicative(F) instances when there's no Functor(F) instance?
% Is that within the scope of the system? 

% What about coherence?

:- op(100, xfx, $).
:- op(100, xfx, @).

constraint_arg(tyvar(_)).
% FIXME: the constructor may also be a variable, e.g. `instance Foo (f a)`
constraint_arg(Ctor $ Args) :-
    atom(Ctor),
    maplist([A] >> (A = tyvar(_)), Args).

constraint(Name @ Arg) :-
    atom(Name),
    constraint_arg(Arg).

instance_decl(Goals, Head, _Dict) :-
    maplist(constraint, Goals),
    constraint(Head).

tyvars(tyvar(V), [V]).
tyvars(_ @ Arg, Vars) :-
    tyvars(Arg, Vars).
tyvars(_ $ Args, Vars) :-
    tyvars(Args, Vars).
tyvars(List, Vars) :-
    maplist(tyvars, List, Subvars),
    foldl(union, Subvars, [], Vars).

no_unbound_tyvars(Goals, Head) :-
    tyvars(Goals, GVars),
    tyvars(Head, HVars),
    subset(GVars, HVars).

substitute_vars(tyvar(V), Mapping, Value) :-
    member(V-Value, Mapping).
substitute_vars(Ctor $ Args, Mapping, Ctor $ MArgs) :-
    substitute_vars(Args, Mapping, MArgs).
substitute_vars(Ctor @ Args, Mapping, Ctor @ MArgs) :-
    substitute_vars(Args, Mapping, MArgs).
% We could have written the list clause with maplist, but that seemed to prevent identically-named variables to unify.
substitute_vars([], _, []).
substitute_vars([X | XS], Mapping, [M | MS]) :-
    substitute_vars(X, Mapping, M),
    substitute_vars(XS, Mapping, MS).

make_nonground(Term, NonGround) :-
    tyvars(Term, Vars),
    maplist([TyVar, Mapping] >> (Mapping = TyVar - _), Vars, Mappings),
    substitute_vars(Term, Mappings, NonGround).
