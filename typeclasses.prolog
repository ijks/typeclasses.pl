
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

%! tyvars(Term, Vars).
%
% True if `Vars` is the set of type variables ocurring in `Term`.
tyvars(Atom, []) :-
    atom(Atom).
tyvars(tyvar(V), [V]).
tyvars(_ @ Arg, Vars) :-
    tyvars(Arg, Vars).
tyvars(Ctor $ Arg, Vars) :-
    tyvars(Ctor, CVars),
    tyvars(Arg, AVars),
    union(CVars, AVars, Vars).
tyvars(List, Vars) :-
    maplist(tyvars, List, Subvars),
    foldl(union, Subvars, [], Vars).

%! no_unbound_tyvars(Goals, Head).
%
% True if all variables in `Goals` are bound in `Head`, i.e. all occur in `Head`.
no_unbound_tyvars(Goals, Head) :-
    tyvars(Goals, GVars),
    tyvars(Head, HVars),
    subset(GVars, HVars).

%! substitute_terms(Term, Mapping, Substituted)
%
% Given a list of mappings `Subterm - Substitution`, replace each occurrence of
% a subterm with its corresponding substituted term.
substitute_terms(Term, Mapping, Substituted) :-
    ( member(Term - S, Mapping) ->
        Substituted = S
    ; compound(Term) ->
        Term =.. [Functor | Args],
        substitute_terms_list(Args, Mapping, SArgs),
        Substituted =.. [Functor | SArgs]
    ;
        Substituted = Term
    ).

% Helper function for `substitute_terms`.
substitute_terms_list([], _, []).
substitute_terms_list([T | Ts], Mapping, [U | Us]) :-
    substitute_terms(T, Mapping, U),
    substitute_terms_list(Ts, Mapping, Us).

%! make_nonground(Term, NonGround).
%
% Replace each type variable in a (Haskell) term, or list of terms, with a fresh Prolog variable.
make_nonground(Term, NonGround) :-
    tyvars(Term, Vars),
    maplist([TyVar, Mapping] >> (Mapping = tyvar(TyVar) - _), Vars, Mappings),
    substitute_terms(Term, Mappings, NonGround).
