:- use_module(syntax).

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

type_to_term(V, V) :-
    var(V).
type_to_term(ty(T), T) :-
    atom(T).
type_to_term(Ctor $ Arg, Functor) :-
    type_to_term(Ctor, TCtor),
    TCtor =.. [Atom | LeftArgs],
    type_to_term(Arg, TArg),
    append(LeftArgs, [TArg], TArgs),
    Functor =.. [Atom | TArgs].

term_to_type(Var, tyvar(VarName)) :-
    var(Var),
    term_to_atom(Var, VarName).
term_to_type(Atom, ty(Atom)) :-
    atom(Atom).
term_to_type(Functor, Ap) :-
    Functor =.. Components,
    maplist(term_to_type, Components, Types),
    syntax:list_to_application(Types, Ap).

constraint_to_goal(Class @ Type, Goal) :-
    type_to_term(Type, Arg),
    Goal =.. [Class, Arg].

instance_to_clause(instance(Constraints, Head, _), Clause) :-
    make_nonground([Head | Constraints], [NGHead | NGConstraints]),
    constraint_to_goal(NGHead, ClauseHead),
    maplist(constraint_to_goal, NGConstraints, Goals),
    foldl([G1, G2, B] >> (B = (G2, G1)), Goals, true, Body),
    Clause = (ClauseHead :- Body).

make_instance_nonground(instance(Constraints, Head, Methods), instance(NGConstraints, NGHead, Methods)) :-
    make_nonground([Head | Constraints], [NGHead | NGConstraints]).

has_instance(Instances, Candidate, Instance) :-
    maplist(make_instance_nonground, Instances, NGInstances),
    make_nonground(Candidate, NGCandidate),
    has_instance_ng(NGInstances, NGCandidate, Instance).

has_instance_ng(Instances, Candidate, Instance) :-
    Instance = instance(Constraints, Head, _),
    member(Instance, Instances),
    Head = Candidate,
    maplist([C] >> (has_instance_ng(Instances, C, _)), Constraints).
