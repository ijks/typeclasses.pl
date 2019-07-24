:- module(parse, []).

:- use_module(library(dcg/high_order)).

:- use_module(lex).

:- op(100, yfx, $).
:- op(100, xfx, @).

% TODO: maybe use this everywhere instead of `[some_atom]`?
token(T) --> [T].

tyvar(tyvar(V)) -->
    [varident(V)].

basic_type(Prim) -->
    [ident(Prim)].
basic_type(tyvar(Var)) -->
    [varident(Var)].
basic_type('->') -->
    ['(', '->', ')'].
basic_type(T) -->
    ['('], type(T), [')'].

applied_type(Ap) -->
    basic_type(T), sequence(basic_type, Ts),
    { list_to_application([T | Ts], Ap) }.

% Helper to convert a list of types to a left-associative application of those types.
% E.g. `[a, b, c]` becomes `(a $ b) $ c`.
list_to_application([T], T).
list_to_application([T | Ts], Ap) :-
    foldl([TX, TY, R] >> (R = TY $ TX), Ts, T, Ap).

type(T) -->
    applied_type(T).
type(('->' $ Lhs) $ Rhs) -->
    applied_type(Lhs),
    ['->'],
    type(Rhs).

constraint(Name @ Arg) -->
    [ident(Name)], type(Arg).

class(class(Constraints, Name, Var, Methods)) -->
    % TODO: support multi-parameter typeclasses
    [class],
    optional(context(Constraints), { Constraints = [] }),
    [ident(Name), varident(Var), where],
    sequence(token('{'), method_decl, token(';'), token('}'), Methods).

method_decl(method(Name, Type)) -->
    [varident(Name), '::'], type(Type).

context([Constraint]) -->
    constraint(Constraint),
    ['=>'].
context(Constraints) -->
    sequence(token('('), constraint, token(','), token(')'), Constraints),
    ['=>'].

instance(instance(Constraints, Class, Type, Impls)) -->
    [instance],
    optional(context(Constraints), { Constraints = [] }),
    [ident(Class)],
    basic_type(Type),
    [where],
    sequence(token('{'), method_body, token(';'), token('}'), Impls).

method_body(body(Name, Body)) -->
    % We only allow `undefined` as a method body, because it's outside of our
    % scope to also parse Haskell's expression language.
    % One future improvement would be to just grab everything up to the `;` as a plain string.
    { Body = undefined },
    [varident(Name), '=', varident(Body)].
