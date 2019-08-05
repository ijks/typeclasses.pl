:- module(parse,
    [ parse/2

    , type//1
    , constraint//1
    , class//1
    , instance//1
    , declaration//1
    , mod//2

    , parse_file/2
    ]).

:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(syntax).
:- use_module(lex).

% TODO: maybe use this everywhere instead of `[some_atom]`?
token(T) --> [T].

tyvar(tyvar(V)) -->
    [varident(V)].

basic_type(ty(Prim)) -->
    [ident(Prim)].
basic_type(tyvar(Var)) -->
    [varident(Var)].
basic_type('->') -->
    ['(', '->', ')'].
basic_type(T) -->
    ['('], type(T), [')'].

applied_type(Ap) -->
    basic_type(T), sequence(basic_type, Ts),
    { syntax:list_to_application([T | Ts], Ap) }.

type(T) -->
    applied_type(T).
type(('->' $ Lhs) $ Rhs) -->
    applied_type(Lhs),
    ['->'],
    type(Rhs).

constraint(Name @ Arg) -->
    [ident(Name)], type(Arg).

class(class(Constraints, Name @ tyvar(Var), Methods)) -->
    % TODO: support multi-parameter typeclasses
    % TODO: default method impls
    [class],
    optional(context(Constraints), { Constraints = [] }),
    % FIXME: the `where` isn't obligatory for zero-method classes
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

instance(instance(Constraints, Class @ Type, Impls)) -->
    [instance],
    optional(context(Constraints), { Constraints = [] }),
    [ident(Class)],
    basic_type(Type),
    % FIXME: the `where` isn't obligatory for zero-method instances
    [where],
    sequence(token('{'), method_body, token(';'), token('}'), Impls).

method_body(body(Name, Body)) -->
    % We only allow `undefined` as a method body, because it's outside of our
    % scope to also parse Haskell's expression language.
    % One future improvement would be to just grab everything up to the `;` as a plain string.
    { Body = undefined },
    [varident(Name), '=', varident(Body)].

declaration(D) -->
    class(D) | instance(D).

mod(Classes, Instances) -->
    sequence(declaration, Decls),
    { partition([class(_, _, _)] >> true, Decls, Classes, Instances) }.

parse(Str, Rule) :-
    string_codes(Str, Codes),
    phrase(lex:tokens(Tokens), Codes),
    phrase(Rule, Tokens).

parse_file(File, Rule) :-
    lex_file(File, Tokens),
    phrase(Rule, Tokens).
