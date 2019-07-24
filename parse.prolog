:- module(parse, []).

:- use_module(library(dcg/high_order)).

:- use_module(lex).

:- op(100, yfx, $).
:- op(100, xfx, @).

% TODO: maybe use this everywhere instead of `[some_atom]`?
token(T) --> [T].

tyvar(tyvar(V)) -->
    [varident(V)].

constraint(Name @ Arg) -->
    [ident(Name)], type(Arg).

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

class(class(Constraints, Name, Var)) -->
    % For now, we don't allow actual method declarations, since we haven't (yet)
    % implemented a lexer/parser for the full type language.
    % (We could possibly 'cop out' by parsing everything after the `::` up to the `;` as a single atom.)
    % We also only allow the definition of single-parameter classes.
    [class],
    optional(context(Constraints), { Constraints = [] }),
    [ident(Name), varident(Var), where, '{', '}'].

context([Constraint]) -->
    constraint(Constraint),
    ['=>'].
context(Constraints) -->
    sequence(token('('), constraint, token(','), token(')'), Constraints),
    ['=>'].
