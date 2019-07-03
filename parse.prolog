:- module(parse, []).

:- use_module(library(dcg/high_order)).

:- op(100, xfx, $).
:- op(100, xfx, @).

token(T) --> [T].

tyvar(tyvar(V)) -->
    [varident(V)].

constraint(Name @ Arg) -->
    [ident(Name)], type(Arg).

type(Prim) -->
    [ident(Prim)].
type(tyvar(Var)) -->
    [varident(Var)].
type(T) -->
    ['('], type(T), [')'].
type(Ctor $ [Arg | Args]) -->
    % For now, we don't allow type variables as constructors - this shouldn't
    % occur in instance/class declarations without FlexibleInstances.
    [ident(Ctor)],
    type(Arg),
    sequence(type, Args).

class(class(Constraints, Name, Var)) -->
    % For now, we don't allow actual method declarations, since we haven't (yet)
    % implemented a lexer/parser for the full type language.
    % (We could possibly 'cop out' by parsing everything after the `::` up to the `;` as a single atom.)
    [class],
    optional(context(Constraints), { Constraints = [] }),
    [ident(Name), varident(Var), where, '{', '}'].

context([Constraint]) -->
    constraint(Constraint),
    ['=>'].
context(Constraints) -->
    sequence(token('('), constraint, token(','), token(')'), Constraints),
    ['=>'].
