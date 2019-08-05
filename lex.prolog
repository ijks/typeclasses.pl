:- module(lex,
    [ token//1
    , tokens//1
    , lex_file/2
    ]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

left_paren('(') --> "(".
right_paren(')') --> ")".
left_curly('{') --> "{".
right_curly('}') --> "}".
thin_arrow('->') --> "->".
fat_arrow('=>') --> "=>".
comma(',') --> ",".
double_colon('::') --> "::".
semicolon(';') --> ";".
equals('=') --> "=".

punctuation(P) -->
    left_paren(P) | right_paren(P)
    | left_curly(P) | right_curly(P)
    | thin_arrow(P) | fat_arrow(P)
    | comma(P) | semicolon(P)
    | double_colon(P) | equals(P).

upper(C) --> [C], { code_type(C, upper) }.
lower(C) --> [C], { code_type(C, lower) }.
ident_char(C) -->
    [C],
    { code_type(C, alnum); [C] = `'` ; [C] = `_` }.

% TODO: qualified identifiers
% TODO: rename to make clear this is a _constructor_ ident
ident(ident(Ident)) -->
    upper(C),
    sequence(ident_char, Rest),
    { atom_codes(Ident, [C | Rest]) }.

varident(varident(Ident)) -->
    lower(C),
    sequence(ident_char, Rest),
    { atom_codes(Ident, [C | Rest]) }.

keyword_or_varident(T) -->
    varident(varident(V)),
    { member(V, [class, instance, where])
        -> T = V
        ; T = varident(V)
    }.

token(T) -->
    punctuation(T) | keyword_or_varident(T) | ident(T).

tokens(Toks) -->
    sequence(token, blanks, Toks).

lex_file(File, Tokens) :-
    phrase_from_file(tokens(Tokens), File).
