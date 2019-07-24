% From: https://github.com/triffon/lcpt-2017-18/blob/master/typeinf.pro

:- op(160, fx, ⊢).
:- op(150, xfx, ⊢).
:- op(140, xfx, :).
:- op(140, xfx, to).
:- op(140, xfx, ⊑).
:- op(120, xfy, =>).
:- op(100, yfx, @).

% Γ ⊢ X : T :-
%     member(X : T, Γ).

% Γ ⊢ M @ N : Σ :-
%     Γ ⊢ M : (Ρ ⇒ Σ),
%     Γ ⊢ N : Ρ.

% Γ ⊢ λ(X,M) : (Ρ ⇒ Σ) :-
%     [ X : Ρ | Γ ] ⊢ M : Σ.

% _ ⊢ true : bool.
% _ ⊢ false : bool.

% ⊢ M : Τ :-
%     [] ⊢ M : Τ.

evar(x).
evar(y).
evar(z).

tvar(a).
tvar(b).
tvar(c).

prim(bool).

expr(X) :- evar(X).
expr(λ(X, E)) :- evar(X), expr(E).
expr(X @ Y) :- expr(X), expr(Y).
expr(let(X, E1, E2)) :- evar(X), expr(E1), expr(E2).

mono(A) :- tvar(A).
mono(T1 => T2) :- mono(T1), mono(T2).

ty(T) :- mono(T).
ty(∀(A, T)) :- tvar(A), ty(T).

free(V, [V]) :- tvar(V).
free(P, []) :- prim(P).

free(T1 => T2, F) :-
    free(T1, F1), free(T2, F2), union(F1, F2, F).

free([], []).
free([ _ : T | Ctx ], F) :-
    free(T, FT), free(Ctx, FC), union(FT, FC, F).

free(∀(Y, T), F) :-
    free(T, FT),
    subtract(FT, [Y], F).

free(Ctx ⊢ _ : T, F) :-
    free(T, FT),
    free(Ctx, FC),
    subtract(FT, FC, F).

substitute(_ to _, P, P) :- prim(P).
substitute(X to Y, X, Y) :- tvar(X).
substitute(X to Y, T1 => T2, S1 => S2) :-
    substitute(X to Y, T1, S1),
    substitute(X to Y, T2, S2).
substitute(X to _, ∀(X, T), ∀(X, T)).
substitute(X to Y, ∀(Z, T), ∀(Z, S)) :-
    substitute(X to Y, T, S).

substitution([], T, T).
substitution([ Subst | Rest ], T, S) :-
    substitute(Subst, U, S),
    substitution(Rest, T, U).
