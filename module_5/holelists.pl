:- module(holelists, [n_ns/2]).

% n_ns([1,2,3],[1,2,2,3,3,3]).

n_ns(List, Out) :-
    holey_n_ns(List, L-Out, L-[]).

holey_n_ns([], L, L).
holey_n_ns([X|In], L-OutPrefix, L-OutSuffix) :-
    append_count(L-OutPrefix, X, X, L-RestPrefix),
    holey_n_ns(In, L-RestPrefix, L-OutSuffix).

append_count(L, 0, _, L) :-
    !.

append_count(List-H1, Count, Elem, List-H2) :-
    H1 = [Elem | Hi],
    succ(PredCount, Count),
    append_count(List-Hi, PredCount, Elem, List-H2).
