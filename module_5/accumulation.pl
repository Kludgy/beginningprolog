:- module(accumulation, [unzip/2]).

% n_ns([1,2,3],[1,2,2,3,3,3]).

unzip(List, Out) :-
    unzip(List, L-Out, L-[]).

unzip([], L, L).
unzip([Elem-Count|In], L-OutPrefix, L-OutSuffix) :-
    append_count(L-OutPrefix, Count, Elem, L-RestPrefix),
    unzip(In, L-RestPrefix, L-OutSuffix).

append_count(L, 0, _, L) :-
    !.

append_count(List-H1, Count, Elem, List-H2) :-
    H1 = [Elem | Hi],
    succ(PredCount, Count),
    append_count(List-Hi, PredCount, Elem, List-H2).
