:- module(recursion_exercise, []).

% dedup([a,a,a,b,b,b,c,d,d,e], [a,b,c,d,e]).

dedup([], []) :-
    !.
dedup([H|In], Out) :-
    dedup_(H, In, Out).
dedup_(H, [], [H]) :- !.
dedup_(H, [A|I], O) :-
    H = A,
    !,
    dedup_(H,I,O).
dedup_(H, [A|I], [H|O]) :-
    H \= A,
    !,
    dedup_(A, I, O).
