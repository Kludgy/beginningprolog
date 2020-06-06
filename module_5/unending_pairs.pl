:- module(unending_pairs, [unending_pairs/2]).

unending_pairs(List, X-Y) :-
    left_rotate(List, YList),
    zip(List, YList, XYList),
    ring_member(X-Y, XYList).

ring_member(_, []) :- !, fail.
ring_member(X, List) :-
    repeat,
    member(X, List).

left_rotate([], []).
left_rotate([X|Xs], O) :-
    append(Xs, [X], O).

zip([], [], []).
zip([X|Xs], [Y|Ys], [X-Y|O]) :-
    zip(Xs, Ys, O).
