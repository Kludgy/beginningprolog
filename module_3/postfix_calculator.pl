:- module(postfix_calculator, [test/0, postfix/2]).

test :-
    postfix([2, 3, *], 6),
    postfix([2 ,3, *, 5, +], 11),
    \+ postfix([*, *], _),
    postfix([1, 3, *, 4, 5, *, +], 23).

postfix(Operations, Result) :-
    postfix_(Operations, Result, []),
    !.
postfix_([], X, [X]).
postfix_([*|Ops], Out, [Y,X|S]) :-    Z is X * Y,    postfix_(Ops, Out, [Z|S]).
postfix_([/|Ops], Out, [Y,X|S]) :-    Z is X / Y,    postfix_(Ops, Out, [Z|S]).
postfix_([+|Ops], Out, [Y,X|S]) :-    Z is X + Y,    postfix_(Ops, Out, [Z|S]).
postfix_([-|Ops], Out, [Y,X|S]) :-    Z is X - Y,    postfix_(Ops, Out, [Z|S]).
postfix_([X|Ops], Out, S) :-
    number(X),                      % might as well be disjoint for purity
    postfix_(Ops, Out, [X|S]).
