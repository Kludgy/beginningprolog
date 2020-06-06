:- module(train_route, [train_route/3]).

train_route(Start, End, Route) :-
    train_route(Start, End, [], Route).

train_route(End, End, _, [End]).
train_route(Start, End, Inc, [Start|Route]) :-
    udarc(Start, X),
    \+ member(X, Inc),
    train_route(X, End, [X|Inc], Route).

arc(paris, brussels).
arc(paris, lisbon).
arc(brussels, amsterdam).
arc(amsterdam, copenhagen).
arc(amsterdam, munich).
arc(munich, brussels).
arc(berlin, munich).

udarc(X,Y) :- arc(X,Y).
udarc(X,Y) :- arc(Y,X).
