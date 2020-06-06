:- module(triangulate, [test/0, pick_location/0, find_location/3]).

test :-
    pick_location,
    find_location(X, Y, Z).

%% find_location(?X, ?Y, ?Z) is det
find_location(X, Y, Z) :-
    march(0, 0, 0, X, Y, Z), 
    !.
    % I mean, ideally we'd just work out the algebra to compute the
    % intersection point of 4 spheres, but this is a prolog SLD
    % exercise.

%% march(+X, +Y, +Z, -XO, -YO, -ZO)
%
% Recursively marches toward location, moving half the distance along
% an axis at each step until within 1 unit of the location.
%
% What are some good ways to write this more concisely?
%
march(X, Y, Z, X, Y, Z) :-
    distance_from(X, Y, Z, D),
    D =< 1,
    !.
march(X, Y, Z, XO, YO, ZO) :- distance_from(X, Y, Z, D), HalfDist is D / 2, X1 is X + HalfDist, X1 =< 1000, distance_from(X1, Y , Z , NewD), NewD < D, march(X1, Y , Z , XO, YO, ZO).
march(X, Y, Z, XO, YO, ZO) :- distance_from(X, Y, Z, D), HalfDist is D / 2, X1 is X - HalfDist, X1 >=    0, distance_from(X1, Y , Z , NewD), NewD < D, march(X1, Y , Z , XO, YO, ZO).
march(X, Y, Z, XO, YO, ZO) :- distance_from(X, Y, Z, D), HalfDist is D / 2, Y1 is Y + HalfDist, Y1 =< 1000, distance_from(X , Y1, Z , NewD), NewD < D, march(X , Y1, Z , XO, YO, ZO).
march(X, Y, Z, XO, YO, ZO) :- distance_from(X, Y, Z, D), HalfDist is D / 2, Y1 is Y - HalfDist, Y1 >=    0, distance_from(X , Y1, Z , NewD), NewD < D, march(X , Y1, Z , XO, YO, ZO).
march(X, Y, Z, XO, YO, ZO) :- distance_from(X, Y, Z, D), HalfDist is D / 2, Z1 is Z + HalfDist, Z1 =< 1000, distance_from(X , Y , Z1, NewD), NewD < D, march(X , Y , Z1, XO, YO, ZO).
march(X, Y, Z, XO, YO, ZO) :- distance_from(X, Y, Z, D), HalfDist is D / 2, Z1 is Z - HalfDist, Z1 >=    0, distance_from(X , Y , Z1, NewD), NewD < D, march(X , Y , Z1, XO, YO, ZO).

% refering to this is no fair. 
:- dynamic location/3.

pick_location :-
    X is 1000.0 * random_float,
    Y is 1000.0 * random_float,
    Z is 1000.0 * random_float,
    retractall(location(_,_,_)),
    asserta(location(X,Y,Z)).

distance_from(X, Y, Z, Dist) :-
    location(XL, YL, ZL),
    Dist is sqrt((X-XL)*(X-XL) + 
                 (Y-YL)*(Y-YL) +
                 (Z-ZL)*(Z-ZL)).
