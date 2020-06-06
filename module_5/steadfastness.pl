:- module(steadfastness, [down_from/1]).

down_from(N) :-
    N =< 0,
    !.
down_from(N) :-
    writeln(N),
    NN is N - 1,
    down_from(NN).

