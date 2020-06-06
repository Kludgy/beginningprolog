:- module(dcg_3, []).
:- set_prolog_flag(double_quotes, codes).

anything --> [].
anything --> [_], anything.

nothing --> [].

% use a dcg to convert a sparse sequence like [0,0,0,0,0,0,7,4,3,0,0,0,0,0,0,0,8,9,14,0,0,0,0....] to
% [zero(6), 7,4,3, zero(7),8,9,14,...]

sparse_sequence([]) -->
    nothing.

sparse_sequence([Number|Sparse]) -->
    {
        number(Number)
    },
    [Number],
    sparse_sequence(Sparse).

sparse_sequence([zero(N)|Sparse]) -->
    zero_run(N),
    sparse_sequence(Sparse).

zero_run(0) -->
    nothing.

zero_run(N) -->
    {
        integer(N), % beware, this only satisfies ground terms
        N > 0
    },
    [0],
    {
        M is N - 1
    },
    zero_run(M).

% phrase(sparse_sequence([zero(6),7,4,3,zero(7),8,9,14]), [0,0,0,0,0,0,7,4,3,0,0,0,0,0,0,0,8,9,14]).
%
% This works:
% ?- phrase(sparse_sequence([zero(6),7,4,3,zero(7),8,9,14]), X).
% X = [0,0,0,0,0,0,7,4,3,0,0,0,0,0,0,0,8,9,14]) ;
% false.
%
% The opposite modes doen't work because:
% - zero_run(N) only works intelligently when N is ground.
% - Current implementation always fails to succeed on zero_run(N) when N is not ground.
% This happens to be because integer(N) yields no solution.
