:- module(find_pattern, [test/0, pattern_index/3]).

test :-
    pattern_index([3, 5, 6, 1, 2, 5, 7, 1, 2, 6], [1, 2], 7).

%% pattern_index(Seq, Sub, Index)
pattern_index(Seq, Sub, 0) :-
    prefix(Sub, Seq).
pattern_index([_|Seq], Sub, Index) :-
    pattern_index(Seq, Sub, I),
    Index is I + 1.
