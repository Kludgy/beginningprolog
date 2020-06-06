:- module(subterm, [test/0, my_sub_term/2]).

test :-
    my_sub_term([1,2,foo(3)], 3),
    my_sub_term([1,2,foo(3)], [2,foo(3)]),
    my_sub_term(blah(blarg(foo, mep), [3,4,5], '8cD'), mep),
    \+ my_sub_term(blah(blarg(foo, mep), [3,4,5], '8cD'), blarg(mep)).

%% my_sub_term(Term, SubTerm)
%
% Like occurs:sub_term/2 but with simpler requirements:
% Consider only numbers, atoms, complex terms, and lists.
% Ignore cyclic terms (not covered).
%
my_sub_term(Term, Term).
my_sub_term(Term, Sub) :-
    compound(Term),
    Term =.. [_|Args],
    my_sub_term_list(Args, Sub).
my_sub_term_list([Arg|_], Sub) :-
    my_sub_term(Arg, Sub).
my_sub_term_list([_|Args], Sub) :-
    my_sub_term_list(Args, Sub).
