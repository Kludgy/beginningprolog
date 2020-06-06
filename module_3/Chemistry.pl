:- module(chemistry, [test/0, reactants_results/2]).

test :-
    reactants_results([flubber, goo, gummy_goo], [smelly_stuff]).

react([flubber, goo], [icky_stuff]).
react([gummy_goo, icky_stuff], [smelly_stuff]).
react([sooty_stuff, ick], [icky_stuff]).

% Decided to interpret "in contact" as next
% to each other in the reactants list.
% Otherwise some of the logic would change below;
% for instance "subsequence" would become "subset"
% yielding more potential combos.

%% reactants_results(+Reactants, -Products) is nondet
reactants_results(Reactants, Products) :-
	subreact(Reactants, TransmutedReactants),
    reactants_results(TransmutedReactants, Products).
reactants_results(Reactants, Products) :-
	subreact(Reactants, Products).

% With the "in contact" constraint, we want to be
% able to react to contact on either side.
react_bidir(X, Y) :-
    reverse(X, Z), 
    react(Z, Y).
react_bidir(X, Y) :-
    react(X, Y).

subreact(Reactants, Products) :-
    subsequence(Reactants, SomeReactants, Rest),
    react_bidir(SomeReactants, SomeProducts),
    append(Rest, SomeProducts, Products).

%% subsequence(+Seq, ?Sub, ?Rest) is nondet
%% subsequence(?Seq, ?Sub, +Rest) is nondet
%
% Sub is a subsequence of Seq and Rest is the remainder.
%
subsequence([X|Seq], Sub, [X|Rest]) :-
    subsequence(Seq, Sub, Rest).
subsequence([X|Seq], [X|Sub], Rest) :-
    prefix(Seq, Sub, Rest).

%% prefix(?List, ?Prefix, ?Rest) is nondet
%
% Prefix of List and Rest is the remainder.
%
prefix(List, [], List).
prefix([X|List], [X|Prefix], Rest) :-
    prefix(List, Prefix, Rest).
