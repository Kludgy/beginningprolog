:- module(dcg_1_1, []).

% phrase(as, L).
% phrase(as, [a,a,a]).
% \+ phrase(as, [b,c,d]).
% phrase(as, [a,X,a]).

as --> [].
as --> [a], as.

abs --> [].
abs --> [a], bas.
bas --> [].
bas --> [b], abs.
