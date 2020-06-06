:- module(seventh, [test/0, seventh/2]).

test :-
    seventh([a,b,c,d,e,f,g,h,i], g),
    \+ seventh([a,b,c], X),
    seventh([_,_,_,_,_,_,hello|_],  hello).

%% seventh(?List, ?X) is semidet
%
% seventh(List, X):- nth1(7, List, X) ;
% seventh([_,_,_,_,_,_,X|_], X).
%
seventh(List, X) :- nth1_(7, List, X).
nth1_(1, [X|_], X) :-
    !.
nth1_(N, [_|List], X) :-
    succ(PrevN, N),
    nth1_(PrevN, List, X).
