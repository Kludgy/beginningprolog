:- module(repeatfaildemo, [read_x/0]).

read_x :-
    open('repeatfaildemo.pl', read, S),
    repeat,
    read_print(S, _),  % print every secon term #1,#3,#5,etc.
    read_skip(S, T),   % therefore skipping every other term #2,#4,...
    T = end_of_file,
    close(S),
    !.  % get rid of repeat/0 choice point

read_print(S, T) :-
    read_term(S, T, []),
    read_print_(T).

% needed to insert additional terms so we can match
% end_of_file without back-tracking, otherwise we get
% thrown back to repeat in read_x.
read_print_(end_of_file).
read_print_(T) :-
    writeln(T).

read_skip(S, T) :-
    read_term(S, T, []).
