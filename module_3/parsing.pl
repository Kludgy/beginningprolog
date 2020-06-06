:- module(parsing, [test/0, codes_words/2]).

test :-
    codes_words("hello, world", [hello, world]),
    codes_words("@&#$^@!", []).

%% codes_words(++Codes, ?Words) is det
%
% Thoughts regarding this proof that I'm not entirely confident about:
%
% Is at least semidet because:
%  * the parser doesn't back-track, or
%  * the intersection of the domains of parse_word & parse_chaff is {0}
%
% Is det because:
%  * the union of the domains of parse_word & parse_chaff is Codes (for all valid strings)
%
codes_words(Codes, Words) :-
    string_codes(Codes, CodesAsCodes),
    codes_words_start(CodesAsCodes, WordsAsCodes),
    !,
    maplist(string_codes, WordsAsStrings, WordsAsCodes),
    maplist(atom_codes, Words, WordsAsStrings).

% The input may lead with either words or chaff.
codes_words_start(Codes, Words) :-
	codes_words_word(Codes, Words).
codes_words_start(Codes, Words) :-
	codes_words_chaff(Codes, Words).

% words and chaff alternate
codes_words_word([], []).
codes_words_word(Codes, [Word|Words]) :-
    parse_word(Codes, Word, Rest),			% word is taken
    codes_words_chaff(Rest, Words).

% chaff and words alternate
codes_words_chaff([], []).
codes_words_chaff(Codes, Words) :-
    parse_chaff(Codes, _, Rest),     		% chaff is skipped
    codes_words_word(Rest, Words).

% word is either alnum followed by word, or alnum
parse_word(In, [X|Word], Rest) :-
	parse_alnum(In, X, Y),
    parse_word(Y, Word, Rest).
parse_word(In, [X], Rest) :-
	parse_alnum(In, X, Rest).

parse_alnum([X|Rest], X, Rest) :-
    code_type(X, alnum).

% chaff is either not-alnum followed by chaff, or not-alnum
parse_chaff(In, [X|Word], Rest) :-
	parse_not_alnum(In, X, Y),
    parse_chaff(Y, Word, Rest).
parse_chaff(In, [X], Rest) :-
	parse_not_alnum(In, X, Rest).

parse_not_alnum([X|Rest], X, Rest) :-
    \+ code_type(X, alnum).
