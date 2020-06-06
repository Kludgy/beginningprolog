:- module(dcg_1_2, []).
:- set_prolog_flag(double_quotes, codes).

cliche -->
    thing(Thing),
    " is a ",
    type_of_thing(TypeOfThing),
    " trapped in a ",
    opposite_type_of_thing(Thing, TypeOfThing),
    " body.".

thing(Thing) -->
    {
        thing_(Thing)
    },
    Thing.
thing_("Cygwin").
thing_("Fluffy").
thing_("Bob the swimmer").

type_of_thing(TypeOfThing) -->
    {
        type_of_thing_(TypeOfThing)
    },
    TypeOfThing.
type_of_thing_("Unix OS").
type_of_thing_("dog").
type_of_thing_("fish").

opposite_type_of_thing(_, _) --> "Windows'".
opposite_type_of_thing(_, _) --> "cat's".
opposite_type_of_thing(Thing, Type) -->
    {
        \+ (Thing = "Fluffy", Type = "fish")
    },
    "human".

% phrase(cliche, X, []), format('~s~n', [X]).
% phrase(cliche, "Cygwin is a Unix OS trapped in a Windows' body.").
% phrase(cliche, "Fluffy is a dog trapped in a human body.").
% \+ phrase(cliche, "Fluffy is a fish trapped in a human body.").
% phrase(cliche, All), append("Cygwin", Rest, All).

article_phrase -->
    phrase_subject,
    " ",
    (phrase_action ; []).

phrase_subject --> 
    ((article, " ", noun) ;
    (article, " ", adjective, " ", noun)).

phrase_action -->
    (verb ; (verb, " ", adverb)).

article --> ("a" ; "an" ; "the").

adjective --> "fluffy".
adjective --> "unrepentant".
adjective --> "giant".

noun --> "book".
noun --> "car".
noun --> "fraggle".
noun -->
    {
        reverse("beastly fido", X)
    },
    X.

verb --> "runs".
verb --> "jumps".
verb --> "hissed".

adverb --> "softly".
adverb --> "aggressively".



anything_but_joe --> \+ [joe].

% ?- phrase(anything_but_joe, [sfdg], X).
% X = [sfdg].
%
% ?- phrase(anything_but_joe, [sfdg], []).
% false.
%
% ?- phrase(anything_but_joe, [sfdg], [sfdg]).
% true.
%
% ?- phrase(anything_but_joe, X, [sfdg]).
% false.
%
% Have given up the ability to generate sentences.

at_end --> \+ [_].

% Example use of at_end, only satisfies sentences
% that are [joe] and nothing more. Does not satisfy
% [joe|_].
%
% Useful way to ensure all input is parsed.
%
just_joe -->
    [joe],
    at_end.

