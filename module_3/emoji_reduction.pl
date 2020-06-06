:- module(emoji_reduction, [test/0, emoticon_emoji/2]).

test :-
    emoticon_emoji("hello there 8cD, I'm Annie", "hello there 😀, I'm Annie"),
    emoticon_emoji("wow, that's crazy X-P =8cO", "wow, that's crazy 🤪 😮").

emoticon_emoji_exact("8cD", "😀").
emoticon_emoji_exact("=8cO","😮").
emoticon_emoji_exact(";-)", "😉").
emoticon_emoji_exact("X-P", "🤪").

%% emoticon_emoji(+In, -Out) is det
emoticon_emoji(In, Out) :-
    string_codes(In, InCodes),
    emoticon_emoji_as_codes(InCodes, OutCodes),
    string_codes(Out, OutCodes).

%% emoticon_emoji_as_codes(+In, -Out) is det
emoticon_emoji_as_codes([], []) :-
    !.
emoticon_emoji_as_codes(In, Out) :-
    append(Emoticon, RestIn, In),
    emoticon_emoji_exact_as_codes(Emoticon, Emoji),
    emoticon_emoji_as_codes(RestIn, RestOut), 
    append(Emoji, RestOut, Out),
    !.
emoticon_emoji_as_codes([I|In], [I|Out]) :-
    emoticon_emoji_as_codes(In, Out).
    
emoticon_emoji_exact_as_codes(Emoticon, Emoji) :-
    string_codes(EmoticonString, Emoticon),
    emoticon_emoji_exact(EmoticonString, EmojiString),
    string_codes(EmojiString, Emoji).
