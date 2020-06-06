:- module(dcg_1_3, []).
:- set_prolog_flag(double_quotes, codes).

beep_boop --> 
    anything, 
    beep(Suffix),
    {
        string_codes(S, Suffix),
        format('beep ~w~n', [S])
    },
    anything, 
    boop(Suffix), 
    {
        format('boop ~w~n', [S])
    },
    anything.

beep(X) -->
    "beep",
    suffix(X).

boop(X) -->
    "boop",
    suffix(X).

suffix([H|T]) -->
      [H],  % The magic - we grab the digit here
      {
          code_type(H, digit)
      },
      suffix(T).
suffix([]) --> []. % must be 2nd suffix clause, or the digits wind up in anything
% At bottom for efficiency. At the top, would match beep first
anything --> [].
anything --> [_], anything.
% A subtlety here.  "foo 7 beep1 bar boop14 fdds" is part of the language

nothing --> [].


