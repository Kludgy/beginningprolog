:- module(area_code, [test/1, area_code/2]).

test(Code415) :-
    area_code(913, 9138236500),
    area_code(Code415, 4158268737).


%% area_code(?AreaCode, +Number) is det
%
% AreaCode is the 3-digit prefix of Number.
% area_code(211, Number) doesn't make much sense.
%
area_code(AreaCode, Number) :-
    number_codes(Number, NumberCodes),      % +Number, -NumberCodes
    length(AreaCodeCodes, 3),               % -AreaCodeCodes, +(3)                 % AreaCodeCodes partial (3 new member vars)
    append(AreaCodeCodes, _, NumberCodes),  % -AreaCodeCodes, -(_), +NumberCodes   %           ... full
    number_codes(AreaCode, AreaCodeCodes).  % -AreaCode, +AreaCodeCodes
