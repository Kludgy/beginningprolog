:- module(bettys_beautiful_you, [test/1, betty/2]).

test(Schedule) :-
    betty([mable-[1,2,3], frieda-[1,4,5,6,7], danielle-[3,4,5,6,8], martha-[8], glinda-[1,2,3], tammy-[3], penny-[5,6,7,8], arleen-[5]],
          [mable, glinda, tammy, danielle, arleen, penny, frieda, martha]),
    betty([mable-[1,2,3], tammy-[1,2,3], ethyl-[1,2,3]], Schedule).

betty(Customers, Schedule) :-
    findall(X, between(1,8,X), AllHours).
    