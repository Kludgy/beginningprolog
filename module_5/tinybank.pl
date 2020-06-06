:- module(tinybank, [end_month/2]).

% At TinyBank accounts with a balance of under $1000 are charged $10 a month service charge.
% Accounts with a balance of $1000-1999 are charged $5 a month service charge.
% Accounts with more are not charged.
% Accounts with a balance of $1500 or more receive a 1% per month interest.
% Accounts with a balance of more than $10000 receive a 2% per month interest.
% The balance for interest is calculated after the service charge is deducted.

% Write a predicate that calculates the new balance after end of month calculations

end_month(Current, New) :-
    service_charge(Current, ServiceCharge),
    Subtotal is Current - ServiceCharge,
    interest_pct(Subtotal, InterestPct),
    New is Subtotal + Subtotal * (InterestPct / 100).

service_charge(Bal, Charge) :- Bal < 1_000, !, Charge = 10.
service_charge(Bal, Charge) :- Bal < 2_000, !, Charge =  5.
service_charge(  _, Charge) :-                 Charge =  0.

interest_pct(Bal, Pct) :- Bal >= 10_000, !, Pct = 2.
interest_pct(Bal, Pct) :- Bal >=  1_500, !, Pct = 1.
interest_pct(  _, Pct) :-                   Pct = 0.
