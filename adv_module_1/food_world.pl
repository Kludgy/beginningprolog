:- module(food_world, [strategy/1, plan/6, lost_without_money/3]).

% connects(LocationA, LocationB, one_way)
% connects(LocationA, LocationB, two_way)
%
% Describes connected locations on the world map.
% Actors may only move between locations in
% connects(A,B).

connects(home, home_parkade, two_way).
connects(home, creekside_dock, two_way).
connects(home_parkade, second_ave, two_way).
connects(second_ave, granville_island, two_way).
connects(granville_island, market, two_way).
connects(granville_island, alder_bay_dock, two_way).
connects(alder_bay_dock, false_creek, two_way).
connects(false_creek, creekside_dock, two_way).

connects(A,B) :-
    connects(A,B,_) ; 
    connects(B,A,two_way).

% location_takes_gives(Loc, WhatYouPay, WhatYouGet).
%
% Describes a bartering service.

location_takes_gives(market, money, food).

% lost_without_money/1 is provided as an example to test all
% features of the story. darren may only eat (eating is not encoded) when
% the final state of being home while carrying food is achieved.
lost_without_money(Plan, MinLength, MaxLength) :-
    I = [actor_location(darren, alder_bay_dock), item_location(money, home)],
    F = [actor_location(darren, home), actor_carrying(darren, food)],
    plan(iddfs, I, F, Plan, MinLength, MaxLength).

% Actions yield new situations. All action//1 predicate clauses define the
% database of possible actions.
%
% DCG clauses are borrowed as a convenient AST to weave situation states
% through the terms.
%
% DCG forms of set operations are defined later on. They all have the form
% operation(Args..., S1-V, S2-V), which may be thought of as S2 = operation(S1).
%
% V is a holder for planners to track which situations have already been
% visited.

action(take(Actor, Item, Location)) -->
    elem(actor_location(Actor, Location)),
    elem(item_location(Item, Location)),
    del(item_location(Item, Location)),
    add(actor_carrying(Actor, Item)).

action(buy(Actor, Item, Location)) -->
    elem(actor_location(Actor, Location)),
    elem(actor_carrying(Actor, Offer)),
    { location_takes_gives(Location, Offer, Item) },
    del(actor_carrying(Actor, Offer)),
    add(actor_carrying(Actor, Item)).

action(move(Actor, From, To)) -->
    elem(actor_location(Actor, From)),
    { connects(From, To) },
    del(actor_location(Actor, From)),
    add(actor_location(Actor, To)).

% X element-of S, where S-Visited
elem(X, S-V, S-V) :-
    member(X, S).
	
% S2 = S1 subtract {X}
del(X, S1-V, S2-V) :-
    ord_del_element(S1, X, S2).

% S2 = {X} union S1
add(X, S1-V, S2-V) :-
    ord_add_element(S1, X, S2).
    
% plan(iddfs, +Init, +Goal, ?Plan, +MinLength, +MaxLength)
%
% Plan is a list of actions anywhere from MinLength to MaxLength that takes
% the situation from Init to Goal. Min & max length give us a way to
% resume and delimit computation where solutions are very sparse, or perhaps
% may never appear again.
% 
% iddfs in the first argument means that a default Iterative Depth-First
% Search strategy is employed.

plan(iddfs, Init, Goal, Plan, MinPlanLength, MaxPlanLength) :-
    sort(Init, OrdInit),							        % Lists are sorted for set equivalence.
    sort(Goal, OrdGoal),
    between(MinPlanLength, MaxPlanLength, PlanLength),      % Iterative deepening is achieved by lengthening the plan on failure. Reasonable for smallish plans?
    length(Plan, PlanLength),
    full_search(Plan, OrdInit, SuperGoal),
    reached_goal(OrdGoal, SuperGoal).          
    
% We consider the goal situation in a plan to be achieved when it can be
% identified as a subset of the current situation. The presence of
% additional terms do not negate the accomplishment.
reached_goal(Goal, Situation) :-
    ord_subset(Goal, Situation).

% full_search(?Plan, +Init, ?Goal)
%
% Enumeration of actions leading from Init.
% Goal may be partially instantiated.
full_search(Plan, Init, Goal) :-
    full_search_(Plan, Init-[Init], Goal-_).

full_search_([], S, S).
full_search_([Action|Plan]) -->
    action(Action),
    unvisited,		                % Prevent cycles
    full_search_(Plan).

% True if Situation is not in Visited and NewVisited = {Situation} union Visited.
unvisited(Situation-Visited, Situation-NewVisited) :-
    \+ member(Situation, Visited),
    ord_add_element(Visited, Situation, NewVisited).
