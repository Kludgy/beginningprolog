:- module(color_math, []).

:- op(700, xfx, color_is).

% color_is(Color, Term)
color_is(rgb(R,G,B), rgb(R,G,B)).
color_is(hsv(H,S,V), hsv(H,S,V)).
color_is(rgb(R,G,B), rgb(E)) :-
    color_is(hsv(H,S,V), E),
    hsv_rgb(H,S,V,R,G,B).
color_is(rgb(R,G,B), vec3(R,G,B)).
color_is(hsv(H,S,V), vec3(H,S,V)).
color_is(R, BinaryFunc) :-
    color_eval(R, BinaryFunc).
color_eval(Result, BinaryFunc) :-
    BinaryFunc =.. [Op, Term1, Term2],
    color_is(S, Term1),
    color_is(T, Term2),
    S =.. [SType|_],
    T =.. [TType|_],
    (SType \= TType -> 
        atomic_list_concat(['type mismatch: ', SType, ', ', TType], Atom),
        atom_string(Atom, Msg),
        throw(Msg)
    ), 
    vec3_promote(vec3(A,B,C), S), 
    vec3_promote(vec3(D,E,F), T),
    PromotedFuncAD =.. [Op, A, D],
    PromotedFuncBE =.. [Op, B, E],
    PromotedFuncCF =.. [Op, C, F],
    G is PromotedFuncAD,
    H is PromotedFuncBE,
    I is PromotedFuncCF,
    vec3_promote(vec3(G,H,I), Result).

vec3_promote(vec3(A,B,C), rgb(A,B,C)).
vec3_promote(vec3(A,B,C), hsv(A,B,C)).

%% hsv_rgb(+H, +S, +V, -R, -G, -B) is semidet
hsv_rgb(H, S, V, R, G, B) :-
    saturate1(H, H1),
    saturate1(S, S1),
    saturate1(V, V1),
    hue_rgb(H1, R1, G1, B1),
    sat_rgb(S1, R1, G1, B1, R2, G2, B2),
    val_rgb(V1, R2, G2, B2, R, G, B).

%% saturate1(+X0, -X) is semidet
saturate1(X0, X) :-
    X is max(min(X0, 1), 0).

%% val_rgb(+V, +R0, +G0, +B0, -R, -G, -B) is semidet
val_rgb(V, R0, G0, B0, R, G, B) :-
    lerp(V, 0, R0, R),
    lerp(V, 0, G0, G),
    lerp(V, 0, B0, B).

%% sat_rgb(+S, +R0, +G0, +B0, -R, -G, -B) is semidet
sat_rgb(S, R0, G0, B0, R, G, B) :-
    lerp(S, 1, R0, R),
    lerp(S, 1, G0, G),
    lerp(S, 1, B0, B).

%% lerp(+T, +X0, +X1, -X) is semidet
lerp(T, X0, X1, X) :-
    X is T * (X1 - X0) + X0.

%% hue_rgb(+H, -R, -G, -B) is det
hue_rgb(H, R, G, B) :-
    H6 is 6*H,
    hue6_rgb(H6, R, G, B).

%% hue6_rgb(+H, -R, -G, -B) is semidet
hue6_rgb(H, 1, G, 0) :- H >= 0, H  < 1, G is   (H-0).
hue6_rgb(H, R, 1, 0) :- H >= 1, H  < 2, R is 1-(H-1).
hue6_rgb(H, 0, 1, B) :- H >= 2, H  < 3, B is   (H-2).
hue6_rgb(H, 0, G, 1) :- H >= 3, H  < 4, G is 1-(H-3).
hue6_rgb(H, R, 0, 1) :- H >= 4, H  < 5, R is   (H-4).
hue6_rgb(H, 1, 0, B) :- H >= 5, H =< 6, B is 1-(H-5).
