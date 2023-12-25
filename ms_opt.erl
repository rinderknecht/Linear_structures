-module(ms_opt).
-export([sort/1]).

sort([]) -> [];
sort( L) -> all(duo(L)).

duo([I,J|L]) when I > J -> [[J,I]|duo(L)];
duo([I,J|L])            -> [[I,J]|duo(L)];
duo(      L)            -> [L].

all([P]) -> P;
all(  L) -> all(level(L)).

level([P,Q|S]) -> [merge(P,Q)|level(S)];
level(      L) -> L.

merge(   [],      Q)            -> Q;
merge(    P,     [])            -> P;
merge(P=[I|_],[J|Q]) when I > J -> [J|merge(P,Q)];
merge(  [I|P],    Q)            -> [I|merge(P,Q)].
