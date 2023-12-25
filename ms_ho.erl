-module(ms_ho).
-export([sort/2]).

sort( _,[]) -> [];
sort(Gt, L) -> all(Gt,duo(Gt,L)).

duo(Gt,[I,J|L]) -> case Gt(I,J) of
                     true  -> [[J,I]|duo(Gt,L)];
                     false -> [[I,J]|duo(Gt,L)]
                   end;
duo( _,      L) -> [L].

all( _,[P]) -> P;
all(Gt,  L) -> all(Gt,level(Gt,L)).

level(Gt,[P,Q|S]) -> [merge(Gt,P,Q)|level(Gt,S)];
level( _,      L) -> L.

merge( _,     [],      Q) -> Q;
merge( _,      P,     []) -> P;
merge(Gt,P=[I|R],Q=[J|S]) -> case Gt(I,J) of
                               true  -> [J|merge(Gt,P,S)];
                               false -> [I|merge(Gt,R,Q)]
                             end.
