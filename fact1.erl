-module(fact1).
-export([fact/1]).

fact(1)        -> 1;
fact(N) when N -> 1 -> N * fact(N-1).
