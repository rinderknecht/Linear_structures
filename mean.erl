-module(mean).
-export([bounds/1]).

floor(X) when X < trunc(X) -> trunc(X) - 1;
floor(X)                   -> trunc(X).

log2(X) -> math:log(X)/math:log(2).

exp2(0) -> 1;
exp2(N) -> E=exp2(N div 2), (1 + N rem 2)*(E*E).

bounds(N) -> A=1.26449978034844420919131974725549848255770,
             LOG=floor(log2(N)), EXP=exp2(LOG),
             {N*LOG + (5-A)*N - 4*EXP + 2*LOG + 13/2,
              N*LOG +     5*N - 2*EXP + 2*LOG +    5}.
